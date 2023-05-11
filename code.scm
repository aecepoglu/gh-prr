#!/bin/env gxi

(defstruct reply (id msg))

(import :std/os/fd)
(import :std/event)

(def (limit lower x upper)
  (min upper (max lower x)))

(def (num->superscript n)
  (let F ((a []) (n n))
   (if (> n 0)
     (F (cons (string-ref "⁰¹²³⁴⁵⁶⁷⁸⁹" (remainder n 10)) a)
        (floor (/ n 10)))
     (list->string a))))

(def (@ o0 . k0)
  (let F ((o o0) (k k0))
   (if (null? k) o
     (with ([h . t] k)
           (F ((if (number? h) list-ref hash-ref) o h)
              t)))))

(module ReplyTpl
 (import :std/event)
 (def start "### enter your message below this line ###")
 (def mid   "### enter your message above this line ###")
 (def (end? x) (match (string-split x #\space)
                      (["###" "thread" x "###"] x)
                      (_ #f)))
                 
 (def (make id lines)
  (append
   [start "" mid]
   lines
   (list (string-append "### thread " id " ###"))))
 (def (read port)
   (let R ((a []) (ok #f))
      (let* ((e (sync port 0.5))
             (x (if e (read-line e) #!eof)))
        (cond
         ((eof-object? x)  [])
         ((equal? start x) (R [] #t))
         ((equal? mid x)   (R a #f))
         (else             (let (y (end? x))
                             (cons y (reverse a))
                             (R (if ok (cons x a) a) ok)))))))
 (export make read))

(module hash-xtra
 (export #t)
 (def (hash-refs t k)
  (if (null? k)
   t
   (hash-refs (hash-ref t (car k)) (cdr k)))))

(module gh
 (import
  :std/misc/process
  :std/text/json)
 (export #t)

 (def gql:pr "query Foo($owner: String!, $repo: String!, $pr: Int!, $endCursor:String) {
   repository(owner: $owner, name: $repo) {
      pullRequest(number: $pr) {
         number, author {login}, bodyText, baseRefName, headRefName, title, url
         participants(first:100) { nodes { login } }
         reviewDecision
         reviewThreads(first: 100, after:$endCursor) {
            nodes {
               id, isResolved, resolvedBy { login }, isOutdated,
               path, line, startLine, subjectType
               comments(first: 100) { nodes {
                  commit {oid} , databaseId
                  author { login }
                  createdAt, lastEditedAt
                  body
                  url
                  minimizedReason
                  diffHunk
               }}
            }
            pageInfo { hasNextPage, endCursor }
         }
      }
   }
}")
 (def gql:resolveThread "mutation Foo($thread: ID!) {
   resolveReviewThread(input: {threadId: $thread}) { thread { isResolved } }
}")
        ;(def gh-env [(string-append
;              "GH_CONFIG_DIR="
;              (path-normalize
;               (string-append (current-directory)
;                              (if #t "../" "")))
;              "gh-config-dir")])
 (def gh-env #f)
 (def (gh-cli args)
  ;(displayln "gh" args)
  (run-process ["gh" "--jq" "." . args] coprocess: read-json environment: gh-env))

 (def (gql args)
   (hash-ref
    (gh-cli ["api" "graphql" "--paginate" "--cache" "5m" . args])
    'data))

 (def (gh:pr-list repo json-fields: (fields ["id"]))
  (gh-cli ["pr" "list"
           "--repo" repo
           "--json" (string-join fields ",")
           "--limit" "100"]))

 (def (gh:pr-comments repo pr)
  (gh-cli ["api" "-H" "Accept: application/vnd.github+json"
                      (string-append "/repos/" repo "/pulls/" pr "/comments")]))

 (def (gh:pr-view repo pr json-fields: (fields ["id"]))
  (gh-cli ["pr" "view"
           pr
           "--repo" repo
           "--json" (string-join fields ",")]))
 (def (gh:reply repo pr parent reply)
   (gh-cli ["api" "-H" "Accept: application/vnd.github+json" "--method" "POST"
            (string-append "/repos/" repo "/pulls/" pr "/comments/"
                           ((if (number? parent) number->string identity) parent)
                           "/replies")
            "-f" (string-append "body=" reply)]))
 
 (def (gh:resolve-thread id (repo "{repo}"))
   (gql ["-F" "owner={owner}"
         "-F" (string-append "repo=" repo)
         "-F" (string-append "thread=" id)
         "-f" (string-append "query=" gql:resolveThread)]))

 (def (gh:pr-info pr (repo "{repo}"))
   (gql ["-F" "owner={owner}"
         "-F" (string-append "repo=" repo)
         "-F" (string-append "pr=" pr)
         "-f" (string-append "query=" gql:pr)])))

(module git
        (import :std/misc/process 
         :std/misc/ports :std/sugar)
 (export #t)
 ;; returns a hexadruple describing git status we're in
 ;; car has error
 ;; cdr is a pentadruple describing ... stuff :/
 (def (git:meta)
   (try
    (cons #f
      (run-process ["git" "rev-parse"
                    "HEAD" ; commit id
                    "--is-inside-work-tree" ;
                    "--show-prefix" ; cwd relative to git root
                    "--show-toplevel" ; abs path to git root
                    "--abbrev-ref" "HEAD"] ; branch name
        coprocess: read-all-as-lines))
    (catch (err)
           (displayln err)
           (cons err (make-list 5 "")))))
 (def (git:state-ok? branch commit)
   (with
     ([ok? cmt is-in _ _ brn] (git:meta))
     (if (and ok? ; return richer errors
          (equal? is-in "true")
          (or (equal? cmt commit)
              (equal? brn branch)))
       #f
       (string-append
        (if ok? "OK" "NOTOK")
        ", branch=" branch " vs " brn
        ", commit=" commit " vs " cmt
        ", is-in=" is-in
        "\r\n"))))
 (def (git:checkout commit)
   (run-process ["git" "checkout" commit] coprocess: read-all-as-lines check-status: #f)))

(module kakoune
 (import :gerbil/gambit/os
         :std/misc/ports
         :std/misc/process)
 (def (kak:p txt)
   (lambda (p)
    (display txt p)
    (close-output-port p)))

 (def (kak:eval txt (client "client0") (session "wss"))
   (run-process ["kak" "-p" session]
       coprocess: (kak:p (string-append "eval -client " client " %{ " txt "}"))))

 (def (kak:edit file (line #f) (client "client0") (session "wss"))
   (kak:eval (string-append
              "edit -existing " file " " (if line line ""))))

 (def (kak:edit-fifo lines str/PR (client "client0") (session "wss"))
  (let (fifo "/tmp/gh-prr.replies.fifo")
   (unless (file-exists? fifo)
           (create-fifo fifo))
   (kak:eval (string-append
              "edit -fifo " fifo " *pr-" str/PR "-replies*"))
   (write-file-lines fifo lines))))

(import :std/misc/process
        :std/text/json
        :std/srfi/19 ; dates
        :std/format
        :std/sugar
        :std/getopt
        :std/misc/ports)
(import :tui
        hash-xtra
        gh
        git
        kakoune)

(def my-name "aecepoglu")
(def BROWSER "firefox")
(def (me? x) (equal? x my-name))

(def (repl-stdin-fix)
 (run-process ["stty" "sane"] stdin-redirection: #f))

(def (run-length-encode eq? xs)
 (let aux ((a '()) (x xs))
  (if (null? x)
   (reverse a)
   (let (a_ (if (and (not (null? a))
                     (eq? (car x) (caar a)))
              (cons (cons (caar a) (1+ (cdar a)))
               (cdr a))
              (cons [(car x) . 1] a)))
    (aux a_ (cdr x))))))

(def (listreview->string r prefix: (prefix "  "))
   (string-append
      prefix
      (hash-refs r ['author 'login])
      " "
      (hash-ref r 'state)
      " at "
      (hash-ref r 'submittedAt)
      ":"
      (hash-ref r 'body)))

(def (reviews->summary r)
  (let (counted (lambda (x) (string-append
                             (car x)
                             (with (n (cdr x))
                                   (if (> n 1) (num->superscript (cdr x))
                                               "")))))
   (string-join
    (map counted
     (run-length-encode string=?
      (map (cut hash-refs <> ['author 'login])
       r)))
    " ")))

(def (listedPR->string t)
  (let-hash t
   (string-append
    (number->string .number)
    " "
    .title
    " ("
    (hash-refs t ['author 'login])
    " > "
    (string-join
     (map (cut hash-ref <> 'login)
      .reviewRequests)
     ",")
    ")\n    "
    .baseRefName " <_-_ " .headRefName
    (let (r .reviews)
      (if (> (length r) 0)
        (string-append "\n    " (reviews->summary r))
        "")))))

(def (has-no-reviews? listed-pr)
  (= 0 (length (hash-ref listed-pr 'reviews))))

(def (is-mine? pr)
  (me? (hash-refs pr ['author 'login])))

(def (requests-me? listed-pr)
  (ormap
   (lambda (r) (me? (hash-ref r 'login)))
   (hash-ref listed-pr 'reviewRequests)))

(def (interesting? listed-pr)
  (or (has-no-reviews? listed-pr)
      (is-mine? listed-pr)
      (requests-me? listed-pr)))

(def (time->relative t)
  (let (s (time-second t))
   (with ([m h d] (map (lambda (x) (floor (/ s x)))
                       [60 3600 86400]))
       (with ([num . unit] (cond ((<= m 120) [m . "mins"])
                                 ((<= h 48)  [h . "hours"])
                                 (else       [d . "days"])))
             (format "~d ~a ago" num unit)))))

(def t:rel-treshold (make-time 'time-duration 0 (* 60 60 24 30)))
(def (humanised date (now (current-time 'time-utc)))
  (let* ((d (string->date date "~Y-~m-~dT~H:~M:~SZ"))
         (dt (time-difference now (date->time-utc d))))

   (if (time>=? t:rel-treshold dt)
     (time->relative dt)
     (date->string d "~Y-~m-~d"))))

(def reformat-body identity)

(def (thread->commit thread)
  ; TODO instead of filter, use a (find)
  (let* ((comments (@ thread 'comments 'nodes))
         (resp (filter (? (not is-mine?)) (reverse comments)))
         (comment (if (null? resp)
                   (car comments)
                   (car resp))))
    (@ comment 'commit 'oid)))
    

(def (handle-input t repo str/PR pr)
  ;(hash-keys (last (@ (@ pr 'repository 'pullRequest 'reviewThreads 'nodes 0) 'comments 'nodes)))
  (case (tui:read-key)
     ((#\q) [#f 0]) ; I can move onto another PR?
     ((#\n) [#t 1])
     ((#\p) [#t -1])
     ((#\G) (let (err (git:state-ok? (@ pr 'headRefName)
                                     (thread->commit t)))
             (if err
               (tui:sayln err "press 'c' to checkout this branch & commit"
                          "(make sure you've stashed your changes and stuff)")
               (tui:sayln "ALL GOOD"))
             [#t 0]))
   ((#\c) (chain t
            thread->commit
            git:checkout ; TODO test again
            tui:say-lines)
          [#t 0])
   ((#\r) (tui:sayln "Please send data into /tmp/gh-prr.in.fifo")
          (let (lines (apply append
                       (map thread->reply-tpl threads)))
           (kak:edit-fifo threads str/PR))
          [#t 0])
   ((#\z) (let (resp (gh:resolve-thread (hash-ref t 'id)))
            (tui:sayln (string-append
                        "Resolving thread is... "
                        (if (@ resp 'resolveReviewThread 'thread 'isResolved) "OK." "FAIL!"))))
          [#t 0])
   ((#\w) (run-process [BROWSER (@ t 'comments 'nodes 0 'url)]) [#t 0])
   ((#\i) (interactive:reply t repo str/PR) [#t 0])
   ((#\g) (kak:edit (hash-ref (car t) 'path) (comment->line (car t)))
          [#t 0])
   (else  [#t 0])))

(def (review-threads-interactive pr repo str/PR fifo stdin)
 (let* ((v (list->vector (@ pr 'reviewThreads 'nodes)))
        (n (1- (vector-length v)))
        (tty-state (tui:init!)))
  (let R ((j -1) (i 0) (loops 0))
    (when (not (= i j))
     (tui:say-lines (cons
                     "-----------------------------------------------"
                     (thread->strings (vector-ref v i)))))
    (say (fmt bri) "thread " (number->string (1+ i)) "/" (number->string (1+ n)) (fmt) " "
         (fmt ita) "(g G c n p i r z w):" (fmt))
    (let (e (sync fifo stdin))
     (cond ; TODO move di over here and call R at a single point
      ((> loops 50)     #t)
      ((not (port? e)) (R i i (1+ loops)))
      ((equal? e fifo)
       (let (x (ReplyTpl#read fifo)) ; TODO return a record instead
        (unless (null? x)
          (tui:sayln "\n\rSending reply to " (car x))
          (gh:reply
           repo ; TODO these should be read from the incoming reply data
           str/PR
           (car x)
           (string-join (cdr x) "\r\n"))))
       (R i i (1+ loops)))
      ((equal? e stdin) (with ([fin? di] (handle-input (vector-ref v i)
                                                       repo str/PR pr))
                          (when fin? (R i (limit 0 (+ i di) n) (1+ loops)))))
      (else (R i i (1+ loops))))))
  (tui:restore! tty-state)
  (displayln "Bye!")))

(def (comments->threads comments)
  (let ((tbl (make-hash-table))
        (pred (cut hash-key? <> 'in_reply_to_id)))
   (for-each (lambda (x) (hash-put! tbl (hash-ref x 'id) (cons x [])))
            (filter (? (not pred)) comments))
   (for-each (lambda (x) (hash-update! tbl (hash-ref x 'in_reply_to_id) (cut cons x <>) x))
            (filter (? pred) comments))
   ; TODO did I add them in the right order?
   (map reverse (hash-values tbl))))

(def (comment->line comment)
  ; start_line ; original_start_line
  ; line ; original_line
  (let-hash comment
    (if (void? .line)
        "??"
        (number->string .line))))

(def (comment->string comment) ; TODO remove if nobody refers to this
  (let-hash comment
    (string-append
     (hash-refs comment ['user 'login]) " (" (humanised .created_at) "): "
     (reformat-body .body))))
(def (comment->string_v2 comment)
  (let-hash comment
    (string-append
     (hash-refs comment ['author 'login]) " (" (humanised (if (void? .lastEditedAt) .createdAt .lastEditedAt)) "): "
     (reformat-body .body))))

(def (thread->strings thread)
  (let-hash thread
    (append
     ; TODO only line comments have a .line
     [(format "• (~a) ~a:~d" .subjectType .path .line)]
     (map (cut string-append "  " <>) (map comment->string_v2 (hash-ref .comments 'nodes)))
     (if .isResolved ["======== RESOLVED ========"] []))))

(def (interactive:reply thread repo str/PR)
  (tui:say-lines ["" "Please enter a line of comment below."])
  (say "Comment (no backspaces): " (fmt ita))
  (let (msg (tui:read-line-echoing))
    (say (fmt))
    (send-reply repo str/PR (hash-ref (car thread) 'databaseId) [msg]))) ; TODO test. Use gql instead
    
(def (send-reply repo str/PR parent msg)
  (say "... sending reply ...")
  (gh:reply repo str/PR parent (string-join msg "\n\r"))
  (tui:sayln " sent."))

(def opt-parser
  (getopt
   (command 'list
     help: "lists PRs")
   (command 'comments
     help: "list comments in PR"
     (argument 'pr))
   (command 'review
     help: "review comments in PR"
     (argument 'pr))
   (command 'sum
     help: "summarise PR"
     (argument 'pr))
   (command 'wip
     help: "test current feature in development")
   (option 'repo "-r" help: "repo org/name" default: "aecepoglu/gh-prr")))

(def (op:summarise repo str/PR)
  (let (pr (gh:pr-info str/PR))
   (let-hash (@ pr 'repository 'pullRequest)
    (tui:sayln (number->string .number) " " (fmt bri) (hash-ref .author 'login) " "(fmt ita) .title (fmt)" " (if (void? .reviewDecision) "-" .reviewDecision)
               "\r\n\r\n" .bodyText "\r\n\r\n"
               (fmt und) .url (fmt))
    (tui:sayln "\r\n-------------- Comments ------------------")
    (for-each (lambda (x)
                (thread:display-summary (@ x 'comments 'nodes))
                (tui:sayln "---------------"))
     ; highlight / dim instead?
     (filter thread-needs-attn? (hash-ref .reviewThreads 'nodes))))
   #t))

(def (op:list-prs repo)
 (chain
  (gh:pr-list repo json-fields: ["number" "title" "author"
                                 "reviewRequests" "reviews"
                                 "headRefName" "baseRefName"])
  (filter interesting? <>)
  (map listedPR->string <>)
  (map (lambda (x) (string-append "• " x)) <>)
  (for-each displayln <>)))

(def (op:list-comments repo pr)
  (chain
   (gh:pr-info pr) ; TODO pass in repo
   (@ <> 'repository 'pullRequest 'reviewThreads 'nodes)
   (map thread->strings <>)
   (apply append <>)
   (for-each displayln  <>)))

(def (op:review-comments repo prNo)
  (let* (pr (gh:pr-info prNo)) ; TODO repo
   (with-input-fifo "/tmp/gh-prr.in.fifo"
     (cut review-threads-interactive
          (@ pr 'repository 'pullRequest) repo pr
          <> (fdopen 0 'in 'pipe)))))

(def (main . args)
 (try
  (let-values (((cmd opt) (getopt-parse opt-parser args)))
    (let-hash opt
      (case cmd
          ((list)     (op:list-prs .repo))
          ((comments) (op:list-comments .repo .pr))
          ((review)   (op:review-comments .repo .pr))
          ((sum)      (op:summarise .repo .pr))
          ((wip)      (wip))))
    #t)
  (catch (getopt-error? exn)
   (getopt-display-help exn "gh-prr" (current-error-port))
   (exit 1))))

(def (comment=? a b)
  (equal? (hash-ref a 'databaseId)
          (hash-ref b 'databaseId)))

(def (thread->pivots comments)
 (let ((fst (car comments))
       (lst (last comments)))
      (if (comment=? fst lst)
       [fst 0 #f]
       [fst (- (length comments) 2) lst])))
                   
(def (thread:display-summary comments)
  (with ([a b c] (thread->pivots comments))
    (tui:sayln (comment->string_v2 a))
    (when (> b 0)
     (tui:sayln (fmt dim ita) "(" (number->string b) " more hidden)" (fmt)))
    (when c
     (tui:sayln (comment->string_v2 c)))))

(def (thread-needs-attn? thread)
  (not (hash-ref thread 'isResolved)))

(def (repl-stuff)
 (repl-stdin-fix)
 (def comments (gh:pr-comments "aecepoglu/gh-prr" "1"))
 (def threads (comments->threads comments))
 (kak:edit-replies threads "1")
 (def pr (gh:pr-view "senchabot-dev/monorepo" "85"
                     json-fields: ["number" "baseRefName" "headRefName" "title"
                                   "body" "author" "reviewDecision" "url"]))
 (hash-keys (car comments))
 #t)

(def (try-create-fifo path)
 (unless (file-exists? path) (create-fifo path))
 path) ; TODO very naive

(def (open-input-fifo path)
  (open-input-file (try-create-fifo path)))

(def (with-input-fifo path f)
  (let* ((port (open-input-fifo path))
         (lock (open-output-file path)))
    (f port)
    (close-port lock)
    (close-port port)))


(import (prefix-in ReplyTpl ReplyTpl#))
     
(def (thread->reply-tpl thread)
  (ReplyTpl#write
   (number->string (hash-ref (car thread) 'id))
   (map comment->string thread)))

(def (wip)
  ;(load "code.scm")
  (def pr (gh:pr-info "1"))
  (displayln (@ pr 'repository 'pullRequest 'reviewThreads 'nodes 0 'id))
  (gh:resolve-thread
   (@ pr 'repository 'pullRequest 'reviewThreads 'nodes 0 'id)))
; TODO be able to read error from stderr
; TODO replies should be trimmed before being sent
; TODO open a PR
; TODO syntax highlighting for kakoune files
