#!/bin/env gxi

(import :std/os/fd 
        :std/event
        :std/misc/process
        :std/format
        :std/sugar
        :std/getopt
        :std/misc/ports
        :std/srfi/19 ; dates
        :tui
        "git"
        "kakoune"
        (prefix-in "reply-tpl" ReplyTpl#))

(def (TODO . args) #!void)

(def (env-or k v)
  (try (getenv k) (catch _ v)))

(def (limit lower x upper)
  (min upper (max lower x)))

(def (num->superscript n)
  (let F ((a []) (n n))
   (if (> n 0)
     (F (cons (string-ref "⁰¹²³⁴⁵⁶⁷⁸⁹" (remainder n 10)) a)
        (floor (/ n 10)))
     (list->string a))))

(def (@ o0 . k0) ; TODO change to a macro
  (let F ((o o0) (k k0))
   (if (null? k) o
     (with ([h . t] k)
           (F ((if (number? h) list-ref hash-ref) o h)
              t)))))

(module gh
 (import
  :std/misc/process
  :std/text/json)
 (export #t)

 (def gql:pr "query Foo($owner: String!, $repo: String!, $pr: Int!, $endCursor:String) {
   repository(owner: $owner, name: $repo) {
      pullRequest(number: $pr) {
         repository {nameWithOwner}, baseRefName, headRefName,
         number, author {login}, bodyText, title, url
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
  (run-process ["gh" "--jq" "." . args] coprocess: read-json environment: gh-env))

 (def (gql args)
   (hash-ref
    (gh-cli ["api" "graphql" "--paginate" "--cache" "1m" . args])
    'data))

 (def (gh:pr-list repo json-fields: (fields ["id"]))
  (gh-cli ["pr" "list"
           "--repo" repo
           "--json" (string-join fields ",")
           "--limit" "100"]))

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

(import gh)


(def BROWSER (env-or "BROWSER" "firefox"))
(def my-name "aecepoglu")
(def (me? x) (equal? x my-name))

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
      (@ r 'author 'login)
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
      (map (cut @ <> 'author 'login)
       r)))
    " ")))

(def (listedPR->string t)
  (let-hash t
   (string-append
    (number->string .number)
    " "
    .title
    " ("
    (@ t 'author 'login)
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
  (me? (@ pr 'author 'login)))

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
     ((#\q) [#f 0])
     ((#\?) (tui:say-lines ["q : Quit"
                            "n|p : Next|Prev"
                            "g : Goto line in editor"
                            "G : check Git sanity"
                            "c : Checkout latest commit someone else commented on"
                            "r : edit Replies in editor"
                            "i : Inline reply (a single line reply right here)"
                            "z : reZolve current conversation (without a comment)"
                            "w : open comment in Web browser"])
            [#t 1])
     ((#\n) [#t 1])
     ((#\p) [#t -1])
     ((#\g) (vscode:edit (hash-ref t 'path)
                   (number->string (hash-ref t 'line)))
            [#t 0])
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
            (let (lines (pr->reply-tpl pr))
             (kak:edit-lines lines (string-append "*pr-" str/PR "-replies*")))
            [#t 0])
     ((#\z) (let (resp (gh:resolve-thread (hash-ref t 'id)))
              (tui:sayln (string-append
                          "Resolving thread is... "
                          (if (@ resp 'resolveReviewThread 'thread 'isResolved) "OK." "FAIL!"))))
            [#t 0])
     ((#\w) (run-process [BROWSER (@ t 'comments 'nodes 0 'url)]) [#t 0])
     ((#\i) (interactive:reply t repo str/PR) [#t 0])
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
    (say (fmt bri) "\rthread " (number->string (1+ i)) "/" (number->string (1+ n)) (fmt) " "
         (fmt ita) "(? for info):" (fmt))
    (let (e (sync fifo stdin))
     (cond ; TODO move di over here and call R at a single point
      ((> loops 50)     #t)
      ((not (port? e)) (R i i (1+ loops)))
      ((equal? e fifo)
       (match (ReplyTpl#read fifo)
         ([]             #t)
         ([[r p c]]      #t)
         ([[r p c] . lines]
          (gh:reply r p c
           (string-join lines "\n\r"))))
       (R i i (1+ loops)))
      ((equal? e stdin) (with ([fin? di] (handle-input (vector-ref v i)
                                                       repo str/PR pr))
                          (when fin? (R i (limit 0 (+ i di) n) (1+ loops)))))
      (else (R i i (1+ loops))))))
  (tui:restore! tty-state)
  (displayln "Bye!")))

(def (comment->string comment)
  (let-hash comment
    (string-append
     (@ comment 'author 'login) " (" (humanised (if (void? .lastEditedAt) .createdAt .lastEditedAt)) "): "
     (reformat-body .body))))

(def (thread->strings thread)
  (let-hash thread
    (append
     ; TODO only line comments have a .line
     [(format "• (~a) ~a:~d" .subjectType .path .line)]
     (map (cut string-append "  " <>) (map comment->string (hash-ref .comments 'nodes)))
     (if .isResolved ["======== RESOLVED ========"] []))))

(def (pr->reply-tpl pr)
  (let ((prnum (number->string (@ pr 'number)))
        (repo (@ pr 'repository 'nameWithOwner))
        (threads (@ pr 'reviewThreads 'nodes)))
   (apply append
     (map (lambda (thread)
           (ReplyTpl#make repo prnum
                 (number->string (@ thread 'comments 'nodes 0 'databaseId))
                 (thread->strings thread)))
        threads))))

(def (interactive:reply thread repo str/PR)
  (tui:say-lines ["" "Please enter a line of comment below."])
  (say "Comment (no backspaces): " (fmt ita))
  (let (msg (tui:read-line-echoing))
    (say (fmt))
    (gh:reply "aecepoglu/gh-prr" "2" "1191546052" msg))) ; TODO test. Use gql instead
    
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
               "\n\r\n\r" .bodyText "\n\r\n\r"
               (fmt und) .url (fmt))
    (tui:sayln "\n\r-------------- Comments ------------------")
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
          (@ pr 'repository 'pullRequest) repo prNo
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
    (tui:sayln (comment->string a))
    (when (> b 0)
     (tui:sayln (fmt dim ita) "(" (number->string b) " more hidden)" (fmt)))
    (when c
     (tui:sayln (comment->string c)))))

(def (thread-needs-attn? thread)
  (not (hash-ref thread 'isResolved)))

(def (repl-stuff)
 (repl-stdin-fix)
 (def comments (gh:pr-comments "aecepoglu/gh-prr" "2"))
 (def threads (comments->threads comments))
 (kak:edit-replies threads "2")
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


(def (wip)
 (load "code.scm")
 (def pr (@ (gh:pr-info "2") 'repository 'pullRequest))
 (run-process ["stty" "sane"] stdin-redirection: #f)
 #t)
; TODO be able to read error from stderr
; TODO open a PR
; TODO syntax highlighting for kakoune files
; participants(first:100) { nodes { login}} may be useful in a PR to retrieve info about all participants so I can assign them unique colours
