#!/bin/env gxi
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
;(def gh-env [(string-append
;              "GH_CONFIG_DIR="
;              (path-normalize
;               (string-append (current-directory)
;                              (if #t "../" "")))
;              "gh-config-dir")])
 (def gh-env #f)
 (def (gh-cli args)
  (displayln "gh" args)
  (run-process ["gh" . args] coprocess: read-json environment: gh-env))

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
           "--json" (string-join fields ",")])))

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
   (run-process ["git" "checkout" commit] coprocess: read-all-as-lines)))

(import :std/misc/process
        :std/text/json
        :std/srfi/19 ; dates
        :std/format
        :std/sugar
        :std/getopt
        :std/misc/ports)
(import :std/os/fd
        :std/os/fdio
        :std/os/fcntl)
(import :myuser/termios
        :std/misc/process)

(import hash-xtra gh git)

(def my-name "aecepoglu")
(def (me? x) (equal? x my-name))

(def (get-tty-state fd)
 (let (x (malloc-termios))
   (getattr fd x)
   x))
(def (set-raw-mode_ x)
 (set-raw-mode x)
 x)

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

(def (thread->commit thread)
  (hash-ref (car thread) 'commit_id))


(def (thread:git-good? thread)
  (let-hash (car thread)
    (git:state-ok? .branch_name .commit_id)))

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

(def (num->superscript n)
  (let F ((a []) (n n))
   (if (> n 0)
     (F (cons (string-ref "⁰¹²³⁴⁵⁶⁷⁸⁹" (remainder n 10)) a)
        (floor (/ n 10)))
     (list->string a))))

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
    (hash-ref t 'title)
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
  (or (has-no-reviews?)listed-pr
      (is-mine?)listed-pr
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

(def (comment->pretty-string c)
  (let-hash c
    (let* ((x .line)
           (line (if (void? x) "??"
                  (number->string .line))))
           ; start_line original_start_line start_side line original_line side original_position position
     (string-append
      "•  (" .subject_type " comment) "
      .path ":" line "\n"
      (hash-refs c ['user 'login]) " (" (humanised .created_at) "): "
      (reformat-body .body)
      "\n"))))

(def (op:list-prs repo)
  (chain
   (gh:pr-list repo json-fields: ["number" "title" "author"
                                  "reviewRequests" "reviews"
                                  "headRefName" "baseRefName"])
   (filter interesting? <>)
   (map listedPR->string <>)
   (map (lambda (x) (string-append "• " x)) <>)
   (for-each displayln <>)))

(def (comments->threads comments)
  (let ((tbl (make-table))
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

(def (comment->title comment)
  (let-hash comment
    (string-append
     "• (" .subject_type ") " .path ":" (comment->line comment)))) 
        

(def (comment->string comment)
  (let-hash comment
    (string-append
     (hash-refs comment ['user 'login]) " (" (humanised .created_at) "): "
     (reformat-body .body))))

(def (thread->strings comments)
  (string-join (cons
                (comment->title (car comments))
                (map comment->string comments)) "\r\n  "))
 
(def (op:list-comments repo pr)
  (chain
   (gh:pr-comments repo pr)
   (comments->threads <>)
   (map thread->strings <>)
   (for-each displayln  <>)))

; TODO I can generate an absolute path using git info
(def (kak:edit file (line #f) (client "client0") (session "wss"))
  (run-process ["kak" "-p" session]
      coprocess: (lambda (p)
                   (display (string-append "eval -client "
                             client
                             " %{ "
                             "edit -existing " file " "
                             (if line line "")
                             "}")
                    p)
                   (close-output-port p))))


(def (review-threads-interactive threads)
 (let* ((out (open-output-file "/dev/tty"))
        (v (list->vector threads))
        (n (1- (vector-length v)))
        (raw-mode  (set-raw-mode_ (get-tty-state 0)))
        (prev-mode (get-tty-state 0)))
   (let aux ((j -1) (i 0))
     (when (not (= i j))
      (displayln "\r\n-----------------------------------------------\r\n")
      (displayln (thread->strings (vector-ref v i)) "\r\n")
      (display (format "~d/~d {~a}:"
                       (1+ i) (1+ n) " (g)o can(G)o (c)heckout (n)ext (p)rev: ")))
     (let ((c (read-in-mode 0 raw-mode prev-mode)) (t (vector-ref v i)))
      (cond ((equal? c #\q) #t) ; I can move onto another PR?
            ((equal? c #\n) (aux i (min n (1+ i))))
            ((equal? c #\p) (aux i (max 0 (1- i))))
            ((equal? c #\G) (let (err (thread:git-good? t))
                             (if err
                               (display (string-append err "press 'c' to checkout this branch & commit (make sure you've stashed your changes and stuff)\r\n")
                                        out)
                               (display "ALL GOOD\r\n" out))
                             (aux i i)))
            ((equal? c #\c) (chain t
                              thread->commit
                              git:checkout
                              (map (lambda (x) (string-append x "\r\n")) <>)
                              (apply string-ap   pend <>)
                              display)
                            (aux i i))
            ((equal? c #\g) (kak:edit (hash-ref (car t) 'path) (comment->line (car t))) (aux i i))
            (else           (aux i i)))))
   (close-output-port out)
   (displayln "Bye!")))

(def (op:review-comments repo pr)
  (let (pr-info (gh:pr-view repo pr json-fields: ["headRefName"]))
   (chain
    (gh:pr-comments repo pr)
    comments->threads
    (map (lambda (x)
           ; all threads within a PR must belong to the same branch anyway
           ;  Maybe I should pass the branch_name (or pr-info) as an argument instead
           (hash-put! (car x) 'branch_name (hash-ref pr-info 'headRefName))
           x) <>)
    review-threads-interactive)))


(def opt-parser
  (getopt
   (command 'list
     help: "lists PRs")
   (command 'comments
     help: "list comments in PR"
     (argument 'pr))
   (option 'repo "-r" help: "repo org/name" default: "aecepoglu/gh-prr")))

(def (main . args)
 (try
  (let-values (((cmd opt) (getopt-parse opt-parser args)))
    (let-hash opt
      (case cmd
          ((list)     (op:list-prs .repo))
          ((comments) (op:review-comments .repo .pr)))))
          
  (catch (getopt-error? exn)
   (getopt-display-help exn "gh-prr" (current-error-port))
   (exit 1))))

;(def comments (gh:pr-comments "aecepoglu/gh-prr" "1"))
;(def pr (gh:pr-view "aecepoglu/gh-prr" "1" json-fields: ["headRefName"]))

