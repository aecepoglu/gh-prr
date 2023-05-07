#!/bin/env gxi
(import :std/misc/process)
(import :std/misc/ports)
(import :std/sugar)

(def color:default "\033[0m")
(def (gh:pr/checks!)
  (run-process ["gh" "pr" "checks"]
               coprocess: read-all-as-lines
               check-status: #f))

(defstruct check (new? src status t url))

(def (pending? x) (eq? 'pending (check-status x)))

(def status->icon
  (let (tbl (plist->hash-table
             ['pass "✓" 'pending "*" 'skipping "-" 'fail "X"]))
    (cut hash-ref tbl <> "?")))

(def string->status string->symbol)

(def (line->check x)
  (with ([a b c d] (string-split x #\tab))
     (make-check #f a (string->status b) c d)))

(def (check->id x)
  (string-append (check-src x) (check-url x)))

(def status->color
  (let (tbl (plist->hash-table
             ['pass "\033[42m" 'skipping "\033[45m" 'fail "\033[41m"]))
    (cut hash-ref tbl <> "\033[44m")))

(def (check->icon x)
  (let* ((status (check-status x))
         (icon (status->icon status)))
    (if (check-new? x)
      (string-append
       (status->color status)
       icon
       color:default)
      icon)))
(def (checks->statusline checks)
 (apply string-append
   (map check->icon checks)))

(def (check->string  x)
  (string-append
   (symbol->string (check-status x))
   "\t" (check-t x)
   "\t" (check-src x)
   "\t" (check-url x)))

(def (set-newness prevs news)
  (let (tbl (list->hash-table
             (map (cut cons <> #t)
              (map check->id
                prevs))))
   (for-each (lambda (x)
               (check-new?-set! x (not (hash-key? tbl (check->id x)))))
     news))
  news)

(def (display-checks checks)
  (for-each displayln
    (map check->string
     checks)))

(def (loop prev n)
  (let ((checks (set-newness prev (map line->check (gh:pr/checks!)))))
   (display "\r")
   (display-checks
    (filter (lambda (x) (and (check-new? x)
                             (not (pending? x))))
     checks))
   (display (string-append
             "t-" (number->string n) " "
             (checks->statusline checks)))
   (cond ((< n 0) (cons #t checks))
         ((ormap pending? checks)
          (thread-sleep! 10)
          (loop checks (- n 10)))
         (else (cons #f checks)))))

(with ([timeout? . checks] (loop [] 900))
  (displayln "")
  (if timeout?
    (for-each displayln
     (map check->string
      (filter pending?
        checks)))))
