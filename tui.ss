(import
 :std/os/fdio
 :std/format
 :gerbil/gambit/bytes
 :termios)

(export (except-out #t tty-get init-fd!))

(def stdin 0)
(def stdout 1)
(def stderr 2)

(def (tty-get fd)
 (let (x (malloc-termios))
   (when (= (getattr fd x) -1)
     (raise "getattr failed :/"))
   x))

(def (init-fd! fd)
  (let ((bkp (tty-get fd))
        (raw (tty-get fd)))
   (makeraw raw)
   (when (= (setattr fd 1 raw) -1) ; assert non negative
     (raise "setattr failed :/"))
   bkp))

(def (tui:init! (fds [stdin stdout]))
  (map
   (lambda (x) (cons x (init-fd! x)))
   fds))

(def (tui:restore! termioses)
  (for-each
   (lambda (x)
     (setattr (car x) 0 (cdr x))) ; 0 = TCSANOW
   termioses))

(define-syntax fmt
  (syntax-rules ()
    ((fmt x ...) (string-append "\033[";]
                  (string-join (list x ...) ";")
                  "m"))))

(defvalues (pln nrm bri dim ita und rev)
  (values    "0" "2" "1" "2" "3" "4" "7"))

(defvalues (black red green yellow blue magenta cyan white)
  (values   "0"   "1" "2"   "3"    "4"  "5"     "6"  "7"))

(def (fg x) (string-append "3" x))
(def (bg x) (string-append "4" x))
(def (fG x) (string-append "9" x))
(def (bG x) (string-append "10" x))

(def tui:read-key
  (let (buf (make-bytes 8))
   (lambda ()
     (fdread stdin buf 0 1)
     (integer->char (bytes-ref buf 0)))))


(def (say . x)
  (let (b (string->bytes (apply string-append x)))
    (fdwrite stdout b 0 (bytes-length b))))

(def (tui:sayln . x)
  (apply say (append x ["\r\n"])))

(def (tui:sayf . x)
  (say (apply format x)))

(def (tui:say-lines lines)
  (for-each tui:sayln lines))

;; return #f if exited via ctrl-c, a single line of text otherwise
(def (tui:read-line-echoing)
  (let F ((acc []))
    (unless (null? acc)
      (say (list->string [(car acc)])))
    (let (c (tui:read-key))
      (case c
        ((#\x3)        (when #t (say "(ctrl+c)")) #f)
        ((#\newline
          #\return)    (list->string (reverse acc)))
        ((#\backspace
          #\delete)    (say "\033[2D" "\033[0K")
                       (F ((if (null? acc) identity cdr)
                           acc)))
        (else          (F (cons c acc)))))))

;; REFERENCES
;; Everything you ever wanted to know about terminals
;;   https://xn--rpa.cc/irl/term.html

