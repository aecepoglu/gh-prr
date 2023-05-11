(import :std/foreign)

(export #t)

(begin-ffi (getattr setattr makeraw (struct termios))
 (c-declare "#include <termios.h>")

 (define-c-struct termios)
 (define-c-lambda makeraw
   (termios*) void "cfmakeraw")
 (define-c-lambda getattr
   (int termios*) int "tcgetattr")
 (define-c-lambda setattr
   (int int termios*) int "tcsetattr"))

;; REFERENCES
;; https://cons.io/guide/ffi.html#basic-ffi
;; man tcgetattr(3)
