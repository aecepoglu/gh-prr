(import :std/foreign)

(export set-raw-mode tcgetattr read-in-mode)

(begin-ffi (tcgetattr tcsetattr set-raw-mode read-in-mode)
 (c-declare "
#include <termios.h>
#include <unistd.h>
#include <stdio.h>
void set_raw_mode(struct termios *t) {
	t->c_lflag &= ~(ECHO | ICANON);
}
char read_in_mode(int fd, struct termios *in, struct termios *out) {
	char c;
	tcsetattr(fd, TCSAFLUSH, in);
   int x = read(fd, &c, 1);
	tcsetattr(fd, TCSAFLUSH, out);
   return c;
}
")

 (define-c-struct termios)
 (define-c-lambda set-raw-mode
   (termios*) void "set_raw_mode")
 (define-c-lambda read-in-mode
   (int termios* termios*) char "read_in_mode")
 (define-c-lambda getattr
   (int termios*) int "tcgetattr")
 (define-c-lambda setattr
   (int int termios*) int "tcsetattr"))
