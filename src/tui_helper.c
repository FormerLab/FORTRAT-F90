/* fortrat_tui_helper.c
   Terminal helpers for FORTRAT — called from Fortran via ISO C binding.
*/
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <stdio.h>

static struct termios saved_termios;

static void xwrite(int fd, const void *buf, size_t n) {
    ssize_t r = write(fd, buf, n);
    (void)r;
}

int fortrat_enter_raw(void) {
    struct termios raw;
    if (tcgetattr(STDIN_FILENO, &saved_termios) < 0) return -1;
    raw = saved_termios;
    raw.c_lflag &= ~(ICANON | ECHO | ISIG);
    raw.c_iflag &= ~(IXON | ICRNL);
    raw.c_cc[VMIN]  = 1;
    raw.c_cc[VTIME] = 0;
    if (tcsetattr(STDIN_FILENO, TCSANOW, &raw) < 0) return -1;
    int flags = fcntl(STDIN_FILENO, F_GETFL, 0);
    fcntl(STDIN_FILENO, F_SETFL, flags | O_NONBLOCK);
    return 0;
}

int fortrat_exit_raw(void) {
    int flags = fcntl(STDIN_FILENO, F_GETFL, 0);
    fcntl(STDIN_FILENO, F_SETFL, flags & ~O_NONBLOCK);
    return tcsetattr(STDIN_FILENO, TCSANOW, &saved_termios);
}

void fortrat_winsize(int *cols, int *rows) {
    struct winsize ws;
    if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) == 0) {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
    } else {
        *cols = 120;
        *rows = 40;
    }
}

void fortrat_clear_screen(void) {
    xwrite(STDOUT_FILENO, "\033[?25l\033[2J\033[H", 11);
}

void fortrat_write_at(int row, int col, const char *str, int len) {
    char buf[32];
    int n = snprintf(buf, sizeof(buf), "\033[%d;%dH", row, col);
    xwrite(STDOUT_FILENO, buf, n);
    xwrite(STDOUT_FILENO, str, len);
}

/* Write an entire pre-built frame buffer in one shot */
void fortrat_write_buf(const char *buf, int len) {
    xwrite(STDOUT_FILENO, buf, len);
}

void fortrat_flush(void) {
    fsync(STDOUT_FILENO);
}

int fortrat_read_key(char *buf) {
    return (int)read(STDIN_FILENO, buf, 4);
}
