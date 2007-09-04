/*

dwm/xmonad status bar provider. launch from your .xinitrc, and pipe
into dzen2.
 
to compile: gcc -Os -s -o xmonad-clock xmonad-clock.c
 
Copyright (c) 2007, Tom Menari <tom dot menari at googlemail dot com>
Copyright (c) 2007, Don Stewart
 
Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.
 
THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/
 
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
 
/* configuration */
#define REFRESH_RATE    60
#define TIME_FORMAT     "%H.%M %a %b %d" 
#define TIME_FORMAT2    "SYD %H.%M"
 
int main(void) {
    char b[34];
    char c[34];
    time_t epochtime;
    struct tm *realtime;

    time_t pdttime;
    struct tm *pdtrealtime;

    double load[3];
 
    signal(SIGPIPE, SIG_IGN);
 
    for(;;) {
        getloadavg(load, 3);
 
        epochtime = time(NULL);
        realtime  = localtime(&epochtime);
        strftime(b, sizeof(b), TIME_FORMAT, realtime);

        setenv("TZ","Australia/Sydney", 1);
        pdttime      = time(NULL);
        pdtrealtime  = localtime(&pdttime);
        strftime(c, sizeof(c), TIME_FORMAT2, pdtrealtime);
        unsetenv("TZ");

        fprintf(stdout, "%s | %s | %.2f %.2f %.2f | xmonad 0.3 \n", b, c, load[0], load[1], load[2]);

        fflush(stdout);
        sleep(REFRESH_RATE);
    }
    return EXIT_SUCCESS;
}
