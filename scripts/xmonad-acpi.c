/*

dwm/xmonad status bar provider. launch from your .xinitrc, and pipe
into dzen2.
 
to compile: gcc -Os -s -o xmonad-acpi xmonad-acpi.c
 
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
#define REFRESH_RATE    2
#define TIME_FORMAT     "%a %b %d %H:%M:%S" 
#define BATTERY_INFO    "/proc/acpi/battery/BAT0/info"
#define BATTERY_STATE   "/proc/acpi/battery/BAT0/state"
 
int main(void) {
    FILE *acpi;
    char b[34];
    time_t epochtime;
    struct tm *realtime;
    int last_full, remaining;

    double load[3];
 
    signal(SIGPIPE, SIG_IGN);

    if ((acpi = fopen(BATTERY_INFO, "r")) == NULL) {
        perror("couldn't open "BATTERY_INFO);
        exit(-1);
    }
    while (fgets(b, sizeof(b), acpi)) 
        if (sscanf(b, "last full capacity: %d", &last_full) == 1)
            break;
    fclose(acpi);
 
    for(;;) {
        /* Load */
        getloadavg(load, 3);

        /* Battery */
        if ((acpi = fopen(BATTERY_STATE, "r")) == NULL) {
            perror("couldn't open "BATTERY_STATE);
            exit(-1);
        }
        while (fgets(b, sizeof(b), acpi))
            if (sscanf(b, "remaining capacity: %d", &remaining) == 1)
                break;
        fclose(acpi);
        
        /* Time */
        epochtime = time(NULL);
        realtime  = localtime(&epochtime);
        strftime(b, sizeof(b), TIME_FORMAT, realtime);


        fprintf(stdout, "%s | %.2f %.2f %.2f | %.1f%% \n", b, load[0], load[1], 
                load[2], (float) (remaining * 100) / last_full);
        fflush(stdout);
        sleep(REFRESH_RATE);
    }
    return EXIT_SUCCESS;
}
