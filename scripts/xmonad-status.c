/*
   Module      :  xmonad-workspace.c
   Copyright   :  (c) Don Stewart 2007
   License     :  BSD3-style (see LICENSE)
   
   Maintainer  :  dons@cse.unsw.edu.au
   Stability   :  stable
   Portability :  portable
  
   C parser for new workspace format
  
*/
 
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>

#define WORKSPACES 9

int main(void) {

    char buf[1024]; 
    char *s, current, *rest;
    int i;

    signal(SIGPIPE, SIG_IGN);

    while (fgets(buf, sizeof(buf), stdin) != NULL) {

        i = strlen(buf);
        buf[i-1] = '\0';
        s = buf;
            
        /* extract tag of current workspace */
        current = *(char *)strsep(&s,"|");
        rest    = s;

        /* split up workspace list */
        /* extract just the tags of the workspace list */
        for (i = 0; i < WORKSPACES; i++) {
            s = (char *)strsep(&rest, ",");

            if (*s == current) {
                printf("[%c]", *s);
            } else if (s[2] != ':') { /* filter empty workspaces */
                printf(" %c ", *s);
            }

        }

        putchar('\n');
        fflush(stdout);
    }
    return EXIT_SUCCESS;
}
