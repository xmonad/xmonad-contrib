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
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>

#define WORKSPACES 9

int main(void) {

    size_t len;
    char workspaces[WORKSPACES];
    char buf[1024]; 
    char *s, *p, *q, current, *rest;
    int n, i = 0;

    signal(SIGPIPE, SIG_IGN);

    while (fgets(buf, sizeof(buf), stdin) != NULL) {

        n = strlen(buf);
        buf[n-1] = '\0';
        s = buf;
            
        /* extract tag of current workspace */
        current = *(char *)strsep(&s,"|");
        rest    = s;

        /* split up workspace list */
        /* extract just the tags of the workspace list */
        while (i < WORKSPACES) {
            workspaces[i++] = *(char *)strsep(&rest, ",");
        }

        /* now print out list */
        for (i = 0; i < WORKSPACES; i++) {
            printf(((workspaces[i] == current) ? "[%c]" : " %c "), workspaces[i]);
        }

        putchar('\n');
        fflush(stdout);
    }
    return EXIT_SUCCESS;
}
