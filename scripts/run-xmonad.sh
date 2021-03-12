#!/bin/sh
#
# launch xmonad, with a couple of dzens to run the status bar
# send xmonad state over a named pipe
#

FG='#a8a3f7' 
BG='#3f3c6d' 
FONT="-xos4-terminus-medium-r-normal--16-160-72-72-c-80-iso8859-1"

PATH=${HOME}/bin:$PATH

# simple xmonad use, no interactive status bar.
#
#clock | dzen2 -ta r -fg $FG -bg $BG -fn $FONT &
#xmonad

#
# with a pipe talking to an external program
#
PIPE=$HOME/.xmonad-status
rm -f $PIPE
PATH=${PATH}:/sbin mkfifo -m 600 $PIPE
[ -p $PIPE ] || exit

# launch the external 60 second clock, and launch the workspace status bar
xmonad-clock | dzen2 -e '' -x 300 -w 768 -ta r -fg $FG -bg $BG -fn $FONT &

# and a workspace status bar
dzen2 -e '' -w 300 -ta l -fg $FG -bg $BG -fn $FONT < $PIPE &

# go for it
xmonad > $PIPE & 

# wait for xmonad
wait $!

pkill -HUP dzen2
pkill -HUP -f xmonad-clock

wait
