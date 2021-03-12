#! /bin/sh
# Script to print common window properties in ManageHook format,
# via xprop.  All xprop options may be used, although anything other
# than -display, -id, and -name is probably a bad idea.
#
# Written and placed into the public domain by Brandon S Allbery
# KF8NH <allbery.b@gmail.com>
#

exec xprop -notype \
  -f WM_NAME        8s ':\n  title =\? $0\n' \
  -f WM_CLASS       8s ':\n  appName =\? $0\n  className =\? $1\n' \
  -f WM_WINDOW_ROLE 8s ':\n  stringProperty "WM_WINDOW_ROLE" =\? $0\n' \
  WM_NAME WM_CLASS WM_WINDOW_ROLE \
  ${1+"$@"}
