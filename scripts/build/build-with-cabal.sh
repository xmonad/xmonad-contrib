#! /bin/sh -eu

# Your source directory. Default is the config dir, if it can be found.
SRC_DIR=

# Executable name, from the executable stanza of your cabal file.
# The script will try to guess it if not specified.
EXE_NAME=

##############################################################################

# config

output="$1"

if [ "$SRC_DIR" = "" ]; then
    # look for the config directory, fall back to the old one
    SRC_DIR="${XMONAD_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config/xmonad}}"
    if test -f "$SRC_DIR/build"; then
	:
    else
	SRC_DIR="$HOME/.xmonad"
    fi
fi
cd "$SRC_DIR"

if [ "$EXE_NAME" = "" ]; then
    # try to extract the name of the executable
    EXE_NAME="$(awk '!done && /^executable / {print $2; done = 1}' *.cabal)"
    # note that there should be only one cabal file or cabal will report a
    # conflict; any utilities should be listed after the xmonad executable
fi

# do it to it

d="$(dirname "$output")"
f="$(basename "$output")"
first=0
for exe in $EXE_NAME; do
    cabal install exe:"$EXE_NAME" \
	  --enable-executable-stripping \
	  --enable-optimization=2 \
	  --installdir="$d" \
	  --overwrite-policy=always
    # NB. a cabal bug may mean it doesn't actually get stripped
    # we assume the first executable in the list is the new xmonad
    if [ $first = 0 ]; then
	first=1
	if [ "$f" = "$exe" ]; then
	    : someone will try it…
	else
	    ln -sf "$exe" "$output"
	fi
    elif [ "$f" = "$exe" ]; then
	# the link above just got replaced with a direct link into the
	# cabal package
	echo I hope you know what you\'re doing... >&2
    fi
done
