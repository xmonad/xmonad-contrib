#!/bin/sh -eu

################################################################################
# Edit the following constants and then rename this script to ~/.xmonad/build

# The directory holding your source code and stack.yaml file:
SRC_DIR=~/develop/oss/xmonad-testing

# The name of the executable produced by stack.  This comes from the
# executable section of your *.cabal or package.yaml file.
EXE_NAME=xmonad-testing

################################################################################

# Unset STACK_YAML, to ensure that $SRC_DIR/stack.yaml is used.
unset STACK_YAML

# Do the build.
cd $SRC_DIR
stack build

# Create a hard link at the requested destination, replacing any existing one.
ln -f -T $(stack exec -- which $EXE_NAME) $1
