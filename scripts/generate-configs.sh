#!/bin/bash

# generate-configs.sh - Docstring parser for generating xmonad build configs
#                       with default settings for extensions
# Author: Alex Tarkovsky <alextarkovsky@gmail.com>
# Released into the public domain

# This script parses custom docstrings specifying build-time configuration data
# from xmonad extension source files, then inserts the data into copies of
# xmonad's Config.hs and xmonad.cabal files accordingly.
#
# Usage: generate-configs.sh PATH_TO_CONTRIBS
#
# Run this script from the directory containing xmonad's main Config.hs and
# xmonad.cabal files, otherwise you'll need to change the value of
# $REPO_DIR_BASE below.
#
# The docstring markup can be extended as needed. Currently the following tags
# are defined, shown with some examples:
#
# ~~~~~
#
# %cabalbuilddep
#
#     Cabal build dependency. Value is appended to the "build-depends" line in
#     xmonad.cabal and automatically prefixed with ", ".  NB: Don't embed
#     comments in this tag!
#
# -- %cabalbuilddep readline>=1.0
#
# %def
#
#     General definition. Value is appended to the end of Config.sh.
#
# -- %def commands :: [(String, X ())]
# -- %def commands = defaultCommands
#
# %import
#
#     Module needed by Config.sh to build the extension. Value is appended to
#     the end of the default import list in Config.sh and automatically
#     prefixed with "import ".
#
# -- %import XMonadContrib.Accordion
# -- %import qualified XMonadContrib.FlexibleManipulate as Flex
#
# %keybind
#
#     Tuple defining a key binding. Must be prefixed with ", ". Value is
#     inserted at the end of the "keys" list in Config.sh.
#
# -- %keybind , ((modMask, xK_d), date)
#
# %keybindlist
#
#     Same as %keybind, but instead of a key binding tuple the definition is a
#     list of key binding tuples (or a list comprehension producing them). This
#     list is concatenated to the "keys" list must begin with the "++" operator
#     rather than ", ".
#
# -- %keybindlist ++
# -- %keybindlist -- mod-[1..9] @@ Switch to workspace N
# -- %keybindlist -- mod-shift-[1..9] @@ Move client to workspace N
# -- %keybindlist -- mod-control-shift-[1..9] @@ Copy client to workspace N
# -- %keybindlist [((m .|. modMask, k), f i)
# -- %keybindlist     | (i, k) <- zip [0..fromIntegral (workspaces-1)] [xK_1 ..]
# -- %keybindlist     , (f, m) <- [(view, 0), (shift, shiftMask), (copy, shiftMask .|. controlMask)]]
#
# %layout
#
#     A layout. Must be prefixed with ", ". Value is inserted at the end of the
#     "defaultLayouts" list in Config.sh.
#
# -- %layout , accordion
#
# %mousebind
#
#     Tuple defining a mouse binding. Must be prefixed with ", ". Value is
#     inserted at the end of the "mouseBindings" list in Config.sh.
#
# -- %mousebind , ((modMask, button3), (\\w -> focus w >> Flex.mouseResizeWindow w))
#
# ~~~~~
#
# NB: '/' and '\' characters must be escaped with a '\' character!
#
# Tags may also contain comments, as illustrated in the %keybindlist examples
# above. Comments are a good place for special user instructions:
#
# -- %def -- comment out default logHook definition above if you uncomment this:
# -- %def logHook = dynamicLog

if [[ -z "$1" || $# > 1 || ! -d "$1" ]] ; then
    echo "Usage: generate-configs.sh PATH_TO_CONTRIB"
    exit 1
fi

REPO_DIR_BASE="."

CABAL_FILE_BASE="${REPO_DIR_BASE}/xmonad.cabal"
CABAL_FILE_CONTRIB="${1}/xmonad.cabal"

CONFIG_FILE_BASE="${REPO_DIR_BASE}/Config.hs"
CONFIG_FILE_CONTRIB="${1}/Config.hs"

# Markup tag to search for in source files.
TAG_CABALBUILDDEP="%cabalbuilddep"
TAG_DEF="%def"
TAG_IMPORT="%import"
TAG_KEYBIND="%keybind"
TAG_KEYBINDLIST="%keybindlist"
TAG_LAYOUT="%layout"
TAG_MOUSEBIND="%mousebind"

# Insert markers to search for in Config.sh and xmonad.cabal. Values are
# extended sed regular expressions.
INS_MARKER_CABALBUILDDEP='^build-depends:.*'
INS_MARKER_DEF='-- Extension-provided definitions$'
INS_MARKER_IMPORT='-- Extension-provided imports$'
INS_MARKER_KEYBIND='-- Extension-provided key bindings$'
INS_MARKER_KEYBINDLIST='-- Extension-provided key bindings lists$'
INS_MARKER_LAYOUT='-- Extension-provided layouts$'
INS_MARKER_MOUSEBIND='-- Extension-provided mouse bindings$'

# Literal indentation strings. Values may contain escaped chars such as \t.
INS_INDENT_CABALBUILDDEP=""
INS_INDENT_DEF=""
INS_INDENT_IMPORT=""
INS_INDENT_KEYBIND="    "
INS_INDENT_KEYBINDLIST="    "
INS_INDENT_LAYOUT="                 "
INS_INDENT_MOUSEBIND="    "

# Prefix applied to inserted values after indent strings have been applied.
INS_PREFIX_CABALBUILDDEP=", "
INS_PREFIX_DEF="-- "
INS_PREFIX_IMPORT="--import "
INS_PREFIX_KEYBIND="-- "
INS_PREFIX_KEYBINDLIST="-- "
INS_PREFIX_LAYOUT="-- "
INS_PREFIX_MOUSEBIND="-- "

cp -f "${CABAL_FILE_BASE}" "${CABAL_FILE_CONTRIB}"
cp -f "${CONFIG_FILE_BASE}" "${CONFIG_FILE_CONTRIB}"

for extension_srcfile in $(ls --color=never -1 "${1}"/*.hs | head -n -1 | sort -r) ; do
    for tag in $TAG_CABALBUILDDEP \
               $TAG_DEF \
               $TAG_IMPORT \
               $TAG_KEYBIND \
               $TAG_KEYBINDLIST \
               $TAG_LAYOUT \
               $TAG_MOUSEBIND ; do

        ifs="$IFS"
        IFS=$'\n'
        tags=( $(sed -n -r -e "s/^.*--\s*${tag}\s//p" "${extension_srcfile}") )
        IFS="${ifs}"

        case $tag in
            $TAG_CABALBUILDDEP) ins_indent=$INS_INDENT_CABALBUILDDEP
                                ins_marker=$INS_MARKER_CABALBUILDDEP
                                ins_prefix=$INS_PREFIX_CABALBUILDDEP
                                ;;
            $TAG_DEF)           ins_indent=$INS_INDENT_DEF
                                ins_marker=$INS_MARKER_DEF
                                ins_prefix=$INS_PREFIX_DEF
                                ;;
            $TAG_IMPORT)        ins_indent=$INS_INDENT_IMPORT
                                ins_marker=$INS_MARKER_IMPORT
                                ins_prefix=$INS_PREFIX_IMPORT
                                ;;
            $TAG_KEYBIND)       ins_indent=$INS_INDENT_KEYBIND
                                ins_marker=$INS_MARKER_KEYBIND
                                ins_prefix=$INS_PREFIX_KEYBIND
                                ;;
            $TAG_KEYBINDLIST)   ins_indent=$INS_INDENT_KEYBINDLIST
                                ins_marker=$INS_MARKER_KEYBINDLIST
                                ins_prefix=$INS_PREFIX_KEYBINDLIST
                                ;;
            $TAG_LAYOUT)        ins_indent=$INS_INDENT_LAYOUT
                                ins_marker=$INS_MARKER_LAYOUT
                                ins_prefix=$INS_PREFIX_LAYOUT
                                ;;
            $TAG_MOUSEBIND)     ins_indent=$INS_INDENT_MOUSEBIND
                                ins_marker=$INS_MARKER_MOUSEBIND
                                ins_prefix=$INS_PREFIX_MOUSEBIND
                                ;;
        esac

        # Insert in reverse so values will ultimately appear in correct order.
        for i in $( seq $(( ${#tags[*]} - 1 )) -1 0 ) ; do
            [ -z "${tags[i]}" ] && continue
            if [[ $tag == $TAG_CABALBUILDDEP ]] ; then
                sed -i -r -e "s/${ins_marker}/\\0${ins_prefix}${tags[i]}/" "${CABAL_FILE_CONTRIB}"
            else
                sed -i -r -e "/${ins_marker}/{G;s/$/${ins_indent}${ins_prefix}${tags[i]}/;}" "${CONFIG_FILE_CONTRIB}"
            fi
        done

        if [[ $tag != $TAG_CABALBUILDDEP && -n "${tags}" ]] ; then
            ins_group_comment="${ins_indent}--   For extension $(basename $extension_srcfile .hs):"
            sed -i -r -e "/${ins_marker}/{G;s/$/${ins_group_comment}/;}" "${CONFIG_FILE_CONTRIB}"
        fi
    done
done
