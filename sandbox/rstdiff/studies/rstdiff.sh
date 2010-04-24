#!/bin/sh

set -u

##############################################################################
##############################################################################
# Some functions

# Does verbose output if wanted.
VrbO () {
  if [ "${verb:-}" ] ; then
    for vrbI
    do
      ErrO "## $vrbI"
    done
  fi
}

##############################################################################

# Outputs error message $1 if present or standard input.
ErrO () {
  if [ "$#" -gt 0 ] ; then
    echo "$argv0: $*" >&2
  else
    (
      while IFS= read errLn ; do
        echo "$argv0: $errLn" >&2
      done
    )
  fi
}

##############################################################################

# Outputs error message $2 (or standard input) and exits with code $1.
ErrEx () {
  exCode="$1"
  shift
  ErrO ${@:+"$@"}
  exit $exCode
}

##############################################################################

# Removes temporary files.
ExRm () {
  exRmF="`expr "${exRmF:-}" : ' *\(.*\) *$'`" # ` Pacify Emacs
  [ ! "$exRmF" ] ||
      rm -f $exRmF
}

##############################################################################

# This function is executed on exit.
OnEx () {
  exCode="${exCode:-$?}"
  ExRm
  trap $exTraps
  exit $exCode
}
exCode=
exTraps="0 1 2 15"
trap OnEx $exTraps

##############################################################################
##############################################################################

# Create a group format option for `diff'. `gtype' is a group type of `diff'
# (one of `old' `new' `unchanged' `changed'). `gfmt' is the format specifier to
# use for the lines (one of `<' `>' `=').
GroupOpt () {
  gtype="$1"
  gfmt="$2"
  echo "--$gtype-group-format=.. class:: CHANGE-$gtype%c'\012'%c'\012'%$gfmt%c'\012'.. endclass CHANGE-$gtype%c'\012'%c'\012'"
}

##############################################################################

# Create a line format option for `diff'. `ltype' is a line type of `diff'
# (one of `old' `new' `unchanged').
LineOpt () {
  ltype="$1"
  echo "--$ltype-line-format=   %l%c'\012'"
}

##############################################################################
##############################################################################
# Handle arguments

# Get name of this script
argv0="`basename $0`"

# A unique temporary file
tmpF="/tmp/$argv0.$$"
exRmF="$tmpF"

# Initialization of flags
verb=
oldSvnRev=
oldF=

# Options and usage
getoptS="vr:f:"
usage="Usage: $argv0 -s <old-svn-rev>|-f <old-file> <new-file>"

# Get flags
while getopts "$getoptS" cOpt ; do
  case "$cOpt" in
  v ) verb=1 ;;
  s ) oldSvnRev="$OPTARG" ;;
  f ) oldF="$OPTARG" ;;
  * ) ErrEx 1 "$usage" ;;
  esac
done
shift `expr $OPTIND - 1`

[ "$oldSvnRev" -a "$oldF" ] &&
    ErrEx 1 "$usage"
[ $# -eq 1 ] ||
    ErrEx 1 "$usage"
newF="$1"
shift

##############################################################################
# Now work

if [ "$oldSvnRev" ] ; then
  svn cat -r "$oldSvnRev" "$newF"
else
  cat "$oldF"
fi |
    diff --ignore-tab-expansion --ignore-blank-lines --ignore-all-space "`GroupOpt old '<'`" "`LineOpt old`" "`GroupOpt new '>'`" "`LineOpt new`" - "$newF"
