#!/usr/bin/env python
"""
A simple tool for converting a Makefile to reStructuredText.

Basically it strips all comments starting at column 0 that don't start with
``##`` and converts all the rest to literal blocks.  The first comment line
defines the number of spaces after the comment sign to be ignored (this allows
``# text...`` which is more readable than ``#text...``).

"""

from __future__ import generators

import fileinput, sys

def convert(lines):
    """Generate lines of reST from makefile lines."""
    in_literal_block = False            # state
    comment_spaces = None               # stripped from all lines
    leading_spaces = 0                  # this/last line's indentation

    for line in lines:
        if line.isspace():
            # Empty line accepted in both states.
            if not in_literal_block:
                line = '#\n'
            else:
                line = '\n'
	if line[0] == '#' and not line[:2] == '##':
	    # Comment line
	    if in_literal_block:
		yield '\n'
		in_literal_block = False

	    line = line.expandtabs()[1:]
	    leading_spaces = len(line) - len(line.lstrip(' '))
	    if comment_spaces is None:
		comment_spaces = leading_spaces
	    else:
		line = line[min(leading_spaces, comment_spaces):]

            yield line
	else:
	    # Literal line
	    if not in_literal_block:
                yield '\n'
		yield '::\n'
		yield '\n'
		in_literal_block = True

	    yield '\t' + line

def main(*args):
    sys.stdout.writelines(convert(fileinput.input(args)))

if __name__ == '__main__':
    main(*sys.argv[1:])
