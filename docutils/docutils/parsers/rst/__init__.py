#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

This is ``the docutils.parsers.restructuredtext`` package. It exports a single
class, `Parser`.

Usage
=====

1. Create a parser::

       parser = docutils.parsers.restructuredtext.Parser()

   Several optional arguments may be passed to modify the parser's behavior.
   Please see `docutils.parsers.Parser` for details.

2. Gather input (a multi-line string), by reading a file or the standard
   input::

       input = sys.stdin.read()

3. Create a new empty `docutils.nodes.document` tree::

       docroot = docutils.utils.newdocument()

   See `docutils.utils.newdocument()` for parameter details.

4. Run the parser, populating the document tree::

       document = parser.parse(input, docroot)

Parser Overview
===============

The reStructuredText parser is implemented as a state machine, examining its
input one line at a time. To understand how the parser works, please first
become familiar with the `docutils.statemachine` module, then see the
`states` module.
"""

__docformat__ = 'reStructuredText'


import docutils.parsers
import docutils.statemachine
import states


class Parser(docutils.parsers.Parser):

    """The reStructuredText parser."""

    def parse(self, inputstring, docroot):
        """Parse `inputstring` and populate `docroot`, a document tree."""
        self.setup_parse(inputstring, docroot)
        debug = docroot.reporter[''].debug
        self.statemachine = states.RSTStateMachine(
              stateclasses=states.stateclasses, initialstate='Body',
              debug=debug)
        inputlines = docutils.statemachine.string2lines(
              inputstring, convertwhitespace=1)
        self.statemachine.run(inputlines, docroot)
