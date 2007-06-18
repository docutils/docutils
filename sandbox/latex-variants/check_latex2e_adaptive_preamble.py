#!/usr/bin/env python

# Author: Guenter Milde
# Contact: 
# Revision: $Revision$
# Date: $Date$
# Copyright: Licensed under the Academic Free License version 1.2

"""
Test script for the "latex2e_adaptive_preamble" writer
"""

from docutils.core import publish_string, publish_file

from latex2e_adaptive_preamble import Writer

title = """\
Title of the Document
=====================

Subtitle
-------------
"""

bibliographic_list = """\

:Author: G\. E\. Milde # escape fullstop to prevent recognision as enumeration
:Status: Work in progress
:field name: genric content

             in several paragraphs

:Dedication:

    For impatient owners of slow computers.

:abstract:

    This is a test document, containing at least one example of each
    reStructuredText construct.

"""             

table_of_contents = '\n.. contents::\n'

admonitions = """

Admonitions
-----------

.. admonition:: testme

   Admonition text,
   
   in two paragraphs.

.. note::

   This is a note

.. warning::

   Beware of red tape.
"""

literal_block = """

Literal Blocks
--------------

Literal blocks are indicated with a double-colon ("::") at the end of
the preceding paragraph (over there ``-->``).  They can be indented::

    if literal_block:
        text = 'is left as-is'
        spaces_and_linebreaks = 'are preserved'
        markup_processing = None

Or they can be quoted without indentation::

>> Great idea!
>
> Why didn't I think of that?

"""

line_block = """

Line Blocks
-----------

This section tests line blocks.  Line blocks are body elements which
consist of lines and other line blocks.  Nested line blocks cause
indentation.

| This is a line block.  It ends with a blank line.
|     New lines begin with a vertical bar ("|").
|     Line breaks and initial indent are significant, and preserved.
|         Continuation lines are also possible.  A long line that is intended
          to wrap should begin with a space in place of the vertical bar.
|     The left edge of a continuation line need not be aligned with
  the left edge of the text above it.

| This is a second line block.
|
| Blank lines are permitted internally, but they must begin with a "|".

"""

table = """

=== ===
ABC DEF
--- ---

 23   5
  7  99

=== ===

""" 


system_message = """

this link should `trigger a system message`_ as it has no defined target.

"""

# Sample input text::

internal_samples = "".join([
                            title,
                            bibliographic_list,
                            table_of_contents,
                            # admonitions,
                            literal_block,
                            # line_block,
                            table,
                            # system_message
                           ])

# samples from the docutils svn data
syntax_samples_dir = '../../docutils/test/functional/input/'
syntax_sample_files = ['data/standard.txt',
                       'data/table_colspan.txt',
                       'data/table_rowspan.txt',
                       # Tests for the LaTeX writer
                       'data/latex2e.txt',
                       'data/nonalphanumeric.txt',
                       'data/unicode.txt',
                       'data/custom_roles.txt',
                       # 'data/errors.txt'
                      ]
    
syntax_samples = '\n'.join([open(syntax_samples_dir+samplefile).read()
                          for samplefile in syntax_sample_files])


# Convert and Print
# -----------------

# Uncomment one of the following:

# document tree (as pseudoxml rendering) for comparision
# doctree = publish_string(sample, writer_name="pseudoxml")
# output = publish_string(internal_samples, writer=Writer())

output = publish_string(syntax_samples, writer=Writer())

# # Replace image links
output = output.replace('../../../docs/user/rst/images/',
                        '../../../docutils/docs/user/rst/images/')


# print doctree # document tree (as pseudoxml rendering) for comparision
print output  # * html4trans output


# save output to data sub-dir
outfile = open("data/latex2e-adaptive-preamble-sample.tex", 'w')
outfile.write(output)



