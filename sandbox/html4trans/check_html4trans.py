#!/usr/bin/env python

# Author: Guenter Milde
# Contact: 
# Revision: $Revision$
# Date: $Date$
# Copyright: Licensed under the Academic Free License version 1.2

"""
Test script for the "html4trans" writer, producing HTML not requiring
a CSS stylesheet.

This is not a proper unit test but a script for interactive checking
and modifying just to get a picture of the working
"""

from docutils.core import publish_string, publish_file

from html4trans import Writer

# RST text samples
# ================

title = """\
Title of the Document
=====================

Subtitle
-------------

First section
'''''''''''''

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

system_message = """

this link should `trigger a system message`_ as it has no defined target.

"""


# no CSS stylesheet needed, so do not include (referencing does not harm)
overrides = {'embed_stylesheet': False}


def publish_some_samples():
    # Sample input text::
    sample = "".join([
                      title,
                      bibliographic_list,
                      admonitions,
                      literal_block,
                      # line_block,
                      system_message
                     ])
    
    doctree = publish_string(sample, writer_name="pseudoxml")

    output = publish_string(sample, 
                            writer=Writer(),
                            settings_overrides=overrides)
    return (doctree, output)


def publish_syntax_samples():
    """Convert the full sample list from the standalone_rst_html4css1.txt file"""
    
    syntax_samples_path = \
        '../../docutils/test/functional/input/standalone_rst_html4css1.txt'

    doctree = '' #publish_string(open(syntax_samples_path), 
                 #            writer_name="pseudoxml")
    
    output = publish_file(open(syntax_samples_path),
                          writer=Writer(),
                          settings_overrides=overrides)
    
    # Replace image links
    output = output.replace('../../../docs/user/rst/images/',
                            '../../../docutils/docs/user/rst/images/')
    return (doctree, output)


# Convert and print
# -----------------

# Uncomment one of the following:

(doctree, output) = publish_some_samples()
#(doctree, output) = publish_syntax_samples()

# * document tree (as pseudoxml rendering) for comparision
print doctree

# * html4trans output
print output

# save output to data sub-dir
outfile = open("data/html4trans-sample.html", 'w')
outfile.write(output)



