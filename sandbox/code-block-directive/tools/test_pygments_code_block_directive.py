#!/usr/bin/env python
# -*- coding: iso-8859-1 -*-

# Test the parsing and formatting by pygments:

# :Author: Felix Wiemann; Guenter Milde
# :Date: $Date$
# :Copyright: This module has been placed in the public domain.

# Requirements
# ------------

from docutils import nodes, utils, core
from pygments_code_block_directive import DocutilsInterface

# Test data
# ---------

code_sample = """\
def my_function():
    "just a test"
    print 8/2
"""

language = "python"

# Do not insert inline nodes for the following tokens.
# (You could add e.g. Token.Punctuation like ``['', 'p']``.) ::
unstyled_tokens = ['']

# Set up a document tree
# ----------------------

document = utils.new_document('generated')
literal_block = nodes.literal_block(classes=["code-block", language])
document += literal_block


# Parse code and fill the <literal_block>
# ----------------------------------------

for cls, value in DocutilsInterface(code_sample, language):
    if cls in unstyled_tokens:
        # insert as Text to decrease the verbosity of the output.
        node = nodes.Text(value, value)
    else:
        node = nodes.inline(value, value, classes=[cls])
    literal_block += node

# Write
# -----

writer_names = ('html', 'pseudoxml', 'xml', 'latex', 'newlatex2e', 's5')
for name in writer_names[2:3]:
    print "\nusing writer %r\n" % name
    print core.publish_from_doctree(document, writer_name=name)

