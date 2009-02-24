#!/usr/bin/python

# rst2latex-highlight
# ===================
# 
# Docutils front-end with syntax highlight.
# 
# :Author: David Goodger, a Pygments author|contributor, Guenter Milde
# :Date: $Date: 2008-05-22 08:42:52 +0200 (Do, 22. Mai 2008) $
# :Copyright: This module has been placed in the public domain.
# 
# This is a merge of the docutils_ `rst2latex` front end with an extension
# suggestion taken from the Pygments_ documentation.
# 
# ::

"""
A front end to docutils, producing LaTeX with syntax colouring using pygments

Generates LaTeX documents from standalone reStructuredText sources. Uses the
`Pygments` syntax highlighter to parse and mark up the content of ``..
code-block::` directives. Needs an adapted stylesheet.
"""

# Requirements
# ------------
# 
# ::

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description

# `<pygments_code_block_directive.py>`_ defines and registers a new
# directive `code-block` that uses the `Pygments`_ syntax highlighter to
# render code in color::

import pygments_code_block_directive

# Call the docutils publisher to render the input as latex::

description = __doc__ + default_description
publish_cmdline(writer_name='latex2e', description=description)


# .. References:
# .. _docutils: http://docutils.sf.net/
# .. _pygments: http://pygments.org/
# .. _Using Pygments in ReST documents: http://pygments.org/docs/rstdirective/
