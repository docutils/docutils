#!/usr/bin/env python

# Author: Guenter Milde
# Contact: 
# Revision: $Revision$
# Date: $Date$
# Copyright: Licensed under the Academic Free License version 1.2

# Hint: run this with the "Examples of Syntax Constructs" in the
# `reStructuredText Test Document`__
#
# __ file:/usr/src/docutils-svn/docutils/test/functional/input/standalone_rst_latex2e.txt

"""
Front end to the Docutils Publisher, producing readable LaTeX2e.

Variant with a class for "intelligent" latex package management
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description
import latex2e_adaptive_preamble

description = ('Experimental "latex2e" writer variant'
               + default_description)

publish_cmdline(writer=latex2e_adaptive_preamble.Writer(), 
                description=description)
