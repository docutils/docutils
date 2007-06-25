#!/usr/bin/env python

# Author: Guenter Milde
# Contact: 
# Revision: $Revision$
# Date: $Date$
# Copyright: Licensed under the Academic Free License version 1.2

# Hint: run this with the "Examples of Syntax Constructs" in the
# `reStructuredText Test Document`__
#
# __ file:/home/milde/.python/docutils-svn/docutils/test/functional/input/standalone_rst_latex2e.txt

"""
Front end to the Docutils Publisher, producing readable LaTeX2e.

Variant with external style sheet 
"""

# prepend parent dir to the PYTHONPATH
import sys, os.path
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))


try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description

# import the writer variant from the current dir
from latex2e_adaptive_preamble import Writer

description = ('Experimental "latex2e" writer variant'
               + default_description)

publish_cmdline(writer=Writer(), description=description)
