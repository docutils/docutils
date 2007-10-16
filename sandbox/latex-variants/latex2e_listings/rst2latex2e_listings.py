#!/usr/bin/env python

# :Author: Guenter Milde
# :Contact: 
# :Revision: $Revision$
# :Date: $Date$
# :Copyright: Licensed under the Academic Free License version 1.2

"""
Front end to the Docutils Publisher, producing LaTeX2e.

Variant using using the `listings` package for literal blocks.
"""

# # prepend parent dir to the PYTHONPATH
# import sys, os.path
# sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description

# import the writer variant
from latex2e_listings import Writer

description = ('Experimental "latex2e" writer variant'
               + default_description)

publish_cmdline(writer=Writer(), description=description)
