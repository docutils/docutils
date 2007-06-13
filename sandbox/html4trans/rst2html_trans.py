#!/usr/bin/env python

# Author: Guenter Milde
# Contact: 
# Revision: $Revision$
# Date: $Date$
# Copyright: Licensed under the Academic Free License version 1.2

# Hint: run this with the "Examples of Syntax Constructs" in the
# `reStructuredText Test Document`__ via test-syntax-examples
#
# __ file:/usr/src/docutils-svn/docutils/test/functional/input/standalone_rst_html4css1.txt

"""
Front end to the Docutils Publisher, producing HTML not requiring
a CSS stylesheet.
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description
import html4trans 

description = ('Generates (X)HTML documents from standalone reStructuredText '
               'sources that do not require a CSS stylesheet.  ' 
               + default_description)

# no CSS stylesheet needed, so do not include (referencing does not harm)
overrides = {'embed_stylesheet': False}

publish_cmdline(writer=html4trans.Writer(), 
                settings_overrides=overrides,
                description=description)
