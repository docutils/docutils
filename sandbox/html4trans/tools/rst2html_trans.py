#!/usr/bin/env python

# Author: Guenter Milde
# Contact: 
# Revision: $Revision$
# Date: $Date$
# Copyright: Licensed under the Academic Free License version 1.2

"""
Front end to the Docutils Publisher, producing HTML not requiring
a CSS stylesheet.
"""

import sys
try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description

# Prepend parent dir to the PYTHONPATH
# (This is a hack to get rst2html_trans.py working without install,
#  not needed if the html4trans.py module is installed in the PYTHONPATH)
sys.path.insert(0, '..')

from html4trans import Writer

description = ('Generates (X)HTML documents from standalone reStructuredText '
               'sources that do not require a CSS stylesheet.  ' 
               + default_description)

# no CSS stylesheet needed, so do not include (referencing does not harm)
overrides = {'embed_stylesheet': False}

publish_cmdline(writer=Writer(), 
                settings_overrides=overrides,
                description=description)
