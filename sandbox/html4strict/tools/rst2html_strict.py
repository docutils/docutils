#!/usr/bin/env python

# Author: Guenter Milde
# Contact: 
# Revision: $Revision$
# Date: $Date$
# Copyright: Licensed under the Academic Free License version 1.2

"""
A minimal front end to the Docutils Publisher, producing HTML relying on
a css stylesheet.
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description

# Prepend parent dir to the PYTHONPATH
# (This is a hack to get rst2html_trans.py working without install,
#  not needed if the html4trans.py module is installed in the PYTHONPATH)
import sys
sys.path.insert(0, '..')

from html4strict import Writer

description = ('Generates (X)HTML documents from standalone reStructuredText '
               'sources that fully rely on a CSS2 stylesheet '
               'and conforms to the XHTML version 1.0 Strict DTD. '
               + default_description)

publish_cmdline(writer=Writer(), 
                description=description)
