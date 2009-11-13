#!/usr/bin/env python
# -*- coding: utf8 -*-
# :Copyright: © 2005, 2009 Günter Milde.
#             Released  without warranties or conditions of any kind
#             under the terms of the Apache License, Version 2.0
# 	      http://www.apache.org/licenses/LICENSE-2.0
# Revision: $Revision$
# Date: $Date$

"""
A minimal front end to the Docutils Publisher, producing (X)HTML relying on
a css stylesheet.
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description

# Import the html4strict writer from either the canonical place for a
# Docutils writer or anywhere in the PYTHONPATH::

try:
    from docutils.writers.html4strict import Writer
except ImportError:
    from html4strict import Writer

description = ('Generates (X)HTML documents from standalone reStructuredText '
               'sources that fully rely on a CSS2 stylesheet '
               'and conforms to the XHTML version 1.0 Strict DTD. '
               + default_description)

publish_cmdline(writer=Writer(), 
                description=description)
