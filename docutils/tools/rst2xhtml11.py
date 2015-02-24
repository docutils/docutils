#!/usr/bin/env python
# -*- coding: utf8 -*-
# :Copyright: © 2005, 2009 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: http://www.spdx.org/licenses/BSD-2-Clause
#
# Revision: $Revision$
# Date: $Date$

"""
A minimal front end to the Docutils Publisher, producing valid XHTML 1.1
"""

try:
    import locale # module missing in Jython
    locale.setlocale(locale.LC_ALL, '')
except locale.Error:
    pass

from docutils.core import publish_cmdline, default_description

description = ('Generates CSS2-styled HTML documents from standalone '
               'reStructuredText! sources that conform to the XHTML 1.1 DTD '
               '(strict XHTML). '
               + default_description)

publish_cmdline(writer_name='xhtml11', description=description)
