#!/usr/bin/env python
# -*- coding: utf8 -*-
# :Copyright: © 2020 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause
#
# Revision: $Revision$
# Date: $Date$

"""
A minimal front end to the Docutils Publisher, parsing CommonMark markdown files
with `recommonmark` and producing HTML 5 documents.

The output is also valid XML.
"""

try:
    import locale # module missing in Jython
    locale.setlocale(locale.LC_ALL, '')
except locale.Error:
    pass

from docutils.core import publish_cmdline, default_description


description = (u'Generate HTML5 documents from standalone '
               u'Markdown (CommonMark) sources.\n'
               + default_description)

publish_cmdline(#parser=mdparser, 
                parser_name="recommonmark",
                writer_name='html5',
                description=description)
