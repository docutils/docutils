#!/usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

A minimal front-end to the Docutils Publisher, producing HTML.
"""

import locale
locale.setlocale(locale.LC_ALL, '')

from docutils.core import publish


usage = '%prog [options] [source [destination]]'
description = ('Generate HTML documents from standalone reStructuredText '
               'sources.')

publish(writer_name='html', usage=usage, description=description)
