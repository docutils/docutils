#!/usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

A minimal front-end to the Docutils Publisher, producing Docutils XML.
"""

import locale
locale.setlocale(locale.LC_ALL, '')

from docutils.core import publish


usage = '%prog [options] [source [destination]]'
description = ('Generate Docutils XML from standalone reStructuredText '
               'sources.')

publish(writer_name='xml', usage=usage, description=description)
