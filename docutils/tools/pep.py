#!/usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

A minimal front-end to the Docutils Publisher, producing HTML from PEP
(Python Enhancement Proposal) documents.
"""

import locale
locale.setlocale(locale.LC_ALL, '')

from docutils.core import publish


usage = 'usage:\n  %prog [options] [source [destination]]'

publish(reader_name='pep', writer_name='pep_html', usage=usage)
