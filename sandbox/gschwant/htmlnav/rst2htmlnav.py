#!/usr/bin/env python

# Author: Gunnar Schwant
# Contact: g.schwant@gmx.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

import locale
try:
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description

description = (default_description)

publish_cmdline(writer_name='htmlnav', description=description)
