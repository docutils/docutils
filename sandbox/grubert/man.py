#!/usr/bin/env python

# Author: 
# Contact: grubert@users.sf.net
# Copyright: This module has been placed in the public domain.

"""
man.py
======

This module provides a simple command line interface that uses the
man page writer to output from ReStructuredText source.
"""

import locale
try:
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description
from man import ManPageWriter

description = ("Generates plain man.  " + default_description)

publish_cmdline(writer=ManPageWriter.Writer(), description=description)
