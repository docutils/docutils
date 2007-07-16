# -*- coding: utf-8 -*-
"""
    sphinx.jinja
    ~~~~~~~~~~~~

    Jinja glue.

    :copyright: 2007 by Georg Brandl.
    :license: Python license.
"""
from __future__ import absolute_import

import sys
from os import path

sys.path.insert(0, path.join(path.dirname(__file__), 'jinja'))

from jinja import Environment, FileSystemLoader
