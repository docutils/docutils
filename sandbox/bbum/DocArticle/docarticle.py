#!/usr/bin/env python

# Author: Bill Bumgarner
# Contact: bbum@codefab.com

import locale
try:
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description
from DocArticle import DocArticleWriter

description = ("Generates O'Reilly article compatible xHTML from ReST "
               "sources.  " + default_description)

publish_cmdline(writer=DocArticleWriter(), description=description)
