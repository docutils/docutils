# -*- coding: utf-8 -*-
#
# Python documentation build configuration file
#

import time

# The default replacements for |version|, |release| and |today|.
version = '2.6'
release = '2.6a0'
today = time.strftime('%B %d, %Y')

# List of files that shouldn't be included in the build.
unused_files = [
    'whatsnew/2.0.rst',
    'whatsnew/2.1.rst',
    'whatsnew/2.2.rst',
    'whatsnew/2.3.rst',
    'whatsnew/2.4.rst',
    'whatsnew/2.5.rst',
    'macmodules/scrap.rst',
    'modules/xmllib.rst',
]

# If not '', a 'Last updated on:' timestamp is inserted at every page bottom,
# using the given strftime format.
last_updated_format = '%b %d, %Y'

# If true, SmartyPants will be used to convert quotes and dashes to
# typographically correct entities.
use_smartypants = False

# If true, trailing '()' will be stripped from :func: etc. cross-references.
strip_trailing_parentheses = False
