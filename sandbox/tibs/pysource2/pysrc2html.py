#!/usr/bin/env python
"""pysrc2html - Read Python package/modules and output HTML documentation

@@@ I'm not terribly happy with the name of this module, but it will do for
now (pydoc2html *might* be better?)

:Author:    Tibs
:Contact:   tibs@tibsnjoan.co.uk
:Revision:  $Revision$
:Date:      $Date$
:Copyright: This module has been placed in the public domain.
"""

__docformat__ = 'reStructuredText'

import sys
from package import parse_package_or_module
import transform
from docutils.writers.html4css1 import Writer
from docutils.frontend import OptionParser

usage = '%prog [options] [<package-directory> | <python-file> [html-file]]'
description = ('Generates .html documentation for the given Python package'
               ' or module.')

writer = Writer()

option_parser = OptionParser(components=[writer],
                             usage=usage,description=description)

settings = option_parser.parse_args(sys.argv[1:])

source_path = settings._source
target_path = settings._destination

nodes = parse_package_or_module(source_path)

# That then needs converting to a docutils tree
document = transform.make_document(nodes,settings)

# And *that* wants converting to the appropriate output format
try:
    target = open(target_path,"w")
    writer.write(document,target)
finally:
    target.close()
