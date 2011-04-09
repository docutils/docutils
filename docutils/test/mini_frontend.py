#!/usr/bin/env python
# -*- coding: utf8 -*-
# :Copyright: © 2011 Günter Milde.
#             Released  without warranties or conditions of any kind
#             under the terms of the Apache License, Version 2.0
#             http://www.apache.org/licenses/LICENSE-2.0
# :Id: $Id$

"""
A minimal Docutils front-end for testing the command line
"""

# allow import also if Doctuils is not installed
import sys, os
testroot = os.path.abspath(os.path.dirname(__file__) or os.curdir)
sys.path.insert(0, os.path.normpath(os.path.join(testroot, '..')))
sys.path.append(os.path.normpath(os.path.join(testroot, '..', 'extras')))

from docutils.core import publish_cmdline

publish_cmdline(description='Generates pseudo-XML for testing purposes.')
