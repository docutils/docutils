# $Id$
# Authors: David Goodger <goodger@python.org>;
#          Garth Kidd <garth@deadlybloodyserious.com>
# Copyright: This module has been placed in the public domain.

__docformat__ = 'reStructuredText'

import os
import sys

testroot = os.path.abspath(os.path.dirname(__file__))
sys.path.insert(0, os.path.dirname(testroot))
sys.path.insert(0, testroot)
