# $Id$
# Authors: David Goodger <goodger@python.org>;
#          Garth Kidd <garth@deadlybloodyserious.com>
# Copyright: This module has been placed in the public domain.

__docformat__ = 'reStructuredText'

import os
import sys

testroot = os.path.abspath(os.path.dirname(__file__) or os.curdir)
os.chdir(testroot)
sys.path.insert(0, os.path.normpath(os.path.join(testroot, '..')))
sys.path.insert(0, testroot)


def exception_data(func, *args, **kwds):
    """
    Execute `func(*args, **kwds)` and return the resulting exception, the
    exception arguments, and the formatted exception string.
    """
    try:
        func(*args, **kwds)
    except Exception as detail:
        return (detail, detail.args,
                '%s: %s' % (detail.__class__.__name__, detail))
    return None, [], "No exception"
