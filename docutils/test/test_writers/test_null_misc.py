#! /usr/bin/env python

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Miscellaneous Null writer tests.
"""

from __init__ import DocutilsTestSupport
from docutils.writers import null


class SupportsTestCase(DocutilsTestSupport.StandardTestCase):

    def test_xmlcharrefreplace(self):
        self.assert_(null.Writer().supports('anyformat'))


if __name__ == '__main__':
    import unittest
    unittest.main()
