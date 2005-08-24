#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Miscellaneous HTML writer tests.
"""

from __init__ import DocutilsTestSupport
from docutils import core


class EncodingTestCase(DocutilsTestSupport.StandardTestCase):

    def test_xmlcharrefreplace(self):
        # Test that xmlcharrefreplace is the default output encoding
        # error handler.
        settings_overrides={
            'output_encoding': 'latin1',
            'stylesheet': '',
            '_disable_config': 1,}
        result = core.publish_string(
            'äöü€', writer_name='html4css1',
            settings_overrides=settings_overrides)
        self.assert_(result.find('\xe4\xf6\xfc&#8364;') != -1)


if __name__ == '__main__':
    import unittest
    unittest.main()
