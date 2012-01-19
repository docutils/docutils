#! /usr/bin/env python

# $Id$
# Author: Lea Wiemann
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""
Miscellaneous HTML writer tests.
"""

from __init__ import DocutilsTestSupport
from docutils import core
from docutils._compat import b


class EncodingTestCase(DocutilsTestSupport.StandardTestCase):

    def test_xmlcharrefreplace(self):
        # Test that xmlcharrefreplace is the default output encoding
        # error handler.
        settings_overrides={
            'output_encoding': 'latin1',
            'stylesheet': '',
            '_disable_config': True,}
        result = core.publish_string(
            u'EUR = \u20ac', writer_name='html4css1',
            settings_overrides=settings_overrides)
        # Encoding a euro sign with latin1 doesn't work, so the
        # xmlcharrefreplace handler is used.
        self.assertNotEqual(result.find(b('EUR = &#8364;')), -1)


if __name__ == '__main__':
    import unittest
    unittest.main()
