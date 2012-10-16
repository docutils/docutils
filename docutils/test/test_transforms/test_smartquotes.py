#!/usr/bin/env python
# -*- coding: utf8 -*-

# $Id$

# :Copyright: © 2011 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: http://www.spdx.org/licenses/BSD-2-Clause

"""
Test module for universal.SmartQuotes transform.
"""


from __init__ import DocutilsTestSupport # must be imported before docutils
from docutils.transforms.universal import SmartQuotes
from docutils.parsers.rst import Parser

def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(
        parser, suite_settings={'smart_quotes': True})
    s.generateTests(totest)
    return s


totest = {}

totest['transitions'] = ((SmartQuotes,), [
["""\
Test "smart quotes", 'single smart quotes'
-- and ---also long--- dashes.
""",
u"""\
<document source="test data">
    <paragraph>
        Test “smart quotes”, ‘single smart quotes’
        – and —also long— dashes.
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
