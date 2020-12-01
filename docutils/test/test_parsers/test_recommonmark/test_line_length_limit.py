#!/usr/bin/env python3
# -*- coding: utf8 -*-
# :Copyright: © 2020 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""
Tests for inline markup in docutils/parsers/rst/states.py.
Interpreted text tests are in a separate module, test_interpreted.py.
"""
from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_parsers import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.RecommonmarkParserTestSuite(
            suite_settings={'line_length_limit': 80})
    s.generateTests(totest)
    return s

totest = {}

totest['default'] = [
["""\
within the limit
%s
""" % ("x"*80),
"""\
<document source="test data">
    <paragraph>
        within the limit
        %s
""" % ("x"*80)],
["""\
above the limit
%s
""" % ("x"*81),
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Line 2 exceeds the line-length-limit.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
