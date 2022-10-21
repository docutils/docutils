#! /usr/bin/env python3
# $Id$
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
Tests for HTML blocks in CommonMark parsers
Cf. the `CommonMark Specification <https://spec.commonmark.org/>`__
"""

if __name__ == '__main__':
    import __init__  # noqa: F401
from test import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.RecommonmarkParserTestSuite()
    s.generateTests(totest)
    return s


totest = {}

totest['html_blocks'] = [
["""\
A paragraph:

<p>A HTML block.</p>
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <raw format="html" xml:space="preserve">
        <p>A HTML block.</p>
"""],
["""\
<DIV CLASS="foo">

*Markdown*

</DIV>
""",
"""\
<document source="test data">
    <raw format="html" xml:space="preserve">
        <DIV CLASS="foo">
    <paragraph>
        <emphasis>
            Markdown
    <raw format="html" xml:space="preserve">
        </DIV>
"""],
["""\
<a href="foo">
*bar*
</a>
""",
"""\
<document source="test data">
    <raw format="html" xml:space="preserve">
        <a href="foo">
        *bar*
        </a>
"""],
# In recommonmark 0.7.0, some raw blocks at paragraph start make the
# paragraph a raw block :(
# ["""\
# <!-- foo -->*bar* (raw because of the comment tag at start of paragraph)
# *baz*
# """,
# """\
# <document source="test data">
#     <paragraph>
#         <raw format="html" xml:space="preserve">
#             <!-- foo -->
#         <emphasis>
#             bar
#     <paragraph>
#         <emphasis>
#             baz
# """],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
