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
Test for targets in CommonMark parsers.
Cf. the `CommonMark Specification <https://spec.commonmark.org/>`__
"""

from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_parsers import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.RecommonmarkParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['targets'] = [
[r"""
External hyperlink [target]s:

[target]: http://www.python.org/
""",
"""\
<document source="test data">
    <paragraph>
        External hyperlink \n\
        <reference name="target" refuri="http://www.python.org/">
            target
        s:
"""],
["""\
Indirect hyperlink [target]s:

[target]: target2

[target2]: /url
""",
"""\
<document source="test data">
    <paragraph>
        Indirect hyperlink \n\
        <reference name="target" refuri="target2">
            target
        s:
"""],
["""\
Duplicate external [targets] (different URIs):

[targets]: <first wins>
[targets]: second
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate external \n\
        <reference name="targets" refuri="first wins">
            targets
         (different URIs):
"""],
["""\
Duplicate external [targets] (same URIs):

[targets]: spam
[targets]: spam
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate external \n\
        <reference name="targets" refuri="spam">
            targets
         (same URIs):
"""],
["""\
Duplicate implicit targets.

Title
=====

Paragraph.

Title
=====

Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate implicit targets.
    <section dupnames="title" ids="title">
        <title>
            Title
        <paragraph>
            Paragraph.
    <section dupnames="title" ids="id1">
        <title>
            Title
        <system_message backrefs="id1" level="1" line="8" source="test data" type="INFO">
            <paragraph>
                Duplicate implicit target name: "title".
        <paragraph>
            Paragraph.
"""],
["""\
Duplicate implicit/explicit targets.

Title
=====

[title]: hoppla

Paragraph with link to [title].
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate implicit/explicit targets.
    <section ids="title" names="title">
        <title>
            Title
        <paragraph>
            Paragraph with link to \n\
            <reference name="title" refuri="hoppla">
                title
            .
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
