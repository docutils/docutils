#!/usr/bin/env python3
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

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))


from docutils.frontend import get_default_settings
from docutils.parsers.recommonmark_wrapper import Parser
from docutils.utils import new_document


class RecommonmarkParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


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
        <reference refuri="http://www.python.org/">
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
        <reference name="targets" refuri="first%20wins">
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
    <section dupnames="title" ids="title-1">
        <title>
            Title
        <system_message backrefs="title-1" level="1" line="8" source="test data" type="INFO">
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
    unittest.main()
