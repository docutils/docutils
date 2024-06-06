#!/usr/bin/env python3
# :Copyright: © 2024 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""Various tests for the XML parser.

Test parsing + transformations with `publish_string()`.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))

from docutils.core import publish_string
from docutils.parsers import docutils_xml

parser = docutils_xml.Parser()


class XMLParserTests(unittest.TestCase):
    maxDiff = None

    mysettings = {'_disable_config': True,
                  'output_encoding': 'unicode',
                  'warning_stream': '',
                  }

    def test_publish(self):
        for name, (settings, cases) in totest.items():
            settings = self.mysettings | settings
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    output = publish_string(case_input, parser=parser,
                                            settings_overrides=settings)
                    self.assertEqual(case_expected, output)


totest = {}

totest['decoration'] = ({'datestamp': 'pi-day'},  # generate footer
[
["""\
<decoration>
    <footer>
        <paragraph>myfooter</paragraph>
    </footer>
</decoration>
""",
"""\
<document source="<string>">
    <decoration>
        <footer>
            <paragraph>
                myfooter
            <paragraph>
                Generated on: pi-day.
"""],
])

totest['hyperlinks'] = ({},  # resolve hyperlinks
[
["""\
<document source="test data">
    <paragraph>A <reference anonymous="1" name="link">link</reference> to Docutils.</paragraph>
    <target anonymous="1" ids="target-1" refuri="http://docutils.sourceforge.io"/>
</document>
""",
"""\
<document source="test data">
    <paragraph>
        A \n\
        <reference anonymous="1" name="link" refuri="http://docutils.sourceforge.io">
            link
         to Docutils.
    <target anonymous="1" ids="target-1" refuri="http://docutils.sourceforge.io">
"""],
# duplicate ids are an error
["""\
<tip ids="i1 i2">
    <paragraph><strong ids="i2 i3"></strong></paragraph>
</tip>
""",
"""\
<document source="<string>">
    <tip ids="i1 i2">
        <paragraph>
            <strong ids="i2 i3">
    <section classes="system-messages">
        <title>
            Docutils System Messages
        <system_message level="3" line="2" source="<string>" type="ERROR">
            <paragraph>
                Duplicate ID: "i2" used by <tip ids="i1 i2"> and <strong ids="i2 i3">
"""],
])


if __name__ == '__main__':
    unittest.main()
