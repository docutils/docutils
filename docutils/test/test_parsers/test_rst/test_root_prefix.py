#! /usr/bin/env python3
# $Id$
# Author: Günter Milde
# Maintainer: docutils-develop@lists.sourceforge.net
# :Copyright: 2022 Günter Milde,
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""Test configurable root directory for included files

Cf. feature-requests #91.
"""

import os
from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).parents[3]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.utils import new_document

ROOT_PREFIX = (Path(__file__).parent/'test_directives').as_posix()
REL_PREFIX = os.path.relpath(ROOT_PREFIX).replace('\\', '/')


class ParserTestCase(unittest.TestCase):

    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.root_prefix = ROOT_PREFIX
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(output, case_expected)


totest = {}

totest['include-root'] = [
# locate included files using "include-root" setting value
["""\
.. include:: /include 11.rst
""",
"""\
<document source="test data">
    <paragraph>
        some text
"""],
# Do we want to support a leading backslash to signify a path root?
# ["""\
# .. include:: \include 11.rst
# """,
# """\
# <document source="test data">
#     <paragraph>
#         some text
# """],
["""\
.. include:: /include 11.rst
   :parser: rst
""",
"""\
<document source="test data">
    <paragraph>
        some text
"""],
["""\
.. include:: /include 11.rst
   :literal:
""",
f"""\
<document source="test data">
    <literal_block source="{REL_PREFIX}/include 11.rst" xml:space="preserve">
        some text
"""],
["""\
.. include:: /include 11.rst
   :code:
""",
f"""\
<document source="test data">
    <literal_block classes="code" source="{REL_PREFIX}/include 11.rst" xml:space="preserve">
        some text
"""],
# search included CSV files using "include-root" setting value
["""\
.. CSV-table::
   :file: /include 11.rst
""",
"""\
<document source="test data">
    <table>
        <tgroup cols="1">
            <colspec colwidth="100">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            some text
"""],
["""\
.. raw:: txt
   :file: /include 11.rst
""",
f"""\
<document source="test data">
    <raw format="txt" source="{REL_PREFIX}/include 11.rst" xml:space="preserve">
        some text
"""],
]


if __name__ == '__main__':
    unittest.main()
