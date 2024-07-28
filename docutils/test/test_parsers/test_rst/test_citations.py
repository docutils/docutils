#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for states.py.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.utils import new_document


class ParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['citations'] = [
["""\
.. [citation] This is a citation.
""",
"""\
<document source="test data">
    <citation ids="citation" names="citation">
        <label>
            citation
        <paragraph>
            This is a citation.
"""],
["""\
.. [citation1234] This is a citation with year.
""",
"""\
<document source="test data">
    <citation ids="citation1234" names="citation1234">
        <label>
            citation1234
        <paragraph>
            This is a citation with year.
"""],
["""\
.. [citation] This is a citation
   on multiple lines.
""",
"""\
<document source="test data">
    <citation ids="citation" names="citation">
        <label>
            citation
        <paragraph>
            This is a citation
            on multiple lines.
"""],
["""\
.. [citation1] This is a citation
     on multiple lines with more space.

.. [citation2] This is a citation
  on multiple lines with less space.
""",
"""\
<document source="test data">
    <citation ids="citation1" names="citation1">
        <label>
            citation1
        <paragraph>
            This is a citation
            on multiple lines with more space.
    <citation ids="citation2" names="citation2">
        <label>
            citation2
        <paragraph>
            This is a citation
            on multiple lines with less space.
"""],
["""\
.. [citation]
   This is a citation on multiple lines
   whose block starts on line 2.
""",
"""\
<document source="test data">
    <citation ids="citation" names="citation">
        <label>
            citation
        <paragraph>
            This is a citation on multiple lines
            whose block starts on line 2.
"""],
["""\
.. [citation]

That was an empty citation.
""",
"""\
<document source="test data">
    <citation ids="citation" names="citation">
        <label>
            citation
        <system_message level="2" line="2" source="test data" type="WARNING">
            <paragraph>
                Citation content expected.
    <paragraph>
        That was an empty citation.
"""],
["""\
.. [citation] The Source
No blank line.
""",
"""\
<document source="test data">
    <citation ids="citation" names="citation">
        <label>
            citation
        <paragraph>
            The Source
    <system_message level="2" line="2" source="test data" type="WARNING">
        <paragraph>
            Explicit markup ends without a blank line; unexpected unindent.
    <paragraph>
        No blank line.
"""],
["""\
.. [citation label with spaces] this isn't a citation

.. [*citationlabelwithmarkup*] this isn't a citation
""",
"""\
<document source="test data">
    <comment xml:space="preserve">
        [citation label with spaces] this isn't a citation
    <comment xml:space="preserve">
        [*citationlabelwithmarkup*] this isn't a citation
"""],
["""
isolated internals : ``.-_``.

.. [citation.withdot] one dot

.. [citation-withdot] one hyphen

.. [citation_withunderscore] one underscore

.. [citation:with:colons] two colons

.. [citation+withplus] one plus
""",
"""<document source="test data">
    <paragraph>
        isolated internals : \n\
        <literal>
            .-_
        .
    <citation ids="citation-withdot" names="citation.withdot">
        <label>
            citation.withdot
        <paragraph>
            one dot
    <citation ids="citation-withdot-1" names="citation-withdot">
        <label>
            citation-withdot
        <paragraph>
            one hyphen
    <citation ids="citation-withunderscore" names="citation_withunderscore">
        <label>
            citation_withunderscore
        <paragraph>
            one underscore
    <citation ids="citation-with-colons" names="citation:with:colons">
        <label>
            citation:with:colons
        <paragraph>
            two colons
    <citation ids="citation-withplus" names="citation+withplus">
        <label>
            citation+withplus
        <paragraph>
            one plus
"""],
]


if __name__ == '__main__':
    unittest.main()
