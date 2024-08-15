#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for inline markup in PEPs (readers/pep.py).
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
from docutils.parsers.rst.states import Inliner
from docutils.readers.pep import Reader
from docutils.utils import new_document


class PEPParserTestCase(unittest.TestCase):
    maxDiff = None

    def test_parser(self):
        parser = Parser(rfc2822=True, inliner=Inliner())
        settings = get_default_settings(Parser, Reader)
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['standalone_references'] = [
["""\
See PEP 287 (pep-0287.txt),
and RFC 2822 (which obsoletes RFC822 and RFC-733).
""",
"""\
<document source="test data">
    <paragraph>
        See \n\
        <reference refuri="https://peps.python.org/pep-0287">
            PEP 287
         (
        <reference refuri="https://peps.python.org/pep-0287">
            pep-0287.txt
        ),
        and \n\
        <reference refuri="https://tools.ietf.org/html/rfc2822.html">
            RFC 2822
         (which obsoletes \n\
        <reference refuri="https://tools.ietf.org/html/rfc822.html">
            RFC822
         and \n\
        <reference refuri="https://tools.ietf.org/html/rfc733.html">
            RFC-733
        ).
"""],
["""\
References split across lines:

PEP
287

RFC
2822
""",
"""\
<document source="test data">
    <paragraph>
        References split across lines:
    <paragraph>
        <reference refuri="https://peps.python.org/pep-0287">
            PEP
            287
    <paragraph>
        <reference refuri="https://tools.ietf.org/html/rfc2822.html">
            RFC
            2822
"""],
["""\
Test PEP-specific implicit references before a URL:

PEP 287 (https://peps.python.org/pep-0287), RFC 2822.
""",
"""\
<document source="test data">
    <paragraph>
        Test PEP-specific implicit references before a URL:
    <paragraph>
        <reference refuri="https://peps.python.org/pep-0287">
            PEP 287
         (
        <reference refuri="https://peps.python.org/pep-0287">
            https://peps.python.org/pep-0287
        ), \n\
        <reference refuri="https://tools.ietf.org/html/rfc2822.html">
            RFC 2822
        .
"""],
]

totest['miscellaneous'] = [
["""\
For *completeness*, _`let's` ``test`` **other** forms_
|of| `inline markup` [*]_.

.. [*] See https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html.
""",
"""\
<document source="test data">
    <paragraph>
        For \n\
        <emphasis>
            completeness
        , \n\
        <target ids="let-s" names="let's">
            let's
         \n\
        <literal>
            test
         \n\
        <strong>
            other
         \n\
        <reference name="forms" refname="forms">
            forms
        \n\
        <substitution_reference refname="of">
            of
         \n\
        <title_reference>
            inline markup
         \n\
        <footnote_reference auto="*" ids="footnote-reference-1">
        .
    <footnote auto="*" ids="footnote-1">
        <paragraph>
            See \n\
            <reference refuri="https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html">
                https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html
            .
"""],
]


if __name__ == '__main__':
    unittest.main()
