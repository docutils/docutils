#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for inline markup in PEPs (readers/pep.py).
"""

if __name__ == '__main__':
    import __init__  # noqa: F401
from test_readers import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.PEPParserTestSuite()
    s.generateTests(totest)
    return s


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
        <reference refuri="https://www.python.org/dev/peps/pep-0287">
            PEP 287
         (
        <reference refuri="https://www.python.org/dev/peps/pep-0287">
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
        <reference refuri="https://www.python.org/dev/peps/pep-0287">
            PEP
            287
    <paragraph>
        <reference refuri="https://tools.ietf.org/html/rfc2822.html">
            RFC
            2822
"""],
["""\
Test PEP-specific implicit references before a URL:

PEP 287 (https://www.python.org/dev/peps/pep-0287), RFC 2822.
""",
"""\
<document source="test data">
    <paragraph>
        Test PEP-specific implicit references before a URL:
    <paragraph>
        <reference refuri="https://www.python.org/dev/peps/pep-0287">
            PEP 287
         (
        <reference refuri="https://www.python.org/dev/peps/pep-0287">
            https://www.python.org/dev/peps/pep-0287
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
    import unittest
    unittest.main(defaultTest='suite')
