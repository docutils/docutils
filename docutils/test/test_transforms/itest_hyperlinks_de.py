#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.references.Hyperlinks with non-English language.

TODO: This test fails currently when run as part of "alltests" because

      - the "info" system-messages for directive fallbacks are only generated
        once (the name -> directive mapping is cached in
        ``docutils.parsers.rst.directives._directives``).

      - the cache is not reset between after processing a document
        (i.e. it contains name -> directive mappings from other tests).

      See also https://sourceforge.net/p/docutils/feature-requests/71/
"""

if __name__ == '__main__':
    import __init__  # noqa: F401
from test import DocutilsTestSupport
from docutils.transforms.references import (
         PropagateTargets, AnonymousHyperlinks, IndirectHyperlinks,
         ExternalTargets, InternalTargets, DanglingReferences)
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    settings = {}
    settings['language_code'] = 'de'
    s = DocutilsTestSupport.TransformTestSuite(
        parser, suite_settings=settings)
    s.generateTests(totest)
    return s


totest = {}

totest['hyperlinks'] = ((PropagateTargets, AnonymousHyperlinks,
                         IndirectHyperlinks, ExternalTargets,
                         InternalTargets, DanglingReferences), [

["""\
Target_ should propagate past the system_message to set "id" on note.

.. _target:
.. note:: Kurznotiz
   :name: mynote
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="Target" refid="target">
            Target
         should propagate past the system_message to set "id" on note.
    <target refid="target">
    <system_message level="1" line="4" source="test data" type="INFO">
        <paragraph>
            No directive entry for "note" in module "docutils.parsers.rst.languages.de".
            Using English fallback for directive "note".
    <note ids="mynote target" names="mynote target">
        <paragraph>
            Kurznotiz
    <system_message level="1" source="test data" type="INFO">
        <paragraph>
            Using <module 'docutils.languages.de' from '/usr/local/src/docutils-git-svn/docutils/docutils/languages/de.py'> for language "de".
"""],
])

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
