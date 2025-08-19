#! /usr/bin/env python3
# $id$
# author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for nested parsing with support for sections (cf. states.py).

The method states.RSTState.nested_parse() provides the argument `match_titles`.
However, in Docutils, it is only used with `match_titles=False`.
None of the standard Docutils directives supports section titles in the
directive content.  (Directives supporting sections in the content are,
e.g., defined by the "autodoc" and "kerneldoc" Sphinx extensions.)

Up to Docutils 0.22, the section title styles were document-wide enforced and
sections with current level or higher were silently dropped!

Sphinx uses the `sphinx.util.parsing._fresh_title_style_context` context
manager to provide a separate title style hierarchy for nested parsing.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))

from docutils import nodes
from docutils.frontend import get_default_settings
from docutils.parsers import rst
from docutils.utils import new_document


class ParseIntoDetachedNode(rst.Directive):
    """A directive implementing nested parsing with support for sections.
    """
    final_argument_whitespace = True
    has_content = True

    def run(self):
        # similar to sphinx.util.parsing.nested_parse_to_nodes()
        node = nodes.Element()
        node.document = self.state.document
        self.state.nested_parse(self.content, input_offset=0,
                                node=node, match_titles=True)
        return node.children


class ParseIntoCurrentNode(ParseIntoDetachedNode):
    def run(self):
        node = self.state.parent  # the current "insertion point"
        self.state.nested_parse(self.content, 0, node, match_titles=True)
        return []  # nodes already attached to document


class ParseIntoAttachedNode(ParseIntoDetachedNode):
    def run(self):
        node = nodes.sidebar('')
        self.state.parent.append(node)
        self.state.nested_parse(self.content, 0, node, match_titles=True)
        return []  # nodes already attached to document


class ParserTestCase(unittest.TestCase):
    maxDiff = None

    def test_parser(self):
        rst.directives.register_directive('nested-detached',
                                          ParseIntoDetachedNode)
        rst.directives.register_directive('nested-current',
                                          ParseIntoCurrentNode)
        rst.directives.register_directive('nested-attached',
                                          ParseIntoAttachedNode)
        parser = rst.Parser()
        settings = get_default_settings(rst.Parser)
        settings.warning_stream = ''
        settings.halt_level = 5
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

# Parse into the base node:
totest['nested_parsing'] = [
["""\
Preceding paragraph.

.. nested-attached::

  .. hint:: this is nested.

Succeeding paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Preceding paragraph.
    <sidebar>
        <hint>
            <paragraph>
                this is nested.
    <paragraph>
        Succeeding paragraph.
"""],
# detached base node -> start new section hierarchy with every nested parse
["""\
sec1
====
sec1.1
------
.. nested-detached::

  detached1
  *********
  detached1.1
  -----------
  detached1.1.1
  =============

.. nested-detached::

  detached2
  ---------
  detached2.1
  ***********

Succeeding paragraph.

sec2
====
The document-wide section title styles are kept.
""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <section ids="sec1-1" names="sec1.1">
            <title>
                sec1.1
            <section ids="detached1" names="detached1">
                <title>
                    detached1
                <section ids="detached1-1" names="detached1.1">
                    <title>
                        detached1.1
                    <section ids="detached1-1-1" names="detached1.1.1">
                        <title>
                            detached1.1.1
            <section ids="detached2" names="detached2">
                <title>
                    detached2
                <section ids="detached2-1" names="detached2.1">
                    <title>
                        detached2.1
            <paragraph>
                Succeeding paragraph.
    <section ids="sec2" names="sec2">
        <title>
            sec2
        <paragraph>
            The document-wide section title styles are kept.
"""],
# base node == current node -> keep section hierarchy
["""\
sec1
====
sec1.1
------
.. nested-current::

  current1
  ********
  current1.1
  -----------
  current1.1.1
  ============
  Top-level section appended to document.

Succeeding paragraph.
""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <section ids="sec1-1" names="sec1.1">
            <title>
                sec1.1
            <section ids="current1" names="current1">
                <title>
                    current1
                <section ids="current1-1" names="current1.1">
                    <title>
                        current1.1
                    <section ids="current1-1-1" names="current1.1.1">
                        <title>
                            current1.1.1
                        <paragraph>
                            Top-level section appended to document.
            <paragraph>
                Succeeding paragraph.
"""],
# parse into attached wrapper node:
["""\
sec1
====
sec1.1
------
.. nested-attached::

  attached1
  *********
  attached1.1
  ===========

Succeeding paragraph.
""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <section ids="sec1-1" names="sec1.1">
            <title>
                sec1.1
            <sidebar>
                <section ids="attached1" names="attached1">
                    <title>
                        attached1
                    <section ids="attached1-1" names="attached1.1">
                        <title>
                            attached1.1
            <paragraph>
                Succeeding paragraph.
"""],
# detached base node -> start new section hierarchy
["""\
sec1
====
sec1.1
------
sec2
====
.. nested-detached::
  detached1
  ~~~~~~~~~
  detached1.1
  -----------

Succeeding paragraph.
""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <section ids="sec1-1" names="sec1.1">
            <title>
                sec1.1
    <section ids="sec2" names="sec2">
        <title>
            sec2
        <section ids="detached1" names="detached1">
            <title>
                detached1
            <section ids="detached1-1" names="detached1.1">
                <title>
                    detached1.1
        <paragraph>
            Succeeding paragraph.
"""],
# base node == <blockquote>
["""\
sec1
====

  A block-quote is parsed into a detached <blockquote> element.

  .. nested-current::

    nested section
    ==============

  The nested <section> becomes a child of the <blockquote> (sic.)!

The calling directive should move the nested <section> or report
a validity violation.
""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <block_quote>
            <paragraph>
                A block-quote is parsed into a detached <blockquote> element.
            <section ids="nested-section" names="nested\\ section">
                <title>
                    nested section
            <paragraph>
                The nested <section> becomes a child of the <blockquote> (sic.)!
        <paragraph>
            The calling directive should move the nested <section> or report
            a validity violation.
"""],
]


if __name__ == '__main__':
    unittest.main()
