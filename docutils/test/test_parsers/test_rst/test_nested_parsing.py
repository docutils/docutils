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
        node.document = self.state.document  # not required
        # support sections if it is valid:
        match_titles = isinstance(self.state.parent, (nodes.document,
                                                      nodes.section))
        self.state.nested_parse(self.content, input_offset=0,
                                node=node, match_titles=match_titles)
        return node.children


class ParseIntoCurrentNode(ParseIntoDetachedNode):
    def run(self):
        node = self.state.parent  # the current "insertion point"
        # support sections if it is valid:
        match_titles = isinstance(node, (nodes.document, nodes.section))
        self.state.nested_parse(self.content, 0, node, match_titles)
        return []  # nodes already attached to document


class ParseIntoSectionNode(ParseIntoDetachedNode):
    def run(self):
        if not isinstance(self.state.parent, (nodes.document, nodes.section)):
            msg = self.reporter.error(
                    'The "nested-section" directive can only be used'
                    ' where a section is valid.',
                    nodes.literal_block(self.block_text, self.block_text),
                    line=self.lineno)
            return [msg]
        node = nodes.section('')
        node.append(nodes.title('', 'generated section'))
        # In production, also generate and register section name and ID
        # (cf. rst.states.RSTState.new_subsection()).
        self.state.nested_parse(self.content, 0, node, match_titles=True)
        return [node]


class ParserTestCase(unittest.TestCase):
    maxDiff = None

    def test_parser(self):
        rst.directives.register_directive('nested-detached',
                                          ParseIntoDetachedNode)
        rst.directives.register_directive('nested-current',
                                          ParseIntoCurrentNode)
        rst.directives.register_directive('nested-section',
                                          ParseIntoSectionNode)
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

totest['nested_parsing'] = [

["""\
Parse into section node:

.. nested-section::

  This is nested.

sec2
====
""",
"""\
<document source="test data">
    <paragraph>
        Parse into section node:
    <section>
        <title>
            generated section
        <paragraph>
            This is nested.
    <section ids="sec2" names="sec2">
        <title>
            sec2
"""],
# start new section hierarchy with every nested parse
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

sec1.1.1
~~~~~~~~
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
            <section ids="sec1-1-1" names="sec1.1.1">
                <title>
                    sec1.1.1
    <section ids="sec2" names="sec2">
        <title>
            sec2
        <paragraph>
            The document-wide section title styles are kept.
"""],
# base node == current node
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

sec1.1.1
~~~~~~~~
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
            <section ids="sec1-1-1" names="sec1.1.1">
                <title>
                    sec1.1.1
"""],
# parse into generated <section> node:
["""\
sec1
====
sec1.1
------
.. nested-section::

  attached1
  *********
  attached1.1
  ===========

sec1.1.1
~~~~~~~~

""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <section ids="sec1-1" names="sec1.1">
            <title>
                sec1.1
            <section>
                <title>
                    generated section
                <section ids="attached1" names="attached1">
                    <title>
                        attached1
                    <section ids="attached1-1" names="attached1.1">
                        <title>
                            attached1.1
            <section ids="sec1-1-1" names="sec1.1.1">
                <title>
                    sec1.1.1
"""],
# Nested parsing in a block-quote:
["""\
  .. nested-current::

    Nested parsing is OK but a section is invalid in a block-quote.

    nested section
    ==============

  .. nested-detached::

    invalid section
    ---------------

  .. nested-section::

    The <section> base node is invalid in a block-quote.
""",
"""\
<document source="test data">
    <block_quote>
        <paragraph>
            Nested parsing is OK but a section is invalid in a block-quote.
        <system_message level="3" line="6" source="test data" type="ERROR">
            <paragraph>
                Unexpected section title.
            <literal_block xml:space="preserve">
                nested section
                ==============
        <system_message level="3" line="11" source="test data" type="ERROR">
            <paragraph>
                Unexpected section title.
            <literal_block xml:space="preserve">
                invalid section
                ---------------
        <system_message level="3" line="13" source="test data" type="ERROR">
            <paragraph>
                The "nested-section" directive can only be used where a section is valid.
            <literal_block xml:space="preserve">
                .. nested-section::
                \n\
                  The <section> base node is invalid in a block-quote.
"""],
]


if __name__ == '__main__':
    unittest.main()
