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
from docutils.parsers.rst.directives import register_directive
from docutils.utils import new_document


class ParseIntoNode(rst.Directive):
    """A directive implementing nested parsing with support for sections.
    """
    final_argument_whitespace = True
    has_content = True

    def run(self):
        # similar to sphinx.util.parsing.nested_parse_to_nodes()
        node = nodes.Element()
        node.document = self.state.document  # not required
        # support sections (unless we know it is invalid):
        match_titles = isinstance(self.state_machine.node,
                                  (nodes.document, nodes.section))
        self.state.nested_parse(self.content, input_offset=0,
                                node=node, match_titles=match_titles)
        # Append and move the "insertion point" to the last nested section.
        self.state_machine.node += node.children
        # print(self.state_machine, self.state_machine.node[-1].shortrepr())
        try:
            while isinstance(self.state_machine.node[-1], nodes.section):
                self.state_machine.node = self.state_machine.node[-1]
        except IndexError:
            pass
        # pass on the new "current node" to parent state machines
        sm = self.state_machine
        try:
            while True:
                sm = sm.parent_state_machine
                sm.node = self.state_machine.node
        except AttributeError:
            pass
        return []  # node already attached to document


class ParseIntoCurrentNode(ParseIntoNode):
    def run(self):
        node = self.state_machine.node  # the current "insertion point"
        # support sections (unless we know it is invalid):
        match_titles = isinstance(node, (nodes.document, nodes.section))
        self.state.nested_parse(self.content, 0, node, match_titles)
        return []  # node already attached to document


class ParseIntoSectionNode(ParseIntoNode):
    def run(self):
        if not isinstance(self.state_machine.node,
                          (nodes.document, nodes.section)):
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
        register_directive('nested', ParseIntoNode)
        register_directive('nested-current', ParseIntoCurrentNode)
        register_directive('nested-section', ParseIntoSectionNode)
        parser = rst.Parser()
        settings = get_default_settings(rst.Parser)
        settings.warning_stream = ''
        settings.halt_level = 5
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    try:
                        document.validate()
                    except nodes.ValidationError as e:
                        document.append(document.reporter.warning(
                            str(e), base_node=e.problematic_element or None))
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['nested_parsing'] = [
# Start new section hierarchy with every nested parse.
["""\
sec1
====
sec1.1
------

.. nested::

  nested1
  *******
  nested1.1
  =========

sec2
====
The document-wide section title styles are kept.

.. nested::

  nested2
  =======
  nested2.1
  *********

sec2.2
------
sec2.2.1
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
            <section ids="nested1" names="nested1">
                <title>
                    nested1
                <section ids="nested1-1" names="nested1.1">
                    <title>
                        nested1.1
    <section ids="sec2" names="sec2">
        <title>
            sec2
        <paragraph>
            The document-wide section title styles are kept.
        <section ids="nested2" names="nested2">
            <title>
                nested2
            <section ids="nested2-1" names="nested2.1">
                <title>
                    nested2.1
        <section ids="sec2-2" names="sec2.2">
            <title>
                sec2.2
            <section ids="sec2-2-1" names="sec2.2.1">
                <title>
                    sec2.2.1
"""],
# Move "insertion point" if the nested block contains sections to
# comply with the validity constraints of the "structure model".
["""\
.. nested::

  nested1
  *******
  nested1.1
  ---------

This paragraph belongs to the last nested section.
""",
"""\
<document source="test data">
    <section ids="nested1" names="nested1">
        <title>
            nested1
        <section ids="nested1-1" names="nested1.1">
            <title>
                nested1.1
            <paragraph>
                This paragraph belongs to the last nested section.
"""],
["""\
.. note:: A preceding directive must not foil the "insertion point move".

.. nested::

  nested1
  *********
  nested1.1
  ---------

This paragraph belongs to the last nested section.
""",
"""\
<document source="test data">
    <note>
        <paragraph>
            A preceding directive must not foil the "insertion point move".
    <section ids="nested1" names="nested1">
        <title>
            nested1
        <section ids="nested1-1" names="nested1.1">
            <title>
                nested1.1
            <paragraph>
                This paragraph belongs to the last nested section.
"""],
["""\
.. nested::

  Keep the "current node", if the nested parse does not
  contain a section.

This paragraph belongs to the document.
""",
"""\
<document source="test data">
    <paragraph>
        Keep the "current node", if the nested parse does not
        contain a section.
    <paragraph>
        This paragraph belongs to the document.
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

sec1.1.2
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
            <section ids="sec1-1-2" names="sec1.1.2">
                <title>
                    sec1.1.2
"""],
# parse into generated <section> node:
["""\
sec1
====
sec1.1
------
.. nested-section::

  nested-section1
  ***************
  nested-section1.1
  =================

This paragraph belongs to the last nested section.

sec1.1.2
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
                <section ids="nested-section1" names="nested-section1">
                    <title>
                        nested-section1
                    <section ids="nested-section1-1" names="nested-section1.1">
                        <title>
                            nested-section1.1
            <paragraph>
                This paragraph belongs to the last nested section.
            <section ids="sec1-1-2" names="sec1.1.2">
                <title>
                    sec1.1.2
    <system_message level="2" line="12" source="test data" type="WARNING">
        <paragraph>
            Element <section ids="sec1-1" names="sec1.1"> invalid:
              Child element <paragraph> not allowed at this position.
"""],
# Nested parsing in a block-quote:
["""\
  .. nested-current::

    Nested parsing is OK but a section is invalid in a block-quote.

    nested section
    ==============

  .. nested::

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
