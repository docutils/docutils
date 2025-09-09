#! /usr/bin/env python3
# $id$
# author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for nested parsing with support for sections (cf. states.py).

The method states.RSTState.nested_parse() provides the argument `match_titles`.
With ``match_titles=True``, sections are supported, the section level is
determined by the document-wide hierarchy of title styles. [1]_

In Docutils, `nested_parse()` is only used with ``match_titles=False``.
None of the standard Docutils directives support section titles in the
directive content.   Up to Docutils 0.22, sections with current level or
higher were silently dropped!

Directives supporting sections in the content are defined
by Sphinx extensions, e.g., "autodoc" and "kerneldoc".

.. [1] Sphinx uses the `sphinx.util.parsing._fresh_title_style_context`
       context manager to provide a separate title style hierarchy for
       nested parsing.
"""

from pathlib import Path
import contextlib
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
        node = nodes.Element()
        node.document = self.state.document
        # Support sections (unless we know it is invalid):
        match_titles = isinstance(self.state_machine.node,
                                  (nodes.document, nodes.section))

        self.state.nested_parse(self.content, input_offset=self.content_offset,
                                node=node, match_titles=match_titles)
        self.state_machine.node += node.children

        # Move the "insertion point" to the last nested section.
        try:
            while isinstance(self.state_machine.node[-1], nodes.section):
                self.state_machine.node = self.state_machine.node[-1]
        except IndexError:
            pass
        # Pass current node to parent state machines:
        sm = self.state_machine
        try:
            while True:
                sm = sm.parent_state_machine
                sm.node = self.state_machine.node
        except AttributeError:
            pass
        return []  # node already attached to document


class ParseIntoCurrentNode(ParseIntoNode):
    # If `node` is the "current node", `nested_parse()` ensures validity
    # and updates the "current node".
    def run(self):
        self.state.nested_parse(self.content, self.content_offset,
                                match_titles=True)
        return []  # node already attached to document


class ParseIntoSectionNode(ParseIntoNode):
    # Some 3rd party extensions use a <section> as dummy base node.
    # cf. https://github.com/sphinx-contrib/autoprogram/blob/master/sphinxcontrib/autoprogram.py
    #
    # Attention: this directive is flawed:
    # * no check for section validity,
    # * "current" node not updated! -> element order may get lost.
    def run(self):
        node = nodes.section()
        self.state.nested_parse(self.content, self.content_offset,
                                node, match_titles=True)
        return node.children


class FreshParseIntoNode(ParseIntoNode):
    """Nested parsing with support for sections (separate title styles).

    * no check for section validity,
    * "current" node not updated! -> element order may get lost.

    cf. `sphinx.util.nodes.nested_parse_with_titles()`
    and `sphinx.util.parsing.nested_parse_to_nodes()`
    """
    def run(self):
        node = nodes.Element()
        with _fresh_title_style_context(self.state):
            self.state.nested_parse(self.content, self.content_offset,
                                    node, match_titles=True)
        return node.children


class FreshParseIntoCurrentNode(ParseIntoNode):
    # Nested parsing with support for sections (separate title styles)
    #
    # Parsing into the current node, `nested_parse()` ensures validity
    # and updates the "current node".
    def run(self):
        with _fresh_title_style_context(self.state):
            self.state.nested_parse(self.content, self.content_offset,
                                    match_titles=True)
        return []  # node already attached to document


@contextlib.contextmanager
def _fresh_title_style_context(state):
    # copied from sphinx/sphinx/util/parsing.py
    memo = state.memo
    surrounding_title_styles = memo.title_styles
    memo.title_styles = []
    memo.section_level = 0
    try:
        yield
    finally:
        memo.title_styles = surrounding_title_styles


class ParserTestCase(unittest.TestCase):
    maxDiff = None

    def test_parser(self):
        register_directive('nested', ParseIntoNode)
        register_directive('nested-current', ParseIntoCurrentNode)
        register_directive('nested-section', ParseIntoSectionNode)
        register_directive('fresh', FreshParseIntoNode)
        register_directive('fresh-current', FreshParseIntoCurrentNode)
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
# The document-wide section hierarchy is employed also in nested parsing.
["""\
sec1
====
sec1.1
------
.. nested::

  nested1.1.1
  ***********
  nested1.1.1.1
  ~~~~~~~~~~~~~

sec2
====
.. nested::

  skipping2.1
  ***********
  nested2.1
  ---------
  nested2.2
  ---------
  inaccessible2
  =============

sec2.2
------
skipping2.2.1
~~~~~~~~~~~~~
""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <section ids="sec1-1" names="sec1.1">
            <title>
                sec1.1
            <section ids="nested1-1-1" names="nested1.1.1">
                <title>
                    nested1.1.1
                <section ids="nested1-1-1-1" names="nested1.1.1.1">
                    <title>
                        nested1.1.1.1
    <section ids="sec2" names="sec2">
        <title>
            sec2
        <system_message level="3" line="16" source="test data" type="ERROR">
            <paragraph>
                Inconsistent title style: skip from level 1 to 3.
            <literal_block xml:space="preserve">
                skipping2.1
                ***********
            <paragraph>
                Established title styles: = - * ~
        <section ids="nested2-1" names="nested2.1">
            <title>
                nested2.1
        <section ids="nested2-2" names="nested2.2">
            <title>
                nested2.2
            <system_message level="3" line="22" source="test data" type="ERROR">
                <paragraph>
                    A level 1 section cannot be used here.
                <literal_block xml:space="preserve">
                    inaccessible2
                    =============
                <paragraph>
                    Established title styles: = - * ~
                <paragraph>
                    The parent of level 1 sections cannot be reached. The parser is at section level 2 but the current node has only 1 parent section(s).
                    One reason may be a high level section used in a directive that parses its content into a base node not attached to the document
                    (up to Docutils 0.21, these sections were silently dropped).
        <section ids="sec2-2" names="sec2.2">
            <title>
                sec2.2
            <system_message level="3" line="27" source="test data" type="ERROR">
                <paragraph>
                    Inconsistent title style: skip from level 2 to 4.
                <literal_block xml:space="preserve">
                    skipping2.2.1
                    ~~~~~~~~~~~~~
                <paragraph>
                    Established title styles: = - * ~
"""],
# The `ParseIntoNode` directive updates the "current node" to comply with
# the validity constraints of the "structure model".
["""\
.. nested::

  nested1
  *******
  nested2
  *******
  nested2.1
  ---------

This paragraph belongs to the last nested section.
""",
"""\
<document source="test data">
    <section ids="nested1" names="nested1">
        <title>
            nested1
    <section ids="nested2" names="nested2">
        <title>
            nested2
        <section ids="nested2-1" names="nested2.1">
            <title>
                nested2.1
            <paragraph>
                This paragraph belongs to the last nested section.
"""],
["""\
.. note:: The next directive is parsed with "nested_list_parse()".
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
            The next directive is parsed with "nested_list_parse()".
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
# If the base node is the "current node", it is possible to have lower
# level sections inside the nested content block.
# The generated nodes are added to the respective parent sections
# and not necessarily children of the base node.
["""\
sec1
====
sec1.1
------
.. note:: The next directive is parsed with "nested_list_parse()".
.. nested-current::

  nc1.1.1
  *******
  nc1.2
  -----
  nc2
  ===

sec2.2
------
""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <section ids="sec1-1" names="sec1.1">
            <title>
                sec1.1
            <note>
                <paragraph>
                    The next directive is parsed with "nested_list_parse()".
            <section ids="nc1-1-1" names="nc1.1.1">
                <title>
                    nc1.1.1
        <section ids="nc1-2" names="nc1.2">
            <title>
                nc1.2
    <section ids="nc2" names="nc2">
        <title>
            nc2
        <section ids="sec2-2" names="sec2.2">
            <title>
                sec2.2
"""],
# Flawed directive (no update of "current node"):
["""\
sec1
====
sec1.1
------
.. nested-section::

  nested-section1.1.1
  *******************

This paragraph belongs to the last nested section (sic!).

sec2
====
""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <section ids="sec1-1" names="sec1.1">
            <title>
                sec1.1
            <section ids="nested-section1-1-1" names="nested-section1.1.1">
                <title>
                    nested-section1.1.1
            <paragraph>
                This paragraph belongs to the last nested section (sic!).
    <section ids="sec2" names="sec2">
        <title>
            sec2
    <system_message level="2" line="10" source="test data" type="WARNING">
        <paragraph>
            Element <section ids="sec1-1" names="sec1.1"> invalid:
              Child element <paragraph> not allowed at this position.
"""],
# Even if the base node is a <section>, it does not show up in
# `node.parent_sections()` because it does not have a parent
# -> we cannot add a sibling section:
["""\
sec1
====
.. nested-section::

  nested-section1
  ===============
  with content
""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <system_message level="3" line="5" source="test data" type="ERROR">
            <paragraph>
                A level 1 section cannot be used here.
            <literal_block xml:space="preserve">
                nested-section1
                ===============
            <paragraph>
                Established title styles: =
            <paragraph>
                The parent of level 1 sections cannot be reached. The parser is at section level 1 but the current node has only 0 parent section(s).
                One reason may be a high level section used in a directive that parses its content into a base node not attached to the document
                (up to Docutils 0.21, these sections were silently dropped).
        <paragraph>
            with content
"""],
# Nested parsing in a block-quote:
["""\
  .. nested::

    A section in a block-quote is invalid.

    invalid section
    ---------------

  .. nested-current::

    invalid, too
    ============

  .. nested-section::

    The <section> base node is discarded.

    invalid section (sic!)
    ----------------------
""",
"""\
<document source="test data">
    <block_quote>
        <paragraph>
            A section in a block-quote is invalid.
        <system_message level="3" line="6" source="test data" type="ERROR">
            <paragraph>
                Unexpected section title.
            <literal_block xml:space="preserve">
                invalid section
                ---------------
        <system_message level="3" line="11" source="test data" type="ERROR">
            <paragraph>
                Unexpected section title.
            <literal_block xml:space="preserve">
                invalid, too
                ============
        <paragraph>
            The <section> base node is discarded.
        <section ids="invalid-section-sic" names="invalid\\ section\\ (sic!)">
            <title>
                invalid section (sic!)
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Element <block_quote> invalid:
              Child element <section ids="invalid-section-sic" names="invalid\\ section\\ (sic!)"> not allowed at this position.
"""],
# Nested parsing with new title style hierarchy
["""\
sec1
====
sec1.1
------
.. fresh::

  fresh1.1.1
  ==========
  fresh1.1.1.1
  ~~~~~~~~~~~~~

sec2
====
.. fresh::

  fresh2.1
  ***********
  New title styles with every directive.

  fresh2.1.1
  -----------
  fresh2.1.2
  -----------
  fresh2.1.2.1
  =============

This text belongs into the last nested section (sic!).

sec2.2
------
Document-wide title styles unchanged

sec2.2.1
********
""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <section ids="sec1-1" names="sec1.1">
            <title>
                sec1.1
            <section ids="fresh1-1-1" names="fresh1.1.1">
                <title>
                    fresh1.1.1
                <section ids="fresh1-1-1-1" names="fresh1.1.1.1">
                    <title>
                        fresh1.1.1.1
    <section ids="sec2" names="sec2">
        <title>
            sec2
        <section ids="fresh2-1" names="fresh2.1">
            <title>
                fresh2.1
            <paragraph>
                New title styles with every directive.
            <section ids="fresh2-1-1" names="fresh2.1.1">
                <title>
                    fresh2.1.1
            <section ids="fresh2-1-2" names="fresh2.1.2">
                <title>
                    fresh2.1.2
                <section ids="fresh2-1-2-1" names="fresh2.1.2.1">
                    <title>
                        fresh2.1.2.1
        <paragraph>
            This text belongs into the last nested section (sic!).
        <section ids="sec2-2" names="sec2.2">
            <title>
                sec2.2
            <paragraph>
                Document-wide title styles unchanged
            <section ids="sec2-2-1" names="sec2.2.1">
                <title>
                    sec2.2.1
    <system_message level="2" line="27" source="test data" type="WARNING">
        <paragraph>
            Element <section ids="sec2" names="sec2"> invalid:
              Child element <paragraph> not allowed at this position.
"""],
# Nested parsing into current node with new title style hierarchy
["""\
sec1
====
sec1.1
------
.. fresh-current::

  fc1.1.1
  -------
  fc1.1.2
  -------
  fc1.1.2.1
  =========

This text belongs into the last nested section.

sec1.2
------
""",
"""\
<document source="test data">
    <section ids="sec1" names="sec1">
        <title>
            sec1
        <section ids="sec1-1" names="sec1.1">
            <title>
                sec1.1
            <section ids="fc1-1-1" names="fc1.1.1">
                <title>
                    fc1.1.1
            <section ids="fc1-1-2" names="fc1.1.2">
                <title>
                    fc1.1.2
                <section ids="fc1-1-2-1" names="fc1.1.2.1">
                    <title>
                        fc1.1.2.1
                    <paragraph>
                        This text belongs into the last nested section.
        <section ids="sec1-2" names="sec1.2">
            <title>
                sec1.2
"""],
]


if __name__ == '__main__':
    unittest.main()
