#! /usr/bin/env python3
# $Id$
# Author: Stefan Rank <strank(AT)strank(DOT)info>
# Copyright: This module has been placed in the public domain.

"""
Tests for basic functionality of parser classes.
"""

from pathlib import Path
import sys
import types
import unittest

if __name__ == '__main__':
    # prepend the local "docutils root" to the Python library path
    sys.path.insert(0, Path(__file__).resolve().parents[2].as_posix())

from docutils import frontend, nodes, statemachine, utils
import docutils.parsers.rst
from docutils.parsers.rst import states


class RstParserTests(unittest.TestCase):

    def test_inputrestrictions(self):
        # input must be unicode at all times, check for meaningful Exception
        parser = docutils.parsers.rst.Parser()
        document = utils.new_document('test data',
                                      frontend.get_default_settings(parser))
        with self.assertRaises(TypeError):
            parser.parse(b'hol', document)


class RSTStateTests(unittest.TestCase):

    # state machine
    machine = states.RSTStateMachine(state_classes=states.state_classes,
                                     initial_state='Body')
    # "generic" state
    state = states.RSTState(machine)

    def title_markup(self, text, adornment='-'):
        underline = adornment * len(text)
        return statemachine.StringList([text, underline], 'section block')

    def setUp(self):
        # state machine runtime initialization (cf. RSTStateMachine.run())
        # Only for test:
        #    don't use this low-level approach in production code!

        # empty <document> and settings:
        settings = frontend.get_default_settings(docutils.parsers.rst.Parser)
        settings.halt_level = 2
        settings.warning_stream = ''
        document = self.document = utils.new_document('test data', settings)
        # language module (localized directive and role names)
        # self.machine.language = languages.get_language(
        #                             document.settings.language_code)
        # self.machine.match_titles = True  # support sections
        # "memo": Container for document-wide auxiliary data
        inliner = states.Inliner()  # rST parser for inline markup
        inliner.init_customizations(document.settings)
        self.machine.memo = types.SimpleNamespace(document=document,
                                                  reporter=document.reporter,
                                                  language='en',
                                                  title_styles=[],
                                                  inliner=inliner)
        # self.machine.document = document
        # self.machine.reporter = document.reporter
        self.machine.node = document
        # initialize `state` object
        self.state.runtime_init()

    def test_nested_parse(self):
        # parse a text block in a nested parser
        text = statemachine.StringList(['test input'], 'nested block')
        tip = nodes.tip('')  # base `node`

        # plain text -> attach <paragraph> to `node`:
        self.state.nested_parse(text, input_offset=0, node=tip)
        self.assertEqual('<tip><paragraph>test input</paragraph></tip>',
                         str(tip))

        # by default, section titles are not supported:
        title = self.title_markup('top', '-')
        with self.assertRaisesRegex(utils.SystemMessage,
                                    'Unexpected section title.'):
            self.state.nested_parse(title, input_offset=0, node=tip)

    # Nested parsing with section markup is supported with
    # ``match_titles=True`` but not used in Docutils.
    # (However, Sphinx "autodoc" and some contributed extensions use it.)
    def test_nested_parse_with_sections_base_attached(self):
        # If the base `node` is attached to the `document`,
        # the document-wide section style hierarchy is used.

        # base`node` is a <section>
        section = nodes.section('')
        self.document += section  # attach to document
        self.machine.memo.title_styles.append('-')  # register title style
        # parse level-2 section title
        title = self.title_markup('sub', '~')
        # -> append new <section> to the parsers base `node`
        self.state.nested_parse(title, 0, node=section, match_titles=True)
        self.assertEqual('<document source="test data">\n'
                         '    <section>\n'
                         '        <section ids="sub" names="sub">\n'
                         '            <title>\n'
                         '                sub\n',
                         str(self.document.pformat()))
        self.assertEqual(['-', '~'], self.machine.memo.title_styles)

        # parse top-level section title
        title = self.title_markup('top', '-')
        # -> move 1 level up and attach <section> to document
        self.state.nested_parse(title, 0, node=section, match_titles=True)
        self.assertEqual('<document source="test data">\n'
                         '    <section>\n'
                         '        <section ids="sub" names="sub">\n'
                         '            <title>\n'
                         '                sub\n'
                         '    <section ids="top" names="top">\n'
                         '        <title>\n'
                         '            top\n',
                         str(self.document.pformat()))

        # base`node` is a <paragraph>
        paragraph = nodes.paragraph('', 'base node')
        section += paragraph  # attach (indirectly)
        # parse top-level section title
        title = self.title_markup('top 2', '-')
        # -> move 1 level up and attach <section> to document
        self.state.nested_parse(title, 0, node=paragraph, match_titles=True)
        self.assertEqual('<section ids="top-2" names="top\\ 2">\n'
                         '    <title>\n'
                         '        top 2\n',
                         self.document[-1].pformat())

        # new (2nd-level) section title
        # TODO: don't append <section> to <paragraph>!
        title = self.title_markup('sub 2', '~')
        self.state.nested_parse(title, 0, node=paragraph, match_titles=True)
        self.assertEqual('<section>\n'
                         '    <section ids="sub" names="sub">\n'
                         '        <title>\n'
                         '            sub\n'
                         '    <paragraph>\n'
                         '        base node\n'
                         '        <section ids="sub-2" names="sub\\ 2">\n'
                         '            <title>\n'
                         '                sub 2\n',
                         section.pformat())


if __name__ == '__main__':
    unittest.main()
