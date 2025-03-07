#! /usr/bin/env python3

# $Id$
# Author: Guenter Milde
# Copyright: This module has been placed in the public domain.

"""
Test the 'code' directive in body.py with syntax_highlight = 'long'.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[4]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.utils import new_document
from docutils.utils.code_analyzer import with_pygments
from test.test_parsers.test_rst.test_directives.test_code \
    import PYGMENTS_2_14_PLUS, def_ws


@unittest.skipUnless(with_pygments, 'needs Pygments')
class ParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.syntax_highlight = 'long'
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['code_parsing_long'] = [
["""\
.. code:: python3
  :number-lines: 7

  def my_function():
      '''Test the lexer.
      '''

      # and now for something completely different
      print(8/2)
""",
f"""\
<document source="test data">
    <literal_block classes="code python3" xml:space="preserve">
        <inline classes="ln">
             7 \n\
        <inline classes="keyword">
            def
        {def_ws}
        <inline classes="name function">
            my_function
        <inline classes="punctuation">
            ():
        <inline classes="whitespace">
            \n\
        <inline classes="ln">
             8 \n\
        <inline classes="whitespace">
                \n\
        <inline classes="literal string doc">
            \'\'\'Test the lexer.
        <inline classes="ln">
             9 \n\
        <inline classes="literal string doc">
                \'\'\'
        <inline classes="whitespace">
            \n\
        <inline classes="ln">
            10 \n\
        <inline classes="whitespace">
            \n\
        <inline classes="ln">
            11 \n\
        <inline classes="whitespace">
            \n\
        <inline classes="comment single">
            # and now for something completely different
        <inline classes="whitespace">
            \n\
        <inline classes="ln">
            12 \n\
        <inline classes="whitespace">
            \n\
        <inline classes="name builtin">
            print
        <inline classes="punctuation">
            (
        <inline classes="literal number integer">
            8
        <inline classes="operator">
            /
        <inline classes="literal number integer">
            2
        <inline classes="punctuation">
            )
""" if PYGMENTS_2_14_PLUS else """\
<document source="test data">
    <literal_block classes="code python3" xml:space="preserve">
        <inline classes="ln">
             7 \n\
        <inline classes="keyword">
            def
         \n\
        <inline classes="name function">
            my_function
        <inline classes="punctuation">
            ():
        \n\
        <inline classes="ln">
             8 \n\
            \n\
        <inline classes="literal string doc">
            \'\'\'Test the lexer.
        <inline classes="ln">
             9 \n\
        <inline classes="literal string doc">
                \'\'\'
        \n\
        <inline classes="ln">
            10 \n\
        \n\
        <inline classes="ln">
            11 \n\
            \n\
        <inline classes="comment single">
            # and now for something completely different
        \n\
        <inline classes="ln">
            12 \n\
            \n\
        <inline classes="name builtin">
            print
        <inline classes="punctuation">
            (
        <inline classes="literal number integer">
            8
        <inline classes="operator">
            /
        <inline classes="literal number integer">
            2
        <inline classes="punctuation">
            )
"""],
["""\
.. code:: latex

  hello \\emph{world} % emphasize
""",
"""\
<document source="test data">
    <literal_block classes="code latex" xml:space="preserve">
        hello \n\
        <inline classes="keyword">
            \\emph
        <inline classes="name builtin">
            {
        world
        <inline classes="name builtin">
            }
         \n\
        <inline classes="comment">
            % emphasize
"""],
]


if __name__ == '__main__':
    unittest.main()
