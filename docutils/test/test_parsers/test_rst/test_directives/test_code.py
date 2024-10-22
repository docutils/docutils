#! /usr/bin/env python3

# $Id$
# Author: Guenter Milde
# Copyright: This module has been placed in the public domain.

"""
Test the 'code' directive in parsers/rst/directives/body.py.
"""

from pathlib import Path
import re
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

if with_pygments:
    import pygments
    _pv = re.match(r'^([0-9]+)\.([0-9]*)', pygments.__version__)
    PYGMENTS_2_14_PLUS = (int(_pv[1]), int(_pv[2])) >= (2, 14)
else:
    PYGMENTS_2_14_PLUS = None


class ParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.report_level = 5
        for name, cases in totest.items():
            if name == 'code_parsing' and not with_pygments:
                self.skipTest('syntax highlight requires pygments')
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['code'] = [
["""\
.. code::

   This is a code block.
""",
"""\
<document source="test data">
    <literal_block classes="code" xml:space="preserve">
        This is a code block.
"""],
["""\
.. code::
  :class: testclass
  :name: without argument

  This is a code block with generic options.
""",
"""\
<document source="test data">
    <literal_block classes="code testclass" ids="without-argument" names="without\\ argument" xml:space="preserve">
        This is a code block with generic options.
"""],
["""\
.. code:: text
  :class: testclass

  This is a code block with text.
""",
"""\
<document source="test data">
    <literal_block classes="code text testclass" xml:space="preserve">
        This is a code block with text.
"""],
["""\
.. code::
  :number-lines:

  This is a code block with text.
""",
"""\
<document source="test data">
    <literal_block classes="code" xml:space="preserve">
        <inline classes="ln">
            1 \n\
        This is a code block with text.
"""],
["""\
.. code::
  :number-lines: 30

  This is a code block with text.
""",
"""\
<document source="test data">
    <literal_block classes="code" xml:space="preserve">
        <inline classes="ln">
            30 \n\
        This is a code block with text.
"""],
["""\
.. code::
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Content block expected for the "code" directive; none found.
        <literal_block xml:space="preserve">
            .. code::
"""],
]

totest['code_parsing'] = [
["""\
.. code:: python3
  :class: testclass

   print('hello world') # to stdout
""",
"""\
<document source="test data">
    <literal_block classes="code python3 testclass" xml:space="preserve">
         \n\
        <inline classes="name builtin">
            print
        <inline classes="punctuation">
            (
        <inline classes="literal string single">
            'hello world'
        <inline classes="punctuation">
            )
         \n\
        <inline classes="comment single">
            # to stdout
"""],
["""\
.. code:: python3
  :class: testclass
  :name: my_function
  :number-lines: 7

  def my_function():
      '''Test the lexer.
      '''

      # and now for something completely different
      print(8/2)
""",
"""\
<document source="test data">
    <literal_block classes="code python3 testclass" ids="my-function" names="my_function" xml:space="preserve">
        <inline classes="ln">
             7 \n\
        <inline classes="keyword">
            def
         \n\
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
    <literal_block classes="code python3 testclass" ids="my-function" names="my_function" xml:space="preserve">
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
  :class: testclass

  hello \\emph{world} % emphasize
""",
"""\
<document source="test data">
    <literal_block classes="code latex testclass" xml:space="preserve">
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
["""\
.. code:: rst
  :number-lines:

  This is a code block with text.
""",
"""\
<document source="test data">
    <literal_block classes="code rst" xml:space="preserve">
        <inline classes="ln">
            1 \n\
        This is a code block with text.
"""],
["""\
Code not parsed but warning silenced in ParserTestCase.

.. code:: s-lang

   % abc.sl
   autoload("abc_mode", "abc");
""",
"""\
<document source="test data">
    <paragraph>
        Code not parsed but warning silenced in ParserTestCase.
    <literal_block classes="code s-lang" xml:space="preserve">
        % abc.sl
        autoload("abc_mode", "abc");
"""],
["""\
Place the language name in a class argument to avoid the no-lexer warning:

.. code::
   :class: s-lang

   % abc.sl
   autoload("abc_mode", "abc");
""",
"""\
<document source="test data">
    <paragraph>
        Place the language name in a class argument to avoid the no-lexer warning:
    <literal_block classes="code s-lang" xml:space="preserve">
        % abc.sl
        autoload("abc_mode", "abc");
"""],
]


if __name__ == '__main__':
    unittest.main()
