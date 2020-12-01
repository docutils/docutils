#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# $Id$
# :Copyright: © 2020 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause
"""
Tests for literal blocks in CommonMark parsers
Cf. the `CommonMark Specification <https://spec.commonmark.org/>`__
"""
from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_parsers import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.RecommonmarkParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['literal_blocks'] = [
["""\
A paragraph:

    A literal block (indented code block).
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
        A literal block (indented code block).
"""],
["""\
A paragraph:
~~~
A literal block (fenced code block).
~~~
Another paragraph:

    Another literal block.
    With two blank lines following.


A final paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
        A literal block (fenced code block).
    <paragraph>
        Another paragraph:
    <literal_block classes="code" xml:space="preserve">
        Another literal block.
        With two blank lines following.
    <paragraph>
        A final paragraph.
"""],
["""\
A paragraph
on more than
one line.
    No literal block
    but paragraph continuation lines.
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph
        on more than
        one line.
        No literal block
        but paragraph continuation lines.
"""],
["""\
A paragraph:

    A literal block.
no blank line
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
        A literal block.
    <paragraph>
        no blank line
"""],
["""\
A paragraph:
```
  A fenced code block.
```
no blank lines.
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
          A fenced code block.
    <paragraph>
        no blank lines.
"""],
[r"""
A paragraph:

   Not a literal block because only indented 3 spaces.
""",
r"""<document source="test data">
    <paragraph>
        A paragraph:
    <paragraph>
        Not a literal block because only indented 3 spaces.
"""],
[r"""
    A literal block.

   Not a literal block.
""",
r"""<document source="test data">
    <literal_block classes="code" xml:space="preserve">
        A literal block.
    <paragraph>
        Not a literal block.
"""],
["""\
A paragraph:

      A wonky literal block.
    Literal line 2.
  
      Literal line 3.
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
          A wonky literal block.
        Literal line 2.
        \n\
          Literal line 3.
"""],
["""\
A paragraph:
~~~
  A fenced literal block.
Literal line 2.

  Literal line 3.
~~~  
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
          A fenced literal block.
        Literal line 2.
        \n\
          Literal line 3.
"""],
["""\
A paragraph:
~~~ ruby
A literal block (fenced code block)
with *info string*.
~~~
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code ruby" xml:space="preserve">
        A literal block (fenced code block)
        with *info string*.
"""],
]



if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
