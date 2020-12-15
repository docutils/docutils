#!/usr/bin/env python3
# -*- coding: utf8 -*-
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
Tests for inline markup in CommonMark parsers
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

totest['emphasis'] = [
["""\
*emphasis*
_also emphasis_
""",
"""\
<document source="test data">
    <paragraph>
        <emphasis>
            emphasis
        \n\
        <emphasis>
            also emphasis
"""],
[u"""\
Partially*emphasised*word.
""",
u"""\
<document source="test data">
    <paragraph>
        Partially
        <emphasis>
            emphasised
        word.
"""],
["""\
*emphasized sentence
across lines*
""",
"""\
<document source="test data">
    <paragraph>
        <emphasis>
            emphasized sentence
            across lines
"""],
["""\
*no emphasis without closing asterisk
""",
"""\
<document source="test data">
    <paragraph>
        *no emphasis without closing asterisk
"""],
[r"""
No markup when \*escaped or unbalanced *.

What about *this**?
Unbalanced _markup__ is kept as-is without warning.
""",
"""\
<document source="test data">
    <paragraph>
        No markup when *escaped or unbalanced *.
    <paragraph>
        What about \n\
        <emphasis>
            this
        *?
        Unbalanced \n\
        <emphasis>
            markup
        _ is kept as-is without warning.
"""],
[r"""
Emphasized asterisk: *\**

Emphasized double asterisk: *\*\**
""",
"""\
<document source="test data">
    <paragraph>
        Emphasized asterisk: \n\
        <emphasis>
            *
    <paragraph>
        Emphasized double asterisk: \n\
        <emphasis>
            **
"""],
]

totest['strong'] = [
["""\
**strong**
__also strong__
""",
"""\
<document source="test data">
    <paragraph>
        <strong>
            strong
        \n\
        <strong>
            also strong
"""],
["""\
Strong asterisk must be escaped **\\***

Strong double asterisk: **\\*\\***
""",
"""\
<document source="test data">
    <paragraph>
        Strong asterisk must be escaped \n\
        <strong>
            *
    <paragraph>
        Strong double asterisk: \n\
        <strong>
            **
"""],
["""\
**not strong without closing asterisks
""",
"""\
<document source="test data">
    <paragraph>
        **not strong without closing asterisks
"""],
]

totest['literal'] = [
["""\
Inline `literals` are called `code spans` in CommonMark.
""",
"""\
<document source="test data">
    <paragraph>
        Inline \n\
        <literal classes="code">
            literals
         are called \n\
        <literal classes="code">
            code spans
         in CommonMark.
"""],
[r"""
`\*literal`
""",
"""\
<document source="test data">
    <paragraph>
        <literal classes="code">
            \\*literal
"""],
[r"""
``lite\ral``
""",
"""\
<document source="test data">
    <paragraph>
        <literal classes="code">
            lite\\ral
"""],
[r"""
``literal\``
""",
"""\
<document source="test data">
    <paragraph>
        <literal classes="code">
            literal\\
"""],
[u"""\
l'``literal`` and l\u2019``literal`` with apostrophe
""",
u"""\
<document source="test data">
    <paragraph>
        l'
        <literal classes="code">
            literal
         and l\u2019
        <literal classes="code">
            literal
         with apostrophe
"""],
[u"""\
quoted '``literal``', quoted "``literal``",
quoted \u2018``literal``\u2019, quoted \u201c``literal``\u201d,
quoted \xab``literal``\xbb
""",
u"""\
<document source="test data">
    <paragraph>
        quoted '
        <literal classes="code">
            literal
        ', quoted "
        <literal classes="code">
            literal
        ",
        quoted \u2018
        <literal classes="code">
            literal
        \u2019, quoted \u201c
        <literal classes="code">
            literal
        \u201d,
        quoted \xab
        <literal classes="code">
            literal
        \xbb
"""],
[u"""\
``'literal'`` with quotes, ``"literal"`` with quotes,
``\u2018literal\u2019`` with quotes, ``\u201cliteral\u201d`` with quotes,
``\xabliteral\xbb`` with quotes
""",
u"""\
<document source="test data">
    <paragraph>
        <literal classes="code">
            'literal'
         with quotes, \n\
        <literal classes="code">
            "literal"
         with quotes,
        <literal classes="code">
            \u2018literal\u2019
         with quotes, \n\
        <literal classes="code">
            \u201cliteral\u201d
         with quotes,
        <literal classes="code">
            \xabliteral\xbb
         with quotes
"""],
[r"""
``literal ``no literal

No warning for `standalone TeX quotes' or other *unbalanced markup**.
""",
"""\
<document source="test data">
    <paragraph>
        <literal classes="code">
            literal
        no literal
    <paragraph>
        No warning for `standalone TeX quotes\' or other \n\
        <emphasis>
            unbalanced markup
        *.
"""],
["""\
``not literal without closing backquotes
""",
"""\
<document source="test data">
    <paragraph>
        ``not literal without closing backquotes
"""],
[r"""
Python ``list``s use square bracket syntax.
""",
"""\
<document source="test data">
    <paragraph>
        Python \n\
        <literal classes="code">
            list
        s use square bracket syntax.
"""],
[r"""
Blank after opening `` not allowed.
""",
"""\
<document source="test data">
    <paragraph>
        Blank after opening `` not allowed.
"""],
[r"""
no blank ``after closing``still ends a literal.
""",
"""\
<document source="test data">
    <paragraph>
        no blank \n\
        <literal classes="code">
            after closing
        still ends a literal.
"""],
]

totest['references'] = [
["""\
[ref]

[ref]: /uri
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="ref" refuri="/uri">
            ref
"""],
["""\
Inline image ![foo *bar*][foobar]
in a paragraph.

[FOOBAR]: train.jpg "train & tracks"
""",
"""\
<document source="test data">
    <paragraph>
        Inline image \n\
        <image alt="foo " title="train & tracks" uri="train.jpg">
        \n\
        in a paragraph.
"""],
["""\
[phrase reference]

[phrase reference]: /uri
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="phrase reference" refuri="/uri">
            phrase reference
"""],
[u"""\
No whitespace required around a[phrase reference].

[phrase reference]: /uri
""",
u"""\
<document source="test data">
    <paragraph>
        No whitespace required around a
        <reference name="phrase reference" refuri="/uri">
            phrase reference
        .
"""],
["""\
[phrase reference
across lines]

[phrase reference across lines]: /uri
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="phrase referenceacross lines" refuri="/uri">
            phrase reference
            across lines
"""],
]

totest['appended_URIs'] = [
["""\
[anonymous reference](http://example.com)
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="anonymous reference" refuri="http://example.com">
            anonymous reference
"""],
["""\
Inline image ![a train](train.jpg) more text.
""",
"""\
<document source="test data">
    <paragraph>
        Inline image \n\
        <image alt="a train" uri="train.jpg">
         more text.
"""],
["""\
Inline image ![foo](/url "title") more text.
""",
"""\
<document source="test data">
    <paragraph>
        Inline image \n\
        <image alt="foo" title="title" uri="/url">
         more text.
"""],
["""\
[URI must follow immediately]
(http://example.com)
""",
"""\
<document source="test data">
    <paragraph>
        [URI must follow immediately]
        (http://example.com)
"""],
["""\
Relative URIs' reference text can't be omitted:

[reference](reference)
""",
"""\
<document source="test data">
    <paragraph>
        Relative URIs' reference text can't be omitted:
    <paragraph>
        <reference name="reference" refuri="reference">
            reference
"""],
]

totest['standalone hyperlink'] = [
["""\
CommonMark calls standalone hyperlinks
like <http://example.com> "autolinks".
""",
"""\
<document source="test data">
    <paragraph>
        CommonMark calls standalone hyperlinks
        like \n\
        <reference name="http://example.com" refuri="http://example.com">
            http://example.com
         "autolinks".
"""],
]

totest['raw HTML'] = [
["""\
foo <a href="uri"> bar
""",
"""\
<document source="test data">
    <paragraph>
        foo \n\
        <raw format="html" xml:space="preserve">
            <a href="uri">
         bar
"""],
["""\
foo <br /> bar
and <!-- this is an inline
comment - with hyphen -->
""",
"""\
<document source="test data">
    <paragraph>
        foo \n\
        <raw format="html" xml:space="preserve">
            <br />
         bar
        and \n\
        <raw format="html" xml:space="preserve">
            <!-- this is an inline
            comment - with hyphen -->
"""],
["""\
Hard line breaks are not supported by Docutils.
Not the soft line break preceded by two or more spaces,  \n\
nor the more visible alternative,\\
a backslash before the line ending.
""",
"""\
<document source="test data">
    <paragraph>
        Hard line breaks are not supported by Docutils.
        Not the soft line break preceded by two or more spaces,\
nor the more visible alternative,\
a backslash before the line ending.
"""],
]

totest['markup recognition rules'] = [
[r"""
Character-level m*a***r**`k`_u_p
works except for underline.
""",
"""\
<document source="test data">
    <paragraph>
        Character-level m
        <emphasis>
            a
        <strong>
            r
        <literal classes="code">
            k
        _u_p
        works except for underline.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
