#! /usr/bin/env python3
# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for inline markup in docutils/parsers/rst/states.py
with the "character-level-inline-markup" setting.

Experimental.
"""

import unittest

from test import DocutilsTestSupport  # NoQA: F401

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.utils import new_document


class ParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.character_level_inline_markup = True
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(output, case_expected)


totest = {}

totest['emphasis'] = [
[r"""some punctuation is allowed around inline markup, e.g.
/*emphasis*/, -*emphasis*-, and :*emphasis*: (delimiters),
(*emphasis*), [*emphasis*], <*emphasis*>, {*emphasis*} (open/close pairs)
*emphasis*., *emphasis*,, *emphasis*!, and *emphasis*\ (closing delimiters),

With simple-inline-markup also
)*emphasis*(, ]*emphasis*[, >*emphasis*>, }*emphasis*{ (close/open pairs),
x*2* or 2*x* (alphanumeric char before),

but not
(*), [*], '*' or '"*"' ("quoted" start-string),
\*args or * (escaped; whitespace behind start-string),
or *the\* *stars\* *inside* (escaped; whitespace before end-string).

However, '*args' triggers a warning.

Also *this**.
""",
"""\
<document source="test data">
    <paragraph>
        some punctuation is allowed around inline markup, e.g.
        /
        <emphasis>
            emphasis
        /, -
        <emphasis>
            emphasis
        -, and :
        <emphasis>
            emphasis
        : (delimiters),
        (
        <emphasis>
            emphasis
        ), [
        <emphasis>
            emphasis
        ], <
        <emphasis>
            emphasis
        >, {
        <emphasis>
            emphasis
        } (open/close pairs)
        <emphasis>
            emphasis
        ., \n\
        <emphasis>
            emphasis
        ,, \n\
        <emphasis>
            emphasis
        !, and \n\
        <emphasis>
            emphasis
        (closing delimiters),
    <paragraph>
        With simple-inline-markup also
        )
        <emphasis>
            emphasis
        (, ]
        <emphasis>
            emphasis
        [, >
        <emphasis>
            emphasis
        >, }
        <emphasis>
            emphasis
        { (close/open pairs),
        x
        <emphasis>
            2
         or 2
        <emphasis>
            x
         (alphanumeric char before),
    <paragraph>
        but not
        (*), [*], '*' or '"*"' ("quoted" start-string),
        *args or * (escaped; whitespace behind start-string),
        or \n\
        <emphasis>
            the* *stars* *inside
         (escaped; whitespace before end-string).
    <paragraph>
        However, '
        <problematic ids="problematic-1" refid="system-message-1">
            *
        args' triggers a warning.
    <system_message backrefs="problematic-1" ids="system-message-1" level="2" line="15" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
    <paragraph>
        Also \n\
        <emphasis>
            this
        <problematic ids="problematic-2" refid="system-message-2">
            *
        .
    <system_message backrefs="problematic-2" ids="system-message-2" level="2" line="17" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
"""],
[r"""
Emphasized asterisk: *\**

Emphasized double asterisk: *\*\** (requires two escape chars).
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
         (requires two escape chars).
"""],
]

totest['strong'] = [
[r"""
(**strong**) but not (**) or '(** '

However, '**kwargs' and x**2 are recognized as strong markup
and \**kwargs or ** as emphasized.
""",
"""\
<document source="test data">
    <paragraph>
        (
        <strong>
            strong
        ) but not (**) or '(** '
    <paragraph>
        However, '
        <strong>
            kwargs' and x
        2 are recognized as strong markup
        and *
        <emphasis>
            kwargs or *
         as emphasized.
"""],
["""\
Strong asterisk: **\\***
and
strong double asterisk: **\\*\\***
require escaping with simple-inline-markup.
""",
"""\
<document source="test data">
    <paragraph>
        Strong asterisk: \n\
        <strong>
            *
        \n\
        and
        strong double asterisk: \n\
        <strong>
            **
        \n\
        require escaping with simple-inline-markup.
"""],
]

totest['literal'] = [
["""\
With simple-inline-markup, this is ```interpreted text``` in backquotes!
""",
"""\
<document source="test data">
    <paragraph>
        With simple-inline-markup, this is \n\
        <literal>
            `interpreted text
        ` in backquotes!
"""],
["""\
``literal without closing backquotes
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            ``
        literal without closing backquotes
    <system_message backrefs="problematic-1" ids="system-message-1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline literal start-string without end-string.
"""],
[r"""
Python ``list``s use square bracket syntax.
""",
"""\
<document source="test data">
    <paragraph>
        Python \n\
        <literal>
            list
        s use square bracket syntax.
"""],
]

totest['references'] = [
["""\
ref_, r_, r_e-f_, -ref_, and anonymousref__,
beware of _ref_ or __attr__ or object.__attr__
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="ref" refname="ref">
            ref
        , \n\
        <reference name="r" refname="r">
            r
        , \n\
        <reference name="r_e-f" refname="r_e-f">
            r_e-f
        , -
        <reference name="ref" refname="ref">
            ref
        , and \n\
        <reference anonymous="1" name="anonymousref">
            anonymousref
        ,
        beware of _
        <reference name="ref" refname="ref">
            ref
         or __
        <reference anonymous="1" name="attr">
            attr
         or object.__
        <reference anonymous="1" name="attr">
            attr
"""],
]

totest['embedded_uris'] = [
[r"""
Escape chars in URIs:

`<reference\:1>`_

`<anonymous\\call>`__

`<anonymous\_call>`__
""",
"""\
<document source="test data">
    <paragraph>
        Escape chars in URIs:
    <paragraph>
        <reference name="reference:1" refuri="reference:1">
            reference:1
        <target ids="reference-1" names="reference:1" refuri="reference:1">
    <paragraph>
        <reference name="anonymous\\call" refuri="anonymous\\call">
            anonymous\\call
    <paragraph>
        <reference name="anonymous_call" refuri="anonymous_call">
            anonymous_call
"""],
]

totest['inline_targets'] = [
["""\
This isn't a _target; targets require backquotes.

With simple-inline-markup, _`this`_ is a a target followed by an
underscore.
""",
"""\
<document source="test data">
    <paragraph>
        This isn't a _target; targets require backquotes.
    <paragraph>
        With simple-inline-markup, \n\
        <target ids="this" names="this">
            this
        _ is a a target followed by an
        underscore.
"""],
]

totest['footnote_reference'] = [
["""\
Adjacent footnote refs are possible with simple-inline-markup:
[*]_[#label]_ [#]_[2]_ [1]_[*]_

.. [#] test1
.. [*] test2
""",
"""\
<document source="test data">
    <paragraph>
        Adjacent footnote refs are possible with simple-inline-markup:
        <footnote_reference auto="*" ids="footnote-reference-1">
        <footnote_reference auto="1" ids="footnote-reference-2" refname="label">
         \n\
        <footnote_reference auto="1" ids="footnote-reference-3">
        <footnote_reference ids="footnote-reference-4" refname="2">
            2
         \n\
        <footnote_reference ids="footnote-reference-5" refname="1">
            1
        <footnote_reference auto="*" ids="footnote-reference-6">
    <footnote auto="1" ids="footnote-1">
        <paragraph>
            test1
    <footnote auto="*" ids="footnote-2">
        <paragraph>
            test2
"""],
]

totest['citation_reference'] = [
["""\
Adjacent citation refs are possible with simple-inline-markup:
[citation]_[CIT1]_
""",
"""\
<document source="test data">
    <paragraph>
        Adjacent citation refs are possible with simple-inline-markup:
        <citation_reference ids="citation-reference-1" refname="citation">
            citation
        <citation_reference ids="citation-reference-2" refname="cit1">
            CIT1
"""],
]

totest['substitution_references'] = [
["""\
|sub|ref
""",
"""\
<document source="test data">
    <paragraph>
        <substitution_reference refname="sub">
            sub
        ref
"""],
]

totest['standalone_hyperlink'] = [
[r"""
Valid URLs with escaped markup characters:

http://example.com/\*content\*/whatever

Invalid with the simple-inline-markup setting:

http://example.com/\*content*/whatever
http://example.com/rST_for_all.html
""",
"""\
<document source="test data">
    <paragraph>
        Valid URLs with escaped markup characters:
    <paragraph>
        <reference refuri="http://example.com/*content*/whatever">
            http://example.com/*content*/whatever
    <paragraph>
        Invalid with the simple-inline-markup setting:
    <paragraph>
        <reference refuri="http://example.com/*content">
            http://example.com/*content
        <problematic ids="problematic-1" refid="system-message-1">
            *
        /whatever
        <reference refuri="http://example.com/">
            http://example.com/
        <reference name="rST_for" refname="rst_for">
            rST_for
        all.html
    <system_message backrefs="problematic-1" ids="system-message-1" level="2" line="8" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
"""],
]

totest['markup_recognition_rules'] = [
["""\
__This__ is an anonymous reference with simple-inline-markup.
""",
"""\
<document source="test data">
    <paragraph>
        __
        <reference anonymous="1" name="This">
            This
         is an anonymous reference with simple-inline-markup.
"""],
[r"""
Character-level m*a***r**``k``\ `u`:title:\p
with backslash-escaped whitespace, including new\
lines.
""",
"""\
<document source="test data">
    <paragraph>
        Character-level m
        <emphasis>
            a
        <strong>
            r
        <literal>
            k
        <title_reference>
            u
        p
        with backslash-escaped whitespace, including newlines.
"""],
["""\
text-*separated*\u2010*by*\u2011*various*\u2012*dashes*\u2013*and*\u2014*hyphens*.
\u00bf*punctuation*? \u00a1*examples*!\xa0*no-break-space*\xa0.
""",
"""\
<document source="test data">
    <paragraph>
        text-
        <emphasis>
            separated
        \u2010
        <emphasis>
            by
        \u2011
        <emphasis>
            various
        \u2012
        <emphasis>
            dashes
        \u2013
        <emphasis>
            and
        \u2014
        <emphasis>
            hyphens
        .
        \xbf
        <emphasis>
            punctuation
        ? \xa1
        <emphasis>
            examples
        !\xa0
        <emphasis>
            no-break-space
        \xa0.
"""],
# Whitespace characters:
["""\
inline markup surrounded by various whitespace characters:
*newline*
or *space* or one of
\xa0*NO-BREAK SPACE*\xa0,
\u1680*OGHAM SPACE MARK*\u1680,
\u180e*MONGOLIAN VOWEL SEPARATOR*\u180e,
\u2000*EN QUAD*\u2000,
\u2001*EM QUAD*\u2001,
\u2002*EN SPACE*\u2002,
\u2003*EM SPACE*\u2003,
\u2004*THREE-PER-EM SPACE*\u2004,
\u2005*FOUR-PER-EM SPACE*\u2005,
\u2006*SIX-PER-EM SPACE*\u2006,
\u2007*FIGURE SPACE*\u2007,
\u2008*PUNCTUATION SPACE*\u2008,
\u2009*THIN SPACE*\u2009,
\u200a*HAIR SPACE*\u200a,
\u202f*NARROW NO-BREAK SPACE*\u202f,
\u205f*MEDIUM MATHEMATICAL SPACE*\u205f,
\u3000*IDEOGRAPHIC SPACE*\u3000,
\u2028*LINE SEPARATOR*\u2028
""",
"""\
<document source="test data">
    <paragraph>
        inline markup surrounded by various whitespace characters:
        <emphasis>
            newline
        \n\
        or \n\
        <emphasis>
            space
         or one of
        \xa0
        <emphasis>
            NO-BREAK SPACE
        \xa0,
        \u1680
        <emphasis>
            OGHAM SPACE MARK
        \u1680,
        \u180e
        <emphasis>
            MONGOLIAN VOWEL SEPARATOR
        \u180e,
        \u2000
        <emphasis>
            EN QUAD
        \u2000,
        \u2001
        <emphasis>
            EM QUAD
        \u2001,
        \u2002
        <emphasis>
            EN SPACE
        \u2002,
        \u2003
        <emphasis>
            EM SPACE
        \u2003,
        \u2004
        <emphasis>
            THREE-PER-EM SPACE
        \u2004,
        \u2005
        <emphasis>
            FOUR-PER-EM SPACE
        \u2005,
        \u2006
        <emphasis>
            SIX-PER-EM SPACE
        \u2006,
        \u2007
        <emphasis>
            FIGURE SPACE
        \u2007,
        \u2008
        <emphasis>
            PUNCTUATION SPACE
        \u2008,
        \u2009
        <emphasis>
            THIN SPACE
        \u2009,
        \u200a
        <emphasis>
            HAIR SPACE
        \u200a,
        \u202f
        <emphasis>
            NARROW NO-BREAK SPACE
        \u202f,
        \u205f
        <emphasis>
            MEDIUM MATHEMATICAL SPACE
        \u205f,
        \u3000
        <emphasis>
            IDEOGRAPHIC SPACE
        \u3000,
    <paragraph>
        <emphasis>
            LINE SEPARATOR
"""],
["""\
no inline markup due to whitespace inside and behind: *
newline
*
* space * or one of
*\xa0NO-BREAK SPACE\xa0*
*\u1680OGHAM SPACE MARK\u1680*
*\u2000EN QUAD\u2000*
*\u2001EM QUAD\u2001*
*\u2002EN SPACE\u2002*
*\u2003EM SPACE\u2003*
*\u2004THREE-PER-EM SPACE\u2004*
*\u2005FOUR-PER-EM SPACE\u2005*
*\u2006SIX-PER-EM SPACE\u2006*
*\u2007FIGURE SPACE\u2007*
*\u2008PUNCTUATION SPACE\u2008*
*\u2009THIN SPACE\u2009*
*\u200aHAIR SPACE\u200a*
*\u202fNARROW NO-BREAK SPACE\u202f*
*\u205fMEDIUM MATHEMATICAL SPACE\u205f*
*\u3000IDEOGRAPHIC SPACE\u3000*
*\u2028LINE SEPARATOR\u2028*
""",
"""\
<document source="test data">
    <paragraph>
        no inline markup due to whitespace inside and behind: *
        newline
        *
        * space * or one of
        *\xa0NO-BREAK SPACE\xa0*
        *\u1680OGHAM SPACE MARK\u1680*
        *\u2000EN QUAD\u2000*
        *\u2001EM QUAD\u2001*
        *\u2002EN SPACE\u2002*
        *\u2003EM SPACE\u2003*
        *\u2004THREE-PER-EM SPACE\u2004*
        *\u2005FOUR-PER-EM SPACE\u2005*
        *\u2006SIX-PER-EM SPACE\u2006*
        *\u2007FIGURE SPACE\u2007*
        *\u2008PUNCTUATION SPACE\u2008*
        *\u2009THIN SPACE\u2009*
        *\u200aHAIR SPACE\u200a*
        *\u202fNARROW NO-BREAK SPACE\u202f*
        *\u205fMEDIUM MATHEMATICAL SPACE\u205f*
        *\u3000IDEOGRAPHIC SPACE\u3000*
        *
        LINE SEPARATOR
        *
"""],
# « * » ‹ * › « * » ‹ * › « * » ‹ * › French,
["""\
"Quoted" markup start-string (matched openers & closers) -> no markup:

'*' "*" (*) <*> [*] {*}
⁅*⁆

Some international quoting styles:
‘*’ “*” English, ...,
„*“ ‚*‘ »*« ›*‹ German, Czech, ...,
„*” «*» Romanian,
“*„ ‘*‚ Greek,
「*」 『*』traditional Chinese,
”*” ’*’ »*» ›*› Swedish, Finnish,
„*” ‚*’ Polish,
„*” »*« ’*’ Hungarian,

But this is „*’ emphasized »*‹.
""",
"""\
<document source="test data">
    <paragraph>
        "Quoted" markup start-string (matched openers & closers) -> no markup:
    <paragraph>
        '*' "*" (*) <*> [*] {*}
        ⁅*⁆
    <paragraph>
        Some international quoting styles:
        ‘*’ “*” English, ...,
        „*“ ‚*‘ »*« ›*‹ German, Czech, ...,
        „*” «*» Romanian,
        “*„ ‘*‚ Greek,
        「*」 『*』traditional Chinese,
        ”*” ’*’ »*» ›*› Swedish, Finnish,
        „*” ‚*’ Polish,
        „*” »*« ’*’ Hungarian,
    <paragraph>
        But this is „
        <emphasis>
            ’ emphasized »
        ‹.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main()
