#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for states.py.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['emphasis'] = [
["""\
*emphasis*
""",
"""\
<document source="test data">
    <paragraph>
        <emphasis>
            emphasis
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
*emphasis without closing asterisk
""",
"""\
<document source="test data">
    <paragraph>
        <problematic id="id2" refid="id1">
            *
        emphasis without closing asterisk
    <system_message backrefs="id2" id="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
"""],
["""\
'*emphasis*' and 1/*emphasis*/2 and 3-*emphasis*-4 and 5:*emphasis*:6
but not '*' or '"*"' or  x*2* or 2*x* or \\*args or *
or *the\\* *stars\\\\\\* *inside*

(however, '*args' will trigger a warning and may be problematic)

what about *this**?
""",
"""\
<document source="test data">
    <paragraph>
        '
        <emphasis>
            emphasis
        ' and 1/
        <emphasis>
            emphasis
        /2 and 3-
        <emphasis>
            emphasis
        -4 and 5:
        <emphasis>
            emphasis
        :6
        but not '*' or '"*"' or  x*2* or 2*x* or *args or *
        or \n\
        <emphasis>
            the* *stars\* *inside
    <paragraph>
        (however, '
        <problematic id="id2" refid="id1">
            *
        args' will trigger a warning and may be problematic)
    <system_message backrefs="id2" id="id1" level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
    <paragraph>
        what about \n\
        <emphasis>
            this*
        ?
"""],
["""\
Emphasized asterisk: *\\**

Emphasized double asterisk: *\\***
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
""",
"""\
<document source="test data">
    <paragraph>
        <strong>
            strong
"""],
["""\
(**strong**) but not (**) or '(** ' or x**2 or \\**kwargs or **

(however, '**kwargs' will trigger a warning and may be problematic)
""",
"""\
<document source="test data">
    <paragraph>
        (
        <strong>
            strong
        ) but not (**) or '(** ' or x**2 or **kwargs or **
    <paragraph>
        (however, '
        <problematic id="id2" refid="id1">
            **
        kwargs' will trigger a warning and may be problematic)
    <system_message backrefs="id2" id="id1" level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Inline strong start-string without end-string.
"""],
["""\
Strong asterisk: *****

Strong double asterisk: ******
""",
"""\
<document source="test data">
    <paragraph>
        Strong asterisk: \n\
        <strong>
            *
    <paragraph>
        Strong double asterisk: \n\
        <strong>
            **
"""],
["""\
**strong without closing asterisks
""",
"""\
<document source="test data">
    <paragraph>
        <problematic id="id2" refid="id1">
            **
        strong without closing asterisks
    <system_message backrefs="id2" id="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline strong start-string without end-string.
"""],
]

totest['literal'] = [
["""\
``literal``
""",
"""\
<document source="test data">
    <paragraph>
        <literal>
            literal
"""],
["""\
``\\literal``
""",
"""\
<document source="test data">
    <paragraph>
        <literal>
            \\literal
"""],
["""\
``lite\\ral``
""",
"""\
<document source="test data">
    <paragraph>
        <literal>
            lite\\ral
"""],
["""\
``literal\\``
""",
"""\
<document source="test data">
    <paragraph>
        <literal>
            literal\\
"""],
["""\
``literal ``TeX quotes'' & \\backslash`` but not "``" or ``

(however, ``standalone TeX quotes'' will trigger a warning
and may be problematic)
""",
"""\
<document source="test data">
    <paragraph>
        <literal>
            literal ``TeX quotes'' & \\backslash
         but not "``" or ``
    <paragraph>
        (however, \n\
        <problematic id="id2" refid="id1">
            ``
        standalone TeX quotes'' will trigger a warning
        and may be problematic)
    <system_message backrefs="id2" id="id1" level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Inline literal start-string without end-string.
"""],
["""\
Find the ```interpreted text``` in this paragraph!
""",
"""\
<document source="test data">
    <paragraph>
        Find the \n\
        <literal>
            `interpreted text`
         in this paragraph!
"""],
["""\
``literal without closing backquotes
""",
"""\
<document source="test data">
    <paragraph>
        <problematic id="id2" refid="id1">
            ``
        literal without closing backquotes
    <system_message backrefs="id2" id="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline literal start-string without end-string.
"""],
]

totest['interpreted'] = [
["""\
`interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <interpreted>
            interpreted
"""],
["""\
:role:`interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <interpreted position="prefix" role="role">
            interpreted
"""],
["""\
`interpreted`:role:
""",
"""\
<document source="test data">
    <paragraph>
        <interpreted position="suffix" role="role">
            interpreted
"""],
["""\
:role:`interpreted`:role:
""",
"""\
<document source="test data">
    <paragraph>
        <problematic id="id2" refid="id1">
            :role:`interpreted`:role:
    <system_message backrefs="id2" id="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Multiple roles in interpreted text (both prefix and suffix present; only one allowed).
"""],
["""\
:role:`:not-role: interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <interpreted position="prefix" role="role">
            :not-role: interpreted
"""],
["""\
:very.long-role_name:`interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <interpreted position="prefix" role="very.long-role_name">
            interpreted
"""],
["""\
`interpreted` but not \\`interpreted` [`] or ({[`] or [`]}) or `
""",
"""\
<document source="test data">
    <paragraph>
        <interpreted>
            interpreted
         but not `interpreted` [`] or ({[`] or [`]}) or `
"""],
["""\
`interpreted`-text `interpreted`: text `interpreted`:text `text`'s interpreted
""",
"""\
<document source="test data">
    <paragraph>
        <interpreted>
            interpreted
        -text \n\
        <interpreted>
            interpreted
        : text \n\
        <interpreted>
            interpreted
        :text \n\
        <interpreted>
            text
        's interpreted
"""],
["""\
`interpreted without closing backquote
""",
"""\
<document source="test data">
    <paragraph>
        <problematic id="id2" refid="id1">
            `
        interpreted without closing backquote
    <system_message backrefs="id2" id="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline interpreted text or phrase reference start-string without end-string.
"""],
]

totest['references'] = [
["""\
ref_
""",
"""\
<document source="test data">
    <paragraph>
        <reference refname="ref">
            ref
"""],
["""\
ref__
""",
"""\
<document source="test data">
    <paragraph>
        <reference anonymous="1">
            ref
"""],
["""\
ref_, r_, r_e-f_, -ref_, and anonymousref__,
but not _ref_ or __attr__ or object.__attr__
""",
"""\
<document source="test data">
    <paragraph>
        <reference refname="ref">
            ref
        , \n\
        <reference refname="r">
            r
        , \n\
        <reference refname="r_e-f">
            r_e-f
        , -
        <reference refname="ref">
            ref
        , and \n\
        <reference anonymous="1">
            anonymousref
        ,
        but not _ref_ or __attr__ or object.__attr__
"""],
]

totest['phrase_references'] = [
["""\
`phrase reference`_
""",
"""\
<document source="test data">
    <paragraph>
        <reference refname="phrase reference">
            phrase reference
"""],
["""\
`anonymous reference`__
""",
"""\
<document source="test data">
    <paragraph>
        <reference anonymous="1">
            anonymous reference
"""],
["""\
`phrase reference
across lines`_
""",
"""\
<document source="test data">
    <paragraph>
        <reference refname="phrase reference across lines">
            phrase reference
            across lines
"""],
["""\
`phrase\`_ reference`_
""",
"""\
<document source="test data">
    <paragraph>
        <reference refname="phrase`_ reference">
            phrase`_ reference
"""],
["""\
Invalid phrase reference:

:role:`phrase reference`_
""",
"""\
<document source="test data">
    <paragraph>
        Invalid phrase reference:
    <paragraph>
        <problematic id="id2" refid="id1">
            :role:`phrase reference`_
    <system_message backrefs="id2" id="id1" level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Mismatch: both interpreted text role prefix and reference suffix.
"""],
["""\
Invalid phrase reference:

`phrase reference`:role:_
""",
"""\
<document source="test data">
    <paragraph>
        Invalid phrase reference:
    <paragraph>
        <problematic id="id2" refid="id1">
            `phrase reference`:role:_
    <system_message backrefs="id2" id="id1" level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Mismatch: both interpreted text role suffix and reference suffix.
"""],
["""\
`phrase reference_ without closing backquote
""",
"""\
<document source="test data">
    <paragraph>
        <problematic id="id2" refid="id1">
            `
        phrase \n\
        <reference refname="reference">
            reference
         without closing backquote
    <system_message backrefs="id2" id="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline interpreted text or phrase reference start-string without end-string.
"""],
["""\
`anonymous phrase reference__ without closing backquote
""",
"""\
<document source="test data">
    <paragraph>
        <problematic id="id2" refid="id1">
            `
        anonymous phrase \n\
        <reference anonymous="1">
            reference
         without closing backquote
    <system_message backrefs="id2" id="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline interpreted text or phrase reference start-string without end-string.
"""],
]

totest['inline_targets'] = [
["""\
_`target`

Here is _`another target` in some text. And _`yet
another target`, spanning lines.

_`Here is  a    TaRgeT` with case and spacial difficulties.
""",
"""\
<document source="test data">
    <paragraph>
        <target id="target" name="target">
            target
    <paragraph>
        Here is \n\
        <target id="another-target" name="another target">
            another target
         in some text. And \n\
        <target id="yet-another-target" name="yet another target">
            yet
            another target
        , spanning lines.
    <paragraph>
        <target id="here-is-a-target" name="here is a target">
            Here is  a    TaRgeT
         with case and spacial difficulties.
"""],
["""\
But this isn't a _target; targets require backquotes.

And _`this`_ is just plain confusing.
""",
"""\
<document source="test data">
    <paragraph>
        But this isn't a _target; targets require backquotes.
    <paragraph>
        And \n\
        <problematic id="id2" refid="id1">
            _`
        this`_ is just plain confusing.
    <system_message backrefs="id2" id="id1" level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Inline target start-string without end-string.
"""],
["""\
_`inline target without closing backquote
""",
"""\
<document source="test data">
    <paragraph>
        <problematic id="id2" refid="id1">
            _`
        inline target without closing backquote
    <system_message backrefs="id2" id="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline target start-string without end-string.
"""],
]

totest['footnote_reference'] = [
["""\
[1]_
""",
"""\
<document source="test data">
    <paragraph>
        <footnote_reference id="id1" refname="1">
            1
"""],
["""\
[#]_
""",
"""\
<document source="test data">
    <paragraph>
        <footnote_reference auto="1" id="id1">
"""],
["""\
[#label]_
""",
"""\
<document source="test data">
    <paragraph>
        <footnote_reference auto="1" id="id1" refname="label">
"""],
["""\
[*]_
""",
"""\
<document source="test data">
    <paragraph>
        <footnote_reference auto="*" id="id1">
"""],
]

totest['citation_reference'] = [
["""\
[citation]_
""",
"""\
<document source="test data">
    <paragraph>
        <citation_reference id="id1" refname="citation">
            citation
"""],
["""\
[citation]_ and [cit-ation]_ and [cit.ation]_ and [CIT1]_ but not [CIT 1]_
""",
"""\
<document source="test data">
    <paragraph>
        <citation_reference id="id1" refname="citation">
            citation
         and \n\
        <citation_reference id="id2" refname="cit-ation">
            cit-ation
         and \n\
        <citation_reference id="id3" refname="cit.ation">
            cit.ation
         and \n\
        <citation_reference id="id4" refname="cit1">
            CIT1
         but not [CIT 1]_
"""],
]

totest['substitution_references'] = [
["""\
|subref|
""",
"""\
<document source="test data">
    <paragraph>
        <substitution_reference refname="subref">
            subref
"""],
["""\
|subref|_ and |subref|__
""",
"""\
<document source="test data">
    <paragraph>
        <reference refname="subref">
            <substitution_reference refname="subref">
                subref
         and \n\
        <reference anonymous="1">
            <substitution_reference refname="subref">
                subref
"""],
["""\
|substitution reference|
""",
"""\
<document source="test data">
    <paragraph>
        <substitution_reference refname="substitution reference">
            substitution reference
"""],
["""\
|substitution
reference|
""",
"""\
<document source="test data">
    <paragraph>
        <substitution_reference refname="substitution reference">
            substitution
            reference
"""],
["""\
|substitution reference without closing verbar
""",
"""\
<document source="test data">
    <paragraph>
        <problematic id="id2" refid="id1">
            |
        substitution reference without closing verbar
    <system_message backrefs="id2" id="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline substitution_reference start-string without end-string.
"""],
["""\
| and || and |||
""",
"""\
<document source="test data">
    <paragraph>
        | and || and |||
"""],
]

totest['standalone_hyperlink'] = [
["""\
http://www.standalone.hyperlink.com

http:/one-slash-only.absolute.path

http://[1080:0:0:0:8:800:200C:417A]/IPv6address.html

http://[3ffe:2a00:100:7031::1]

mailto:someone@somewhere.com

news:comp.lang.python

An email address in a sentence: someone@somewhere.com.

ftp://ends.with.a.period.

(a.question.mark@end?)
""",
"""\
<document source="test data">
    <paragraph>
        <reference refuri="http://www.standalone.hyperlink.com">
            http://www.standalone.hyperlink.com
    <paragraph>
        <reference refuri="http:/one-slash-only.absolute.path">
            http:/one-slash-only.absolute.path
    <paragraph>
        <reference refuri="http://[1080:0:0:0:8:800:200C:417A]/IPv6address.html">
            http://[1080:0:0:0:8:800:200C:417A]/IPv6address.html
    <paragraph>
        <reference refuri="http://[3ffe:2a00:100:7031::1]">
            http://[3ffe:2a00:100:7031::1]
    <paragraph>
        <reference refuri="mailto:someone@somewhere.com">
            mailto:someone@somewhere.com
    <paragraph>
        <reference refuri="news:comp.lang.python">
            news:comp.lang.python
    <paragraph>
        An email address in a sentence: \n\
        <reference refuri="mailto:someone@somewhere.com">
            someone@somewhere.com
        .
    <paragraph>
        <reference refuri="ftp://ends.with.a.period">
            ftp://ends.with.a.period
        .
    <paragraph>
        (
        <reference refuri="mailto:a.question.mark@end">
            a.question.mark@end
        ?)
"""],
["""\
None of these are standalone hyperlinks (their "schemes"
are not recognized): signal:noise, a:b.
""",
"""\
<document source="test data">
    <paragraph>
        None of these are standalone hyperlinks (their "schemes"
        are not recognized): signal:noise, a:b.
"""],
]

totest['miscellaneous'] = [
["""\
__This__ should be left alone.
""",
"""\
<document source="test data">
    <paragraph>
        __This__ should be left alone.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
