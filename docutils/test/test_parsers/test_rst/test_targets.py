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

totest['targets'] = [
["""\
.. _target:

(Internal hyperlink target.)
""",
"""\
<document source="test data">
    <target id="target" name="target">
    <paragraph>
        (Internal hyperlink target.)
"""],
["""\
External hyperlink targets:

.. _one-liner: http://structuredtext.sourceforge.net

.. _starts-on-this-line: http://
                         structuredtext.
                         sourceforge.net

.. _entirely-below:
   http://structuredtext.
   sourceforge.net

.. _not-indirect: uri\_
""",
"""\
<document source="test data">
    <paragraph>
        External hyperlink targets:
    <target id="one-liner" name="one-liner" refuri="http://structuredtext.sourceforge.net">
    <target id="starts-on-this-line" name="starts-on-this-line" refuri="http://structuredtext.sourceforge.net">
    <target id="entirely-below" name="entirely-below" refuri="http://structuredtext.sourceforge.net">
    <target id="not-indirect" name="not-indirect" refuri="uri_">
"""],
["""\
Indirect hyperlink targets:

.. _target1: reference_

.. _target2: `phrase-link reference`_
""",
"""\
<document source="test data">
    <paragraph>
        Indirect hyperlink targets:
    <target id="target1" name="target1" refname="reference">
    <target id="target2" name="target2" refname="phrase-link reference">
"""],
["""\
.. _target1: Not a proper hyperlink target

.. _target2: Although it ends with an underscore, this is not a phrase-link_

.. _target3: A multi-line verson of something
   ending with an underscore, but not a phrase-link_
""",
"""\
<document source="test data">
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Hyperlink target contains whitespace. Perhaps a footnote was intended?
        <literal_block xml:space="1">
            .. _target1: Not a proper hyperlink target
    <system_message level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Hyperlink target contains whitespace. Perhaps a footnote was intended?
        <literal_block xml:space="1">
            .. _target2: Although it ends with an underscore, this is not a phrase-link_
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Hyperlink target contains whitespace. Perhaps a footnote was intended?
        <literal_block xml:space="1">
            .. _target3: A multi-line verson of something
               ending with an underscore, but not a phrase-link_
"""],
["""\
.. __: Not a proper hyperlink target

__ Although it ends with an underscore, this is not a phrase-link_

__ A multi-line verson of something
   ending with an underscore, but not a phrase-link_
""",
"""\
<document source="test data">
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Hyperlink target contains whitespace. Perhaps a footnote was intended?
        <literal_block xml:space="1">
            .. __: Not a proper hyperlink target
    <system_message level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Anonymous hyperlink target contains whitespace. Perhaps a footnote was intended?
        <literal_block xml:space="1">
            __ Although it ends with an underscore, this is not a phrase-link_
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Anonymous hyperlink target contains whitespace. Perhaps a footnote was intended?
        <literal_block xml:space="1">
            __ A multi-line verson of something
            ending with an underscore, but not a phrase-link_
"""],
["""\
.. _a long target name:

.. _`a target name: including a colon (quoted)`:

.. _a target name\: including a colon (escaped):
""",
"""\
<document source="test data">
    <target id="a-long-target-name" name="a long target name">
    <target id="a-target-name-including-a-colon-quoted" name="a target name: including a colon (quoted)">
    <target id="a-target-name-including-a-colon-escaped" name="a target name: including a colon (escaped)">
"""],
["""\
.. _a very long target name,
   split across lines:
.. _`and another,
   with backquotes`:
""",
"""\
<document source="test data">
    <target id="a-very-long-target-name-split-across-lines" name="a very long target name, split across lines">
    <target id="and-another-with-backquotes" name="and another, with backquotes">
"""],
["""\
External hyperlink:

.. _target: http://www.python.org/
""",
"""\
<document source="test data">
    <paragraph>
        External hyperlink:
    <target id="target" name="target" refuri="http://www.python.org/">
"""],
["""\
Duplicate external targets (different URIs):

.. _target: first

.. _target: second
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate external targets (different URIs):
    <target dupname="target" id="target" refuri="first">
    <system_message backrefs="id1" level="2" source="test data" type="WARNING">
        <paragraph>
            Duplicate explicit target name: "target".
    <target dupname="target" id="id1" refuri="second">
"""],
["""\
Duplicate external targets (same URIs):

.. _target: first

.. _target: first
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate external targets (same URIs):
    <target id="target" name="target" refuri="first">
    <system_message backrefs="id1" level="1" source="test data" type="INFO">
        <paragraph>
            Duplicate explicit target name: "target".
    <target dupname="target" id="id1" refuri="first">
"""],
["""\
Duplicate implicit targets.

Title
=====

Paragraph.

Title
=====

Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate implicit targets.
    <section dupname="title" id="title">
        <title>
            Title
        <paragraph>
            Paragraph.
    <section dupname="title" id="id1">
        <title>
            Title
        <system_message backrefs="id1" level="1" source="test data" type="INFO">
            <paragraph>
                Duplicate implicit target name: "title".
        <paragraph>
            Paragraph.
"""],
["""\
Duplicate implicit/explicit targets.

Title
=====

.. _title:

Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate implicit/explicit targets.
    <section dupname="title" id="title">
        <title>
            Title
        <system_message backrefs="id1" level="1" source="test data" type="INFO">
            <paragraph>
                Duplicate implicit target name: "title".
        <target id="id1" name="title">
        <paragraph>
            Paragraph.
"""],
["""\
Duplicate explicit targets.

.. _title:

First.

.. _title:

Second.

.. _title:

Third.
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate explicit targets.
    <target dupname="title" id="title">
    <paragraph>
        First.
    <system_message backrefs="id1" level="2" source="test data" type="WARNING">
        <paragraph>
            Duplicate explicit target name: "title".
    <target dupname="title" id="id1">
    <paragraph>
        Second.
    <system_message backrefs="id2" level="2" source="test data" type="WARNING">
        <paragraph>
            Duplicate explicit target name: "title".
    <target dupname="title" id="id2">
    <paragraph>
        Third.
"""],
["""\
Duplicate targets:

Target
======

Implicit section header target.

.. [target] Citation target.

.. [#target] Autonumber-labeled footnote target.

.. _target:

Explicit internal target.

.. _target: Explicit_external_target
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate targets:
    <section dupname="target" id="target">
        <title>
            Target
        <paragraph>
            Implicit section header target.
        <citation dupname="target" id="id1">
            <label>
                target
            <system_message backrefs="id1" level="1" source="test data" type="INFO">
                <paragraph>
                    Duplicate implicit target name: "target".
            <paragraph>
                Citation target.
        <footnote auto="1" dupname="target" id="id2">
            <system_message backrefs="id2" level="2" source="test data" type="WARNING">
                <paragraph>
                    Duplicate explicit target name: "target".
            <paragraph>
                Autonumber-labeled footnote target.
        <system_message backrefs="id3" level="2" source="test data" type="WARNING">
            <paragraph>
                Duplicate explicit target name: "target".
        <target dupname="target" id="id3">
        <paragraph>
            Explicit internal target.
        <system_message backrefs="id4" level="2" source="test data" type="WARNING">
            <paragraph>
                Duplicate explicit target name: "target".
        <target dupname="target" id="id4" refuri="Explicit_external_target">
"""],
]

totest['anonymous_targets'] = [
["""\
Anonymous external hyperlink target:

.. __: http://w3c.org/
""",
"""\
<document source="test data">
    <paragraph>
        Anonymous external hyperlink target:
    <target anonymous="1" id="id1" refuri="http://w3c.org/">
"""],
["""\
Anonymous external hyperlink target:

__ http://w3c.org/
""",
"""\
<document source="test data">
    <paragraph>
        Anonymous external hyperlink target:
    <target anonymous="1" id="id1" refuri="http://w3c.org/">
"""],
["""\
Anonymous indirect hyperlink target:

.. __: reference_
""",
"""\
<document source="test data">
    <paragraph>
        Anonymous indirect hyperlink target:
    <target anonymous="1" id="id1" refname="reference">
"""],
["""\
Anonymous indirect hyperlink targets:

__ reference_
__ `a very long
   reference`_
""",
"""\
<document source="test data">
    <paragraph>
        Anonymous indirect hyperlink targets:
    <target anonymous="1" id="id1" refname="reference">
    <target anonymous="1" id="id2" refname="a very long reference">
"""],
["""\
Mixed anonymous & named indirect hyperlink targets:

__ reference_
.. __: reference_
__ reference_
.. _target1: reference_
no blank line

.. _target2: reference_
__ reference_
.. __: reference_
__ reference_
no blank line
""",
"""\
<document source="test data">
    <paragraph>
        Mixed anonymous & named indirect hyperlink targets:
    <target anonymous="1" id="id1" refname="reference">
    <target anonymous="1" id="id2" refname="reference">
    <target anonymous="1" id="id3" refname="reference">
    <target id="target1" name="target1" refname="reference">
    <system_message level="2" line="7" source="test data" type="WARNING">
        <paragraph>
            Explicit markup ends without a blank line; unexpected unindent.
    <paragraph>
        no blank line
    <target id="target2" name="target2" refname="reference">
    <target anonymous="1" id="id4" refname="reference">
    <target anonymous="1" id="id5" refname="reference">
    <target anonymous="1" id="id6" refname="reference">
    <system_message level="2" line="13" source="test data" type="WARNING">
        <paragraph>
            Explicit markup ends without a blank line; unexpected unindent.
    <paragraph>
        no blank line
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
