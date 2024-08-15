#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.references.Hyperlinks.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.transforms.references import PropagateTargets, \
     AnonymousHyperlinks, IndirectHyperlinks, ExternalTargets, \
     InternalTargets, DanglingReferences
from docutils.transforms.universal import TestMessages
from docutils.utils import new_document


class TransformTestCase(unittest.TestCase):
    def test_transforms(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        for name, (transforms, cases) in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    # Don't do a ``populate_from_components()`` because that
                    # would enable the Transformer's default transforms.
                    document.transformer.add_transforms(transforms)
                    document.transformer.add_transform(TestMessages)
                    document.transformer.apply_transforms()
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

# Exhaustive listing of hyperlink variations: every combination of
# target/reference, direct/indirect, internal/external, and named/anonymous,
# plus embedded URIs.
totest['exhaustive_hyperlinks'] = ((PropagateTargets, AnonymousHyperlinks,
                                    IndirectHyperlinks, ExternalTargets,
                                    InternalTargets, DanglingReferences), [
["""\
direct_ external

.. _direct: http://direct
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="direct" refuri="http://direct">
            direct
         external
    <target ids="direct" names="direct" refuri="http://direct">
"""],
["""\
indirect_ external

.. _indirect: xtarget_
.. _xtarget: http://indirect
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="indirect" refuri="http://indirect">
            indirect
         external
    <target ids="indirect" names="indirect" refuri="http://indirect">
    <target ids="xtarget" names="xtarget" refuri="http://indirect">
"""],
["""\
.. _direct:

direct_ internal
""",
"""\
<document source="test data">
    <target refid="direct">
    <paragraph ids="direct" names="direct">
        <reference name="direct" refid="direct">
            direct
         internal
"""],
["""\
.. _ztarget:

indirect_ internal

.. _indirect2: ztarget_
.. _indirect: indirect2_
""",
"""\
<document source="test data">
    <target refid="ztarget">
    <paragraph ids="ztarget" names="ztarget">
        <reference name="indirect" refid="ztarget">
            indirect
         internal
    <target ids="indirect2" names="indirect2" refid="ztarget">
    <target ids="indirect" names="indirect" refid="ztarget">
"""],
["""\
Implicit
--------

indirect_ internal

.. _indirect: implicit_
""",
"""\
<document source="test data">
    <section ids="implicit" names="implicit">
        <title>
            Implicit
        <paragraph>
            <reference name="indirect" refid="implicit">
                indirect
             internal
        <target ids="indirect" names="indirect" refid="implicit">
"""],
["""\
Implicit
--------

`multiply-indirect`_ internal

.. _multiply-indirect: indirect_
.. _indirect: implicit_
""",
"""\
<document source="test data">
    <section ids="implicit" names="implicit">
        <title>
            Implicit
        <paragraph>
            <reference name="multiply-indirect" refid="implicit">
                multiply-indirect
             internal
        <target ids="multiply-indirect" names="multiply-indirect" refid="implicit">
        <target ids="indirect" names="indirect" refid="implicit">
"""],
["""\
circular_ indirect reference

.. _circular: indirect_
.. _indirect: circular_
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            circular_
         indirect reference
    <target ids="circular" names="circular" refid="circular">
    <problematic ids="problematic-2 indirect" names="indirect" refid="system-message-1">
        .. _indirect: circular_
    <system_message backrefs="problematic-1 problematic-2" ids="system-message-1" level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            Indirect hyperlink target "circular" (id="circular") refers to target "indirect", forming a circular reference.
"""],
["""\
Implicit
--------

Duplicate implicit targets.

Implicit
--------

indirect_ internal

.. _indirect: implicit_

Direct internal reference: Implicit_
""",
"""\
<document source="test data">
    <section dupnames="implicit" ids="implicit">
        <title>
            Implicit
        <paragraph>
            Duplicate implicit targets.
    <section dupnames="implicit" ids="implicit-1">
        <title>
            Implicit
        <system_message backrefs="implicit-1" level="1" line="7" source="test data" type="INFO">
            <paragraph>
                Duplicate implicit target name: "implicit".
        <paragraph>
            <problematic ids="problematic-1" refid="system-message-1">
                indirect_
             internal
        <target ids="indirect" names="indirect" refname="implicit">
        <paragraph>
            Direct internal reference: \n\
            <problematic ids="problematic-2" refid="system-message-2">
                Implicit_
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="11" source="test data" type="ERROR">
        <paragraph>
            Indirect hyperlink target "indirect" (id="indirect") refers to target "implicit", which is a duplicate, and cannot be used as a unique reference.
    <system_message backrefs="problematic-2" ids="system-message-2" level="3" line="13" source="test data" type="ERROR">
        <paragraph>
            Duplicate target name, cannot be used as a unique reference: "implicit".
"""],
["""\
`direct external`__

__ http://direct
""",
"""\
<document source="test data">
    <paragraph>
        <reference anonymous="1" name="direct external" refuri="http://direct">
            direct external
    <target anonymous="1" ids="target-1" refuri="http://direct">
"""],
["""\
`indirect external`__

__ xtarget_
.. _xtarget: http://indirect
""",
"""\
<document source="test data">
    <paragraph>
        <reference anonymous="1" name="indirect external" refuri="http://indirect">
            indirect external
    <target anonymous="1" ids="target-1" refuri="http://indirect">
    <target ids="xtarget" names="xtarget" refuri="http://indirect">
"""],
["""\
__

`direct internal`__
""",
"""\
<document source="test data">
    <target anonymous="1" refid="target-1">
    <paragraph ids="target-1">
        <reference anonymous="1" name="direct internal" refid="target-1">
            direct internal
"""],
["""\
.. _ztarget:

`indirect internal`__

__ ztarget_
""",
"""\
<document source="test data">
    <target refid="ztarget">
    <paragraph ids="ztarget" names="ztarget">
        <reference anonymous="1" name="indirect internal" refid="ztarget">
            indirect internal
    <target anonymous="1" ids="target-1" refid="ztarget">
"""],
["""\
.. _ztarget:

First

.. _ztarget:

Second

`indirect internal`__

__ ztarget_
""",
"""\
<document source="test data">
    <target dupnames="ztarget" refid="ztarget">
    <paragraph ids="ztarget">
        First
    <system_message backrefs="ztarget-1" level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Duplicate explicit target name: "ztarget".
    <target dupnames="ztarget" refid="ztarget-1">
    <paragraph ids="ztarget-1">
        Second
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            `indirect internal`__
    <target anonymous="1" ids="target-1" refname="ztarget">
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="11" source="test data" type="ERROR">
        <paragraph>
            Indirect hyperlink target (id="target-1") refers to target "ztarget", which is a duplicate, and cannot be used as a unique reference.
"""],
["""\
The next anonymous hyperlink reference is parsed (and discarded) at
some point, but nonetheless anonymous hyperlink references and targets
match in this snippet.

.. |invalid| replace:: anonymous__

hyperlink__

__ URL
""",
"""\
<document source="test data">
    <paragraph>
        The next anonymous hyperlink reference is parsed (and discarded) at
        some point, but nonetheless anonymous hyperlink references and targets
        match in this snippet.
    <system_message level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            Substitution definition contains illegal element <reference>:
        <literal_block xml:space="preserve">
            <reference anonymous="1" name="anonymous">
                anonymous
        <literal_block xml:space="preserve">
            .. |invalid| replace:: anonymous__
    <paragraph>
        <reference anonymous="1" name="hyperlink" refuri="URL">
            hyperlink
    <target anonymous="1" ids="target-1" refuri="URL">
"""],
["""\
An `embedded uri <http://direct>`_.

Another reference to the same `embedded URI`_.
""",
"""\
<document source="test data">
    <paragraph>
        An \n\
        <reference name="embedded uri" refuri="http://direct">
            embedded uri
        <target ids="embedded-uri" names="embedded\\ uri" refuri="http://direct">
        .
    <paragraph>
        Another reference to the same \n\
        <reference name="embedded URI" refuri="http://direct">
            embedded URI
        .
"""],
["""\
An `anonymous embedded uri <http://direct>`__.
""",
"""\
<document source="test data">
    <paragraph>
        An \n\
        <reference name="anonymous embedded uri" refuri="http://direct">
            anonymous embedded uri
        .
"""],
["""\
An `embedded alias <alias_>`_.

Another reference to the same `embedded alias`_.

.. _alias: ham.py
""",
"""\
<document source="test data">
    <paragraph>
        An \n\
        <reference name="embedded alias" refuri="ham.py">
            embedded alias
        <target names="embedded\\ alias" refuri="ham.py">
        .
    <paragraph>
        Another reference to the same \n\
        <reference name="embedded alias" refuri="ham.py">
            embedded alias
        .
    <target ids="alias" names="alias" refuri="ham.py">
"""],
["""\
An `anonymous embedded alias <redirect_>`__.

.. _redirect: spam.py
""",
"""\
<document source="test data">
    <paragraph>
        An \n\
        <reference name="anonymous embedded alias" refuri="spam.py">
            anonymous embedded alias
        .
    <target ids="redirect" names="redirect" refuri="spam.py">
"""],
["""\
An `embedded alias <alias_>`_ with unknown reference.
""",
"""\
<document source="test data">
    <paragraph>
        An \n\
        <problematic ids="problematic-1" refid="system-message-2">
            `embedded alias <alias_>`_
        <target names="embedded\\ alias" refname="alias">
         with unknown reference.
    <system_message ids="system-message-1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Indirect hyperlink target "embedded alias"  refers to target "alias", which does not exist.
    <system_message backrefs="problematic-1" ids="system-message-2" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Unknown target name: "alias".
    <system_message level="1" line="1" source="test data" type="INFO">
        <paragraph>
            Hyperlink target "embedded alias" is not referenced.
"""],
["""\
An embedded URI with trailing underline:
`__init__ <http:example.py.html#__init__>`__.
""",
"""\
<document source="test data">
    <paragraph>
        An embedded URI with trailing underline:
        <reference name="__init__" refuri="http:example.py.html#__init__">
            __init__
        .
"""],
["""\
Hyperlinks with angle-bracketed text need escaping.

See `Element \\<a>`_, `Element <b\\>`_, and `Element <c>\\ `_.

.. _`Element <a>`:
.. _`Element <b>`:
.. _`Element <c>`: elements.rst
""",
"""\
<document source="test data">
    <paragraph>
        Hyperlinks with angle-bracketed text need escaping.
    <paragraph>
        See \n\
        <reference name="Element <a>" refuri="elements.rst">
            Element <a>
        , \n\
        <reference name="Element <b>" refuri="elements.rst">
            Element <b>
        , and \n\
        <reference name="Element <c>" refuri="elements.rst">
            Element <c>
        .
    <target refid="element-a">
    <target refid="element-b">
    <target ids="element-c element-b element-a" names="element\\ <c> element\\ <b> element\\ <a>" refuri="elements.rst">
"""],
["""\
.. _target:

.. [1] Footnote; target_
""",
"""\
<document source="test data">
    <target ids="target" names="target">
    <footnote ids="footnote-1" names="1">
        <label>
            1
        <paragraph>
            Footnote; \n\
            <reference name="target" refid="target">
                target
"""],
["""\
.. _target:

.. [cit] Citation; target_
""",
"""\
<document source="test data">
    <target ids="target" names="target">
    <citation ids="cit" names="cit">
        <label>
            cit
        <paragraph>
            Citation; \n\
            <reference name="target" refid="target">
                target
"""],
])

totest['hyperlinks'] = ((PropagateTargets, AnonymousHyperlinks,
                         IndirectHyperlinks, ExternalTargets,
                         InternalTargets, DanglingReferences), [
["""\
.. _internal hyperlink:

This paragraph referenced.

By this `internal hyperlink`_ reference.
""",
"""\
<document source="test data">
    <target refid="internal-hyperlink">
    <paragraph ids="internal-hyperlink" names="internal\\ hyperlink">
        This paragraph referenced.
    <paragraph>
        By this \n\
        <reference name="internal hyperlink" refid="internal-hyperlink">
            internal hyperlink
         reference.
"""],
["""\
.. _chained:
.. _internal hyperlink:

This paragraph referenced.

By this `internal hyperlink`_ reference
as well as by this chained_ reference.

The results of the transform are not visible at the XML level.
""",
"""\
<document source="test data">
    <target refid="chained">
    <target refid="internal-hyperlink">
    <paragraph ids="internal-hyperlink chained" names="internal\\ hyperlink chained">
        This paragraph referenced.
    <paragraph>
        By this \n\
        <reference name="internal hyperlink" refid="internal-hyperlink">
            internal hyperlink
         reference
        as well as by this \n\
        <reference name="chained" refid="chained">
            chained
         reference.
    <paragraph>
        The results of the transform are not visible at the XML level.
"""],
["""\
.. _chained:
__ http://anonymous

Anonymous__ and chained_ both refer to the same URI.
""",
"""\
<document source="test data">
    <target refid="chained">
    <target anonymous="1" ids="target-1 chained" names="chained" refuri="http://anonymous">
    <paragraph>
        <reference anonymous="1" name="Anonymous" refuri="http://anonymous">
            Anonymous
         and \n\
        <reference name="chained" refuri="http://anonymous">
            chained
         both refer to the same URI.
"""],
["""\
.. _a:
.. _b:

x
""",
"""\
<document source="test data">
    <target refid="a">
    <target refid="b">
    <paragraph ids="b a" names="b a">
        x
    <system_message level="1" line="1" source="test data" type="INFO">
        <paragraph>
            Hyperlink target "a" is not referenced.
    <system_message level="1" line="2" source="test data" type="INFO">
        <paragraph>
            Hyperlink target "b" is not referenced.
"""],
["""\
.. _a:
.. _b:

a_
""",
"""\
<document source="test data">
    <target refid="a">
    <target refid="b">
    <paragraph ids="b a" names="b a">
        <reference name="a" refid="a">
            a
    <system_message level="1" line="2" source="test data" type="INFO">
        <paragraph>
            Hyperlink target "b" is not referenced.
"""],
["""\
.. _a:
.. _b:

b_
""",
"""\
<document source="test data">
    <target refid="a">
    <target refid="b">
    <paragraph ids="b a" names="b a">
        <reference name="b" refid="b">
            b
    <system_message level="1" line="1" source="test data" type="INFO">
        <paragraph>
            Hyperlink target "a" is not referenced.
"""],
["""\
.. _a:
.. _b:

a_\\ b_
""",
"""\
<document source="test data">
    <target refid="a">
    <target refid="b">
    <paragraph ids="b a" names="b a">
        <reference name="a" refid="a">
            a
        <reference name="b" refid="b">
            b
"""],
["""\
.. _external hyperlink: http://uri

`External hyperlink`_ reference.
""",
"""\
<document source="test data">
    <target ids="external-hyperlink" names="external\\ hyperlink" refuri="http://uri">
    <paragraph>
        <reference name="External hyperlink" refuri="http://uri">
            External hyperlink
         reference.
"""],
["""\
.. _external hyperlink: http://uri
.. _indirect target: `external hyperlink`_
""",
"""\
<document source="test data">
    <target ids="external-hyperlink" names="external\\ hyperlink" refuri="http://uri">
    <target ids="indirect-target" names="indirect\\ target" refuri="http://uri">
    <system_message level="1" line="2" source="test data" type="INFO">
        <paragraph>
            Hyperlink target "indirect target" is not referenced.
"""],
["""\
.. _chained:
.. _external hyperlink: http://uri

`External hyperlink`_ reference
and a chained_ reference too.
""",
"""\
<document source="test data">
    <target refid="chained">
    <target ids="external-hyperlink chained" names="external\\ hyperlink chained" refuri="http://uri">
    <paragraph>
        <reference name="External hyperlink" refuri="http://uri">
            External hyperlink
         reference
        and a \n\
        <reference name="chained" refuri="http://uri">
            chained
         reference too.
"""],
["""\
.. _external hyperlink: http://uri
.. _indirect hyperlink: `external hyperlink`_

`Indirect hyperlink`_ reference.
""",
"""\
<document source="test data">
    <target ids="external-hyperlink" names="external\\ hyperlink" refuri="http://uri">
    <target ids="indirect-hyperlink" names="indirect\\ hyperlink" refuri="http://uri">
    <paragraph>
        <reference name="Indirect hyperlink" refuri="http://uri">
            Indirect hyperlink
         reference.
"""],
["""\
.. _external hyperlink: http://uri
.. _chained:
.. _indirect hyperlink: `external hyperlink`_

Chained_ `indirect hyperlink`_ reference.
""",
"""\
<document source="test data">
    <target ids="external-hyperlink" names="external\\ hyperlink" refuri="http://uri">
    <target refuri="http://uri">
    <target ids="indirect-hyperlink chained" names="indirect\\ hyperlink chained" refuri="http://uri">
    <paragraph>
        <reference name="Chained" refuri="http://uri">
            Chained
         \n\
        <reference name="indirect hyperlink" refuri="http://uri">
            indirect hyperlink
         reference.
"""],
["""\
.. __: http://full
__
__ http://simplified
.. _external: http://indirect.external
__ external_
__

`Full syntax anonymous external hyperlink reference`__,
`chained anonymous external reference`__,
`simplified syntax anonymous external hyperlink reference`__,
`indirect anonymous hyperlink reference`__,
`internal anonymous hyperlink reference`__.
""",
"""\
<document source="test data">
    <target anonymous="1" ids="target-1" refuri="http://full">
    <target anonymous="1" refid="target-2">
    <target anonymous="1" ids="target-3 target-2" refuri="http://simplified">
    <target ids="external" names="external" refuri="http://indirect.external">
    <target anonymous="1" ids="target-4" refuri="http://indirect.external">
    <target anonymous="1" refid="target-5">
    <paragraph ids="target-5">
        <reference anonymous="1" name="Full syntax anonymous external hyperlink reference" refuri="http://full">
            Full syntax anonymous external hyperlink reference
        ,
        <reference anonymous="1" name="chained anonymous external reference" refuri="http://simplified">
            chained anonymous external reference
        ,
        <reference anonymous="1" name="simplified syntax anonymous external hyperlink reference" refuri="http://simplified">
            simplified syntax anonymous external hyperlink reference
        ,
        <reference anonymous="1" name="indirect anonymous hyperlink reference" refuri="http://indirect.external">
            indirect anonymous hyperlink reference
        ,
        <reference anonymous="1" name="internal anonymous hyperlink reference" refid="target-5">
            internal anonymous hyperlink reference
        .
"""],
["""\
Duplicate external target_'s (different URIs):

.. _target: first

.. _target: second
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate external \n\
        <problematic ids="problematic-1" refid="system-message-1">
            target_
        's (different URIs):
    <target dupnames="target" ids="target" refuri="first">
    <system_message backrefs="target-1" level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Duplicate explicit target name: "target".
    <target dupnames="target" ids="target-1" refuri="second">
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Duplicate target name, cannot be used as a unique reference: "target".
"""],
["""\
Duplicate external targets (different URIs) without reference:

.. _target: first

.. _target: second
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate external targets (different URIs) without reference:
    <target dupnames="target" ids="target" refuri="first">
    <system_message backrefs="target-1" level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Duplicate explicit target name: "target".
    <target dupnames="target" ids="target-1" refuri="second">
"""],
["""\
Several__ anonymous__ hyperlinks__, but not enough targets.

__ http://example.org
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            Several__
         \n\
        <problematic ids="problematic-2" refid="system-message-1">
            anonymous__
         \n\
        <problematic ids="problematic-3" refid="system-message-1">
            hyperlinks__
        , but not enough targets.
    <target anonymous="1" ids="target-1" refuri="http://example.org">
    <system_message backrefs="problematic-1 problematic-2 problematic-3" ids="system-message-1" level="3" source="test data" type="ERROR">
        <paragraph>
            Anonymous hyperlink mismatch: 3 references but 1 targets.
            See "backrefs" attribute for IDs.
"""],
["""\
.. _external: http://uri
.. _indirect: external_
.. _internal:

.. image:: picture.png
   :target: external_

.. image:: picture.png
   :target: indirect_

.. image:: picture.png
   :target: internal_
""",
"""\
<document source="test data">
    <target ids="external" names="external" refuri="http://uri">
    <target ids="indirect" names="indirect" refuri="http://uri">
    <target refid="internal">
    <reference ids="internal" name="external" names="internal" refuri="http://uri">
        <image uri="picture.png">
    <reference name="indirect" refuri="http://uri">
        <image uri="picture.png">
    <reference name="internal" refid="internal">
        <image uri="picture.png">
"""],
["""\
.. contents:: Table of Contents
.. _indirect reference to the table of contents: `table of contents`_

Section
=======

Testing an `indirect reference to the table of contents`_.
""",
"""\
<document source="test data">
    <topic classes="contents" ids="table-of-contents" names="table\\ of\\ contents">
        <title>
            Table of Contents
        <bullet_list>
            <list_item>
                <paragraph>
                    <reference ids="toc-entry-1" refid="section">
                        Section
    <target ids="indirect-reference-to-the-table-of-contents" names="indirect\\ reference\\ to\\ the\\ table\\ of\\ contents" refid="table-of-contents">
    <section ids="section" names="section">
        <title refid="toc-entry-1">
            Section
        <paragraph>
            Testing an \n\
            <reference name="indirect reference to the table of contents" refid="table-of-contents">
                indirect reference to the table of contents
            .
"""],
["""\
.. _explicit target:

Title
-----

Let's reference it (`explicit target`_) to avoid an irrelevant error.
""",
"""\
<document source="test data">
    <target refid="explicit-target">
    <section ids="title explicit-target" names="title explicit\\ target">
        <title>
            Title
        <paragraph>
            Let's reference it (
            <reference name="explicit target" refid="explicit-target">
                explicit target
            ) to avoid an irrelevant error.
"""],
["""\
target1_ should refer to target2_, not the Title.

.. _target1:
.. _target2: URI

Title
=====
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="target1" refuri="URI">
            target1
         should refer to \n\
        <reference name="target2" refuri="URI">
            target2
        , not the Title.
    <target refid="target1">
    <target ids="target2 target1" names="target2 target1" refuri="URI">
    <section ids="title" names="title">
        <title>
            Title
"""],
["""\
Unknown reference_.
""",
"""\
<document source="test data">
    <paragraph>
        Unknown \n\
        <problematic ids="problematic-1" refid="system-message-1">
            reference_
        .
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Unknown target name: "reference".
"""],
["""\
Duplicate manual footnote labels, with reference ([1]_):

.. [1] Footnote.

.. [1] Footnote.
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate manual footnote labels, with reference (
        <problematic ids="footnote-reference-1" refid="system-message-1">
            [1]_
        ):
    <footnote dupnames="1" ids="footnote-1">
        <label>
            1
        <paragraph>
            Footnote.
    <footnote dupnames="1" ids="footnote-2">
        <label>
            1
        <system_message backrefs="footnote-2" level="2" line="5" source="test data" type="WARNING">
            <paragraph>
                Duplicate explicit target name: "1".
        <paragraph>
            Footnote.
    <system_message backrefs="footnote-reference-1" ids="system-message-1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Duplicate target name, cannot be used as a unique reference: "1".
"""],
])


if __name__ == '__main__':
    unittest.main()
