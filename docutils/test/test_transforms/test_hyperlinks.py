#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.references.Hyperlinks.
"""

from __init__ import DocutilsTestSupport
from docutils.transforms.references import ChainedTargets, \
     AnonymousHyperlinks, IndirectHyperlinks, ExternalTargets, InternalTargets
from docutils.transforms.universal import FinalChecks
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s

totest = {}

# Exhaustive listing of hyperlink variations: every combination of
# target/reference, direct/indirect, internal/external, and named/anonymous,
# plus embedded URIs.
totest['exhaustive_hyperlinks'] = ((ChainedTargets, AnonymousHyperlinks,
                                    IndirectHyperlinks, ExternalTargets,
                                    InternalTargets, FinalChecks), [
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
    <target id="direct" name="direct" refuri="http://direct">
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
    <target id="indirect" name="indirect" refuri="http://indirect">
    <target id="xtarget" name="xtarget" refuri="http://indirect">
"""],
["""\
.. _direct:

direct_ internal
""",
"""\
<document source="test data">
    <target id="direct" name="direct">
    <paragraph>
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
    <target id="ztarget" name="ztarget">
    <paragraph>
        <reference name="indirect" refid="ztarget">
            indirect
         internal
    <target id="indirect2" name="indirect2" refid="ztarget">
    <target id="indirect" name="indirect" refid="ztarget">
"""],
["""\
Implicit
--------

indirect_ internal

.. _indirect: implicit_
""",
"""\
<document source="test data">
    <section id="implicit" name="implicit">
        <title>
            Implicit
        <paragraph>
            <reference name="indirect" refid="implicit">
                indirect
             internal
        <target id="indirect" name="indirect" refid="implicit">
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
    <section id="implicit" name="implicit">
        <title>
            Implicit
        <paragraph>
            <reference name="multiply-indirect" refid="implicit">
                multiply-indirect
             internal
        <target id="multiply-indirect" name="multiply-indirect" refid="implicit">
        <target id="indirect" name="indirect" refid="implicit">
"""],
["""\
circular_ indirect reference

.. _circular: indirect_
.. _indirect: circular_
""",
"""\
<document source="test data">
    <paragraph>
        <problematic id="id2" refid="id1">
            circular_
         indirect reference
    <target id="circular" name="circular" refid="circular">
    <problematic id="id3" refid="id1">
        .. _indirect: circular_
    <system_message backrefs="id2 id3" id="id1" level="3" line="3" source="test data" type="ERROR">
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
    <section dupname="implicit" id="implicit">
        <title>
            Implicit
        <paragraph>
            Duplicate implicit targets.
    <section dupname="implicit" id="id1">
        <title>
            Implicit
        <system_message backrefs="id1" level="1" line="7" source="test data" type="INFO">
            <paragraph>
                Duplicate implicit target name: "implicit".
        <paragraph>
            <problematic id="id3" refid="id2">
                indirect_
             internal
        <target id="indirect" name="indirect" refname="implicit">
        <paragraph>
            Direct internal reference: 
            <problematic id="id5" refid="id4">
                Implicit_
    <system_message backrefs="id3" id="id2" level="3" line="11" source="test data" type="ERROR">
        <paragraph>
            Indirect hyperlink target "indirect" (id="indirect") refers to target "implicit", which is a duplicate, and cannot be used as a unique reference.
    <system_message backrefs="id5" id="id4" level="3" line="13" source="test data" type="ERROR">
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
    <target anonymous="1" id="id1" refuri="http://direct">
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
    <target anonymous="1" id="id1" refuri="http://indirect">
    <target id="xtarget" name="xtarget" refuri="http://indirect">
"""],
["""\
__

`direct internal`__
""",
"""\
<document source="test data">
    <target anonymous="1" id="id1">
    <paragraph>
        <reference anonymous="1" name="direct internal" refid="id1">
            direct internal
"""],
["""\
.. _ztarget:

`indirect internal`__

__ ztarget_
""",
"""\
<document source="test data">
    <target id="ztarget" name="ztarget">
    <paragraph>
        <reference anonymous="1" name="indirect internal" refid="ztarget">
            indirect internal
    <target anonymous="1" id="id1" refid="ztarget">
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
    <target dupname="ztarget" id="ztarget">
    <paragraph>
        First
    <system_message backrefs="id1" level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Duplicate explicit target name: "ztarget".
    <target dupname="ztarget" id="id1">
    <paragraph>
        Second
    <paragraph>
        <problematic id="id4" refid="id3">
            `indirect internal`__
    <target anonymous="1" id="id2" refname="ztarget">
    <system_message backrefs="id4" id="id3" level="3" line="11" source="test data" type="ERROR">
        <paragraph>
            Indirect hyperlink target (id="id2") refers to target "ztarget", which is a duplicate, and cannot be used as a unique reference.
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
        <target id="embedded-uri" name="embedded uri" refuri="http://direct">
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
])

totest['hyperlinks'] = ((ChainedTargets, AnonymousHyperlinks,
                         IndirectHyperlinks, ExternalTargets,
                         InternalTargets,), [
["""\
.. _internal hyperlink:

This paragraph referenced.

By this `internal hyperlink`_ referemce.
""",
"""\
<document source="test data">
    <target id="internal-hyperlink" name="internal hyperlink">
    <paragraph>
        This paragraph referenced.
    <paragraph>
        By this \n\
        <reference name="internal hyperlink" refid="internal-hyperlink">
            internal hyperlink
         referemce.
"""],
["""\
.. _chained:
.. _internal hyperlink:

This paragraph referenced.

By this `internal hyperlink`_ referemce
as well as by this chained_ reference.

The results of the transform are not visible at the XML level.
""",
"""\
<document source="test data">
    <target id="chained" name="chained">
    <target id="internal-hyperlink" name="internal hyperlink">
    <paragraph>
        This paragraph referenced.
    <paragraph>
        By this \n\
        <reference name="internal hyperlink" refid="internal-hyperlink">
            internal hyperlink
         referemce
        as well as by this \n\
        <reference name="chained" refid="chained">
            chained
         reference.
    <paragraph>
        The results of the transform are not visible at the XML level.
"""],
["""\
.. _external hyperlink: http://uri

`External hyperlink`_ reference.
""",
"""\
<document source="test data">
    <target id="external-hyperlink" name="external hyperlink" refuri="http://uri">
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
    <target id="external-hyperlink" name="external hyperlink" refuri="http://uri">
    <target id="indirect-target" name="indirect target" refuri="http://uri">
    <system_message level="1" line="2" source="test data" type="INFO">
        <paragraph>
            Indirect hyperlink target "indirect target" is not referenced.
"""],
["""\
.. _chained:
.. _external hyperlink: http://uri

`External hyperlink`_ reference
and a chained_ reference too.
""",
"""\
<document source="test data">
    <target id="chained" name="chained" refuri="http://uri">
    <target id="external-hyperlink" name="external hyperlink" refuri="http://uri">
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
    <target id="external-hyperlink" name="external hyperlink" refuri="http://uri">
    <target id="indirect-hyperlink" name="indirect hyperlink" refuri="http://uri">
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
    <target id="external-hyperlink" name="external hyperlink" refuri="http://uri">
    <target id="chained" name="chained" refuri="http://uri">
    <target id="indirect-hyperlink" name="indirect hyperlink" refuri="http://uri">
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
    <target anonymous="1" id="id1" refuri="http://full">
    <target anonymous="1" id="id2" refuri="http://simplified">
    <target anonymous="1" id="id3" refuri="http://simplified">
    <target id="external" name="external" refuri="http://indirect.external">
    <target anonymous="1" id="id4" refuri="http://indirect.external">
    <target anonymous="1" id="id5">
    <paragraph>
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
        <reference anonymous="1" name="internal anonymous hyperlink reference" refid="id5">
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
        <reference name="target" refname="target">
            target
        's (different URIs):
    <target dupname="target" id="target" refuri="first">
    <system_message backrefs="id1" level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Duplicate explicit target name: "target".
    <target dupname="target" id="id1" refuri="second">
"""],
["""\
Several__ anonymous__ hyperlinks__, but not enough targets.

__ http://example.org
""",
"""\
<document source="test data">
    <paragraph>
        <problematic id="id3" refid="id2">
            Several__
         \n\
        <problematic id="id4" refid="id2">
            anonymous__
         \n\
        <problematic id="id5" refid="id2">
            hyperlinks__
        , but not enough targets.
    <target anonymous="1" id="id1" refuri="http://example.org">
    <system_message backrefs="id3 id4 id5" id="id2" level="3" source="test data" type="ERROR">
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
    <target id="external" name="external" refuri="http://uri">
    <target id="indirect" name="indirect" refuri="http://uri">
    <target id="internal" name="internal">
    <reference name="external_" refuri="http://uri">
        <image uri="picture.png">
    <reference name="indirect_" refuri="http://uri">
        <image uri="picture.png">
    <reference name="internal_" refid="internal">
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
    <topic class="contents" id="table-of-contents" name="table of contents">
        <title>
            Table of Contents
        <bullet_list>
            <list_item>
                <paragraph>
                    <reference id="id1" refid="section">
                        Section
    <target id="indirect-reference-to-the-table-of-contents" name="indirect reference to the table of contents" refid="table-of-contents">
    <section id="section" name="section">
        <title refid="id1">
            Section
        <paragraph>
            Testing an 
            <reference name="indirect reference to the table of contents" refid="table-of-contents">
                indirect reference to the table of contents
            .
"""],
# ["""\
# Title
# -----

# Duplicate implicit targets.

# Title
# -----

# indirect_ internal

# .. _indirect: implicit_
# """,
# """\
# """],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
