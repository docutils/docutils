#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "role" directive.
"""

from __init__ import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['role'] = [
["""\
.. role:: custom
.. role:: special

:custom:`interpreted` and :special:`interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <inline class="custom">
            interpreted
         and \n\
        <inline class="special">
            interpreted
"""],
["""\
.. role:: custom
   :class: custom-class
.. role:: special
   :class: special-class

:custom:`interpreted` and :special:`interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <inline class="custom-class">
            interpreted
         and \n\
        <inline class="special-class">
            interpreted
"""],
["""\
Must define :custom:`interpreted` before using it.

.. role:: custom

Now that it's defined, :custom:`interpreted` works.
""",
"""\
<document source="test data">
    <paragraph>
        Must define 
        <problematic id="id2" refid="id1">
            :custom:`interpreted`
         before using it.
    <system_message level="1" line="1" source="test data" type="INFO">
        <paragraph>
            No role entry for "custom" in module "docutils.parsers.rst.languages.en".
            Trying "custom" as canonical role name.
    <system_message backrefs="id2" id="id1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Unknown interpreted text role "custom".
    <paragraph>
        Now that it's defined, \n\
        <inline class="custom">
            interpreted
         works.
"""],
["""
.. role:: custom
   :class: 1
""",
"""\
<document source="test data">
    <system_message level="3" line="2" source="test data" type="ERROR">
        <paragraph>
            Error in "role" directive:
            invalid option value: (option: "class"; value: '1')
            cannot make "1" into a class name.
        <literal_block xml:space="preserve">
            .. role:: custom
               :class: 1
"""],
["""
.. role:: 1
""",
"""\
<document source="test data">
    <system_message level="3" line="2" source="test data" type="ERROR">
        <paragraph>
            Invalid argument for "role" directive:
            cannot make "1" into a class name.
        <literal_block xml:space="preserve">
            .. role:: 1
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
