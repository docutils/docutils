#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for images.py image directives.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['images'] = [
["""\
.. image:: picture.png
""",
"""\
<document source="test data">
    <image uri="picture.png">
"""],
["""\
.. image::
""",
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Missing image URI argument at line 1.
        <literal_block xml:space="1">
            .. image::
"""],
["""\
.. image:: one two three
""",
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Image URI at line 1 contains whitespace.
        <literal_block xml:space="1">
            .. image:: one two three
"""],
["""\
.. image:: picture.png
   :height: 100
   :width: 200
   :scale: 50
""",
"""\
<document source="test data">
    <image height="100" scale="50" uri="picture.png" width="200">
"""],
["""\
.. image::
   picture.png
   :height: 100
   :width: 200
   :scale: 50
""",
"""\
<document source="test data">
    <image height="100" scale="50" uri="picture.png" width="200">
"""],
["""\
.. image::
   :height: 100
   :width: 200
   :scale: 50
""",
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Missing image URI argument at line 1.
        <literal_block xml:space="1">
            .. image::
               :height: 100
               :width: 200
               :scale: 50
"""],
["""\
.. image:: a/very/long/path/to/
   picture.png
   :height: 100
   :width: 200
   :scale: 50
""",
"""\
<document source="test data">
    <image height="100" scale="50" uri="a/very/long/path/to/picture.png" width="200">
"""],
["""\
.. image:: picture.png
   :height: 100
   :width: 200
   :scale: 50
   :alt: Alternate text for the picture
""",
"""\
<document source="test data">
    <image alt="Alternate text for the picture" height="100" scale="50" uri="picture.png" width="200">
"""],
["""\
.. image:: picture.png
   :scale: - 50
""",
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive attributes at line 1:
            invalid attribute data: extension attribute field body may contain
            a single paragraph only (attribute "scale").
        <literal_block xml:space="1">
            .. image:: picture.png
               :scale: - 50
"""],
["""\
.. image:: picture.png
   :scale:
""",
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive attributes at line 1:
            invalid attribute value: (attribute: "scale"; value: None)
            object can't be converted to int.
        <literal_block xml:space="1">
            .. image:: picture.png
               :scale:
"""],
["""\
.. image:: picture.png
   :scale 50
""",
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive attributes at line 1:
            invalid attribute block.
        <literal_block xml:space="1">
            .. image:: picture.png
               :scale 50
"""],
["""\
.. image:: picture.png
   scale: 50
""",
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Image URI at line 1 contains whitespace.
        <literal_block xml:space="1">
            .. image:: picture.png
               scale: 50
"""],
["""\
.. image:: picture.png
   :: 50
""",
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive attributes at line 1:
            invalid attribute block.
        <literal_block xml:space="1">
            .. image:: picture.png
               :: 50
"""],
["""\
.. image:: picture.png
   :sale: 50
""",
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive attributes at line 1:
            unknown attribute: "sale".
        <literal_block xml:space="1">
            .. image:: picture.png
               :sale: 50
"""],
["""\
.. image:: picture.png
   :scale: fifty
""",
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive attributes at line 1:
            invalid attribute value: (attribute: "scale"; value: 'fifty')
            invalid literal for int(): fifty.
        <literal_block xml:space="1">
            .. image:: picture.png
               :scale: fifty
"""],
["""\
.. image:: picture.png
   :scale: 50
   :scale: 50
""",
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive attributes at line 1:
            invalid attribute data: duplicate attribute "scale".
        <literal_block xml:space="1">
            .. image:: picture.png
               :scale: 50
               :scale: 50
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
