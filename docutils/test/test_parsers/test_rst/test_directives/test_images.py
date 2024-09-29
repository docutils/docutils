#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for images.py image directives.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[4]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.utils import new_document


class ParserTestCase(unittest.TestCase):
    maxDiff = None

    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


try:
    int(None)
except TypeError as detail:
    int_none = detail.args[0]
else:
    int_none = ''


try:
    int('fifty')
except ValueError as detail:
    invalid_literal = detail.args[0]
else:
    invalid_literal = ''


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
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            1 argument(s) required, 0 supplied.
        <literal_block xml:space="preserve">
            .. image::
"""],
["""\
.. image:: one two three.png
""",
"""\
<document source="test data">
    <image uri="onetwothree.png">
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
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            1 argument(s) required, 0 supplied.
        <literal_block xml:space="preserve">
            .. image::
               :height: 100
               :width: 200
               :scale: 50
"""],
# If there are multiple lines in the link block, they are stripped of
# leading and trailing whitespace and joined together:
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
# The following two misspellings were detected in Docutils <= 0.8
# (the option block was started by any line starting with a colon
# which led to problems with named roles in other directives):
["""\
.. image:: picture.png
   :scale 50
""",
"""\
<document source="test data">
    <image uri="picture.png:scale50">
"""],
["""\
.. image:: picture.png
   :: 50
""",
"""\
<document source="test data">
    <image uri="picture.png::50">
"""],
# a missing leading colon went undetected also in Docutils <= 0.8:
["""\
.. image:: picture.png
   scale: 50
""",
"""\
<document source="test data">
    <image uri="picture.pngscale:50">
"""],
["""\
.. image:: picture.png
   :width: 200px
   :height: 100 em
""",
"""\
<document source="test data">
    <image height="100em" uri="picture.png" width="200px">
"""],
["""\
.. image:: picture.png
   :width:  200 Q
   :height: 100 em
""",
"""\
<document source="test data">
    <image height="100em" uri="picture.png" width="200Q">
"""],
# TODO: support CSS3 units (cf. [feature-requests:#57]
["""\
.. image:: picture.png
   :width: 50%
   :height: 10vh
""",
"""\
<document source="test data">
    <image height="10vh" uri="picture.png" width="50%">
"""],
["""\
.. image:: picture.png
   :width: 50%
   :height: 40%
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            invalid option value: (option: "height"; value: \'40%\')
            not a positive number or measure of one of the following units:
            em, ex, ch, rem, vw, vh, vmin, vmax, cm, mm, Q, in, pt, pc, px.
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :width: 50%
               :height: 40%
"""],
["""\
.. image:: picture.png
   :width: 20mc
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            invalid option value: (option: "width"; value: \'20mc\')
            not a positive number or measure of one of the following units:
            em, ex, ch, rem, vw, vh, vmin, vmax, cm, mm, Q, in, pt, pc, px, %.
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :width: 20mc
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
   :scale: -50
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            invalid option value: (option: "scale"; value: '-50')
            negative value; must be positive or zero.
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :scale: -50
"""],
["""\
.. image:: picture.png
   :scale:
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            invalid option value: (option: "scale"; value: None)
            %s.
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :scale:
""" % int_none],
["""\
.. image:: picture.png
   :height: 100
   :scale 50
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            invalid option block.
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :height: 100
               :scale 50
"""],
["""\
.. image:: picture.png
   :sale: 50
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            unknown option: "sale".
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :sale: 50
"""],
["""\
.. image:: picture.png
   :scale is: 50
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            invalid option data: extension option field name may not contain multiple words.
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :scale is: 50
"""],
["""\
.. image:: picture.png
   :scale: fifty
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            invalid option value: (option: "scale"; value: 'fifty')
            %s.
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :scale: fifty
""" % invalid_literal],
["""\
.. image:: picture.png
   :scale: 50
   :scale: 50
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            invalid option data: duplicate option "scale".
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :scale: 50
               :scale: 50
"""],
["""\
.. image:: picture.png
   :alt:

(Empty "alt" option.)
""",
"""\
<document source="test data">
    <image alt="" uri="picture.png">
    <paragraph>
        (Empty "alt" option.)
"""],
["""\
.. image:: picture.png
   :target: bigpicture.png
   :name: fig:pix
""",
"""\
<document source="test data">
    <reference refuri="bigpicture.png">
        <image ids="fig-pix" names="fig:pix" uri="picture.png">
"""],
["""\
.. image:: picture.png
   :target: indirect_
""",
"""\
<document source="test data">
    <reference name="indirect" refname="indirect">
        <image uri="picture.png">
"""],
["""\
.. image:: picture.png
   :target: a/multi/
            line/uri

.. image:: picture.png
   :target: `a multi line
            internal reference`_
""",
"""\
<document source="test data">
    <reference refuri="a/multi/line/uri">
        <image uri="picture.png">
    <reference name="a multi line internal reference" refname="a multi line internal reference">
        <image uri="picture.png">
"""],
["""\
.. image:: picture.png
   :target:
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            invalid option value: (option: "target"; value: None)
            argument required but none supplied.
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :target:
"""],
["""\
.. image:: picture.png
   :align: left
""",
"""\
<document source="test data">
    <image align="left" uri="picture.png">
"""],
["""\
.. image:: picture.png
   :align: top
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive: "top" is not a valid value for the "align" option.  Valid values for "align" are: "left", "center", "right".
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :align: top
"""],
["""\
.. |img| image:: picture.png
   :align: top
""",
"""\
<document source="test data">
    <substitution_definition names="img">
        <image align="top" alt="img" uri="picture.png">
"""],
["""\
.. |img| image:: picture.png
   :align: left
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive: "left" is not a valid value for the "align" option within a substitution definition.  Valid values for "align" are: "top", "middle", "bottom".
        <literal_block xml:space="preserve">
            image:: picture.png
               :align: left
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Substitution definition "img" empty or invalid.
        <literal_block xml:space="preserve">
            .. |img| image:: picture.png
               :align: left
"""],
["""\
.. image:: picture.png
   :align: \xe4
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "image" directive:
            invalid option value: (option: "align"; value: 'ä')
            "\xe4" unknown; choose from "top", "middle", "bottom", "left", "center", or "right".
        <literal_block xml:space="preserve">
            .. image:: picture.png
               :align: \xe4
"""],
["""
.. image:: test.png
   :target: Uppercase_

.. _Uppercase: https://docutils.sourceforge.io/
""",
"""\
<document source="test data">
    <reference name="Uppercase" refname="uppercase">
        <image uri="test.png">
    <target ids="uppercase" names="uppercase" refuri="https://docutils.sourceforge.io/">
"""],
[r"""
.. image:: path\ with\ spaces/name\ with\ spaces.png
   :target: path\ with\ spaces/
            target\ with\ spaces\ across\ lines.html
""",
"""\
<document source="test data">
    <reference refuri="path with spaces/target with spaces across lines.html">
        <image uri="path with spaces/name with spaces.png">
"""],
["""
.. image:: test.png
   :loading: embed
.. image:: test.png
   :loading: link
.. image:: test.png
   :loading: lazy
""",
"""\
<document source="test data">
    <image loading="embed" uri="test.png">
    <image loading="link" uri="test.png">
    <image loading="lazy" uri="test.png">
"""],
]


if __name__ == '__main__':
    unittest.main()
