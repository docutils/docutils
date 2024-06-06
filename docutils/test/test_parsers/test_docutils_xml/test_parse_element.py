#!/usr/bin/env python3
# :Copyright: © 2024 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""Tests for parsers/docutils_xml.py."""

from pathlib import Path
import sys
import unittest
import xml.etree.ElementTree as ET

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))

from docutils import frontend, utils
from docutils.parsers import docutils_xml


class ParseElementTestCase(unittest.TestCase):
    """Test the `docutils.xml.parse_element()` function."""
    maxDiff = None

    # supress warnings when passing `document` to `parse_element()`
    settings = frontend.get_default_settings(docutils_xml.Parser)
    settings.warning_stream = ''  # comment out to see warnings
    document = utils.new_document('xml input', settings)

    def test_element_with_child_with_text(self):
        xml = '<tip><paragraph>some text</paragraph></tip>'
        node = docutils_xml.parse_element(xml)
        self.assertEqual(xml, str(node))

    def test_tailing_text_after_root(self):
        """etree.ElementTree does not accept tailing text in the input.
        """
        xml = '<strong>text</strong>tailing text'
        with self.assertRaisesRegex(ET.ParseError, 'junk after document '):
            docutils_xml.parse_element(xml)
        # If a document is provided, report via a "loose" error system message
        # comment out ``settings.warning_stream = ''`` above to see it).
        node = docutils_xml.parse_element(xml, self.document)
        self.assertEqual('<strong>text</strong>', str(node))

    def test_nonexistent_element_type(self):
        xml = '<tip><p>some text</p></tip>'
        node = docutils_xml.parse_element(xml, self.document)
        self.assertEqual(xml, str(node))
        # see test_misc.py for the warning

    def test_junk_text(self):
        # insert text also in nodes that are not TextElement instances
        xml = '<tip>some text</tip>'
        node = docutils_xml.parse_element(xml)
        self.assertEqual(xml, str(node))
        with self.assertRaisesRegex(ValueError,
                                    'Expecting child of type <Body>,'
                                    ' not text data "some text"'):
            node.validate()

    def test_tailing_junk_text(self):
        # insert text also in nodes that are not TextElement instances
        xml = '<tip><paragraph>some text</paragraph>tailing text</tip>'
        node = docutils_xml.parse_element(xml)
        self.assertEqual(xml, str(node))
        with self.assertRaisesRegex(
            ValueError, 'Spurious text: "tailing text"'):
            node.validate()

    def test_element_with_attributes(self):
        xml = ('<image align="left" alt="a barking dog" height="3ex"'
               ' loading="embed" scale="3" uri="dog.jpg" width="4cm"/>')
        node = docutils_xml.parse_element(xml)
        self.assertEqual(xml, str(node))

    def test_element_with_invalid_attributes(self):
        """Silently accept invalid attribute names and values.

        Validation reports problems.
        """
        xml = ('<image breadth="3 cm" height="3 inch"/>')
        node = docutils_xml.parse_element(xml)
        self.assertEqual(xml, str(node))
        with self.assertRaisesRegex(ValueError,
                                    'Element <image breadth="3 cm".*invalid:\n'
                                    '.*"breadth" not one of "ids",.*\n'
                                    '.*"height" has invalid value "3 inch".\n'
                                    '.*Valid units: em ex px in cm mm pt '
                                    ):
            node.validate()


class XmlAttributesTestCase(unittest.TestCase):
    """
    Test correct parsing of the `supported element attributes`_.

    See also `AttributeTypeTests` in ../../test_nodes.py.

    __ https://docutils.sourceforge.io/
       docs/ref/doctree.html#attribute-reference
    """
    common_attributes = {'classes': [],
                         'dupnames': [],
                         'ids': [],
                         'names': []}

    def test_alt(self):  # CDATA (str)
        xml = ('<image alt="a barking dog" align="left" height="3ex"'
               '       loading="embed" scale="3" uri="dog.jpg" width="4cm"/>')
        expected = {'alt': 'a barking dog',
                    'align': 'left',
                    'height': '3ex',
                    'loading': 'embed',
                    'scale': 3,
                    'uri': 'dog.jpg',
                    'width': '4cm'}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    # 'align': CDATA (str)  → test_alt

    def test_anonymous(self):  # yesorno (int)
        xml = '<target anonymous="1" ids="target-1" refuri="example.html" />'
        expected = {'anonymous': 1,
                    'ids': ['target-1'],
                    'refuri': 'example.html'}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    def test_auto(self):  # CDATA (str) number sequence: '1' or '*'
        xml = '<footnote auto="*" backrefs="footnote-reference-2" />'
        expected = {'auto': '*',
                    'backrefs': ['footnote-reference-2']}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    # 'backrefs':  idrefs.type (list[str])  → test_auto

    def test_bullet(self):  # CDATA (str)
        xml = '<bullet_list bullet="*" classes="first x-2nd" />'
        expected = {'bullet': '*',
                    'classes': ['first', 'x-2nd']}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    # 'classes':  classnames.type (list[str])  → test_bullet

    def test_colwidth(self):  # CDATA (int) sic!
        xml = '<colspec colwidth="33" stub="1" />'
        expected = {'colwidth': 33, 'stub': 1}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    def test_delimiter(self):  # CDATA (str)
        xml = '<option_argument delimiter="=">FILE</option_argument>'
        expected = {'delimiter': '='}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    def test_dupnames(self):  # refnames.type (list[str]).
        xml = r'<section dupnames="title\ 1" ids="title-1" />'
        expected = {'dupnames': ['title 1'],
                    'ids': ['title-1']}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    def test_enumtype(self):  # EnumeratedType (str)
        xml = ('<enumerated_list enumtype="upperroman"'
               '                 prefix="(" start="2" suffix=")" />')
        expected = {'enumtype': 'upperroman',
                    'prefix': '(',
                    'start': 2,
                    'suffix': ')'}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    def test_format(self):  # NMTOKENS (str) (space-delimited list of keywords)
        xml = '<raw format="html latex" xml:space="preserve" />'
        expected = {'format': 'html latex',
                    'xml:space': 'preserve'}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    # 'height': measure (str)         → test_alt
    # 'ids':    ids.type (list[str])  → test_names

    def test_level(self):  # level (int)
        xml = ('<system_message level="3" line="21" source="string"'
               '                type="ERROR" />')
        expected = {'backrefs': [],
                    'level': 3,
                    'line': 21,
                    'source': 'string',
                    'type': 'ERROR'}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    def test_ltrim(self):  # yesorno (int)
        xml = '<substitution_definition ltrim="1" names="nbsp" />'
        expected = {'ltrim': 1, 'names': ['nbsp']}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    # 'loading': EnumeratedType (str)  → test_alt

    def test_morecols(self):  # number (int)
        xml = '<entry morecols="1" />'
        expected = {'morecols': 1}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    def test_names(self):  # refnames.type (list[str])
        #                    internal whitespace in XML escaped
        xml = r'<section ids="title-2 title-1" names="title\ 2\\ title\ 1" />'
        expected = {'ids': ['title-2', 'title-1'],
                    'names': ['title 2\\', 'title 1']}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    # 'prefix': CDATA (str)  → test_enumtype

    def test_refid(self):  # idref.type (str)
        xml = '<target refid="title-1-1"></target>'
        expected = {'refid': 'title-1-1'}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    def test_refname(self):  # refname.type (str)
        xml = '<target refname="title 2"></target>'
        expected = {'refname': 'title 2'}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    # 'refuri: CDATA (str)  → test_anonymous

    def test_rtrim(self):  # yesorno (int)
        xml = '<substitution_definition ltrim="1" names="nbsp" />'
        expected = {'ltrim': 1,
                    'names': ['nbsp']}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    # 'scale': number (int) → test_alt
    # 'source': CDATA (str) → test_title
    # 'start': number (int) → test_enumtype
    # 'stub': yesorno (int) → test_colwidth
    # 'suffix': CDATA (str) → test_enumtype

    def test_title(self):  # CDATA (str)
        xml = (r'<document ids="test-document" names="test\ document"'
               r'          source="/tmp/foo.rst" title="Test Document" />')
        expected = {'ids': ['test-document'],
                    'names': ['test document'],
                    'source': '/tmp/foo.rst',
                    'title': 'Test Document'}
        node = docutils_xml.parse_element(xml)
        self.assertEqual(node.attributes, self.common_attributes | expected)

    # 'uri': CDATA (str)                → test_alt
    # 'width' measure (str)             → test_alt
    # 'xml:space' EnumeratedType (str)  → test_format


if __name__ == '__main__':
    unittest.main()
