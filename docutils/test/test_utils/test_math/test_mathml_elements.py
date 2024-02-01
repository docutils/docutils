#! /usr/bin/env python3
# :Copyright: © 2024 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause


"""
Test module for utils/__init__.py.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import docutils.utils.math.mathml_elements as mml

TEST_ROOT = Path(__file__).parent  # ./test/ from the docutils root

# The index of MathML elements
# on https://developer.mozilla.org/en-US/docs/Web/MathML/Element
# for Nr. of children see https://www.w3.org/TR/mathml4/#presm_reqarg_table
# (Elements with 1* show 1 or * depending on the corresponding TeX macro.)
#   Element     Nr. of children    Description                comment
mathml_elements = """
    <maction>         * Bound actions to sub-expressions      DEPRECATED
    <annotation>      0 Data annotations                      NOT IMPLEMENTED
    <annotation-xml>  * XML annotations                       NOT IMPLEMENTED
    <math>            * Top-level element
    <menclose>        1 Enclosed contents (non-standard)      deprecated
    <merror>          * Enclosed syntax error messages
    <mfenced>         * Parentheses (non-standard)            DEPRECATED
    <mfrac>           2 Fraction
    <mi>              0 Identifier
    <mmultiscripts>   * Prescripts and tensor indices         NOT IMPLEMENTED
    <mn>              0 Number
    <mo>              0 Operator (and similar)
    <mover>           2 Overscript
    <mpadded>         * Extra padding
    <mphantom>        1 Invisible content reserving space
    <mroot>           2 Radical with specified index
    <mrow>            * Grouped sub-expressions
    <ms>              0 String literal                        NOT IMPLEMENTED
    <mspace>          0 Space
    <msqrt>           1 Square root without an index
    <mstyle>          * Style change (for compatibility)
    <msub>            2 Subscript
    <msup>            2 Superscrip
    <msubsup>         3 Subscript-superscript pair
    <mtable>          * Table or matrix
    <mtd>             * Cell in a table or a matrix
    <mtext>           0 Text
    <mtr>             * Row in a table or a matrix
    <munder>          2 Underscript
    <munderover>      3 Underscript-overscript pair
    <semantics>       * annotations (e.g. text source)       NOT IMPLEMENTED
    """


class MathElementTests(unittest.TestCase):
    """Test the <MathElement> base class."""

    prettyXML = """\
<math id="root">
  <math id="c1"></math>
  <math id="c2">
    <math id="cc1"></math>
  </math>
</math>"""

    def test__init__(self):
        """Instantiate a MathElement object and test its properties."""

        # No arguments required, the class name is used as XML tag:
        e1 = mml.MathElement()
        self.assertEqual(e1.tag, 'MathElement')

        # Positional arguments are stored as children,
        # named arguments are stored as element attributes:
        # * argument names are downcased,
        # * attribute values must be `str`, `boolean`, or numerical
        #   and are stored as `str` (cf. test_a_str() below).
        e0 = mml.MathElement(e1, CLASS='root', scriptlevel=1)
        # len() returns the number of children
        self.assertEqual(len(e0), 1)
        # Attributes can be accessed using get(), set(), items(), and keys().
        self.assertEqual(e0.get('class'), 'root')
        e0.set('scriptlevel', 2)
        e0.set('displaystyle', True)
        self.assertEqual(e0.attrib, {'class': 'root',
                                     'scriptlevel': '2',
                                     'displaystyle': 'true'})

    def test_children(self):
        # positional arguments are stored as children.
        e4 = mml.MathElement(id='e4')
        e3 = mml.MathElement(id='e3')
        e2 = mml.MathElement(e3, e4, id='e2')
        e1 = mml.MathElement(id='e1')
        e0 = mml.MathElement(e1, e2, id='e0')
        # xml.etree uses list syntax to access them
        self.assertEqual(e0[0], e1)
        self.assertEqual(e0[1][-1], e4)
        self.assertEqual(e0[:], [e1, e2])  # list of e0's children
        # The default iterator yields the immediate children
        self.assertEqual(tuple(e0), (e1, e2))
        # The `iter()` method returns `self` and all descendants
        self.assertEqual(tuple(e0.iter()), (e0, e1, e2, e3, e4))

        # more list-like functions are tested below...

    def test_a_str(self):
        # test the XML attribute normalisation
        #
        # Values are converted to `str`:
        self.assertEqual(mml.MathElement.a_str(-2), '-2')
        self.assertEqual(mml.MathElement.a_str(1), '1')
        self.assertEqual(mml.MathElement.a_str(0), '0')
        # Booleans are downcased:
        self.assertEqual(mml.MathElement.a_str(True), 'true')
        self.assertEqual(mml.MathElement.a_str(False), 'false')
        # Strings are left as-is:
        self.assertEqual(mml.MathElement.a_str('+1'), '+1')
        self.assertEqual(mml.MathElement.a_str('True'), 'True')
        self.assertEqual(mml.MathElement.a_str(''), '')

    def test__repr__(self):
        """The representation should match the code to instantiate."""
        # Watch the idempotent normalisation:
        # * attribute names are downcased,
        # * XML attribute values are converted to strings
        e1 = mml.MathElement(mml.MathElement(level=2), CLASS='root')
        self.assertEqual(repr(e1),
                         "MathElement(MathElement(level='2'), class='root')")
        # Also report non-default number of children: (as `int`, not `str`)
        e1[0].nchildren = 1
        self.assertEqual(repr(e1[0]), "MathElement(nchildren=1, level='2')")

    def test__str__(self):
        """Informal string representation for debugging and error reports."""
        e1 = mml.MathElement(mml.MathElement(level=2), CLASS='root')
        self.assertEqual(str(e1), 'MathElement(MathElement())')

    def test__setitem__(self):
        """Setting an item also sets the `parent` attribute on the child."""
        e1 = mml.MathElement()
        e1[:] = (mml.MathElement(id=1), mml.MathElement(id=2))
        self.assertEqual(e1[0].parent, e1)
        self.assertEqual(e1[0].get('id'), '1')
        self.assertEqual(e1[1].get('id'), '2')
        e1[0] = mml.MathElement(id=3)
        self.assertEqual(e1[0].get('id'), '3')
        self.assertEqual(e1[0].parent, e1)
        e1[2:] = (mml.MathElement(id=4), mml.MathElement(id=5))
        e1[1:] = []
        e1.nchildren = 2
        with self.assertRaises(TypeError) as cm:
            e1[:] = (mml.MathElement(id=6), mml.MathElement(id=7))
        self.assertIn('takes only 2 children', str(cm.exception))

    def test_is_full(self):
        # A node is "full", if the number of children equals or exceeds
        # the number of accepted children given in the "nchildren" attribute.
        e1 = mml.MathElement()
        self.assertTrue(e1.nchildren is None)  # no limit to number of children
        self.assertFalse(e1.is_full())
        e1.nchildren = 1  # element expects/accepts one child.
        self.assertFalse(e1.is_full())
        e1.nchildren = 0  # element expects/accepts no children.
        self.assertTrue(e1.is_full())

    def test_close(self):
        """Closing an element returns first non-closed anchestor or None."""
        e1 = mml.MathElement(mml.MathElement())
        self.assertEqual(e1[0].close(), e1)
        self.assertTrue(e1.close() is None)
        # When all anchestors are closed, return None as well
        self.assertTrue(e1[0].close() is None)

    def test_append(self):
        """Test special features of the "append()" method."""
        e1 = mml.MathElement()
        e1.nchildren = 2  # element expects/accepts two children.

        # Appending to an element sets the internal "parent" attribute ...
        result = e1.append(mml.MathElement(id='c1'))
        self.assertEqual(e1[0].parent, e1)
        # ... which is hidden in XML ...
        self.assertEqual(e1[0].toxml(), '<MathElement id="c1"></MathElement>')
        # ... and returns the new "insertion point".
        # If more children may be appended, return self
        self.assertEqual(result, e1)
        # If the element is "full" after appending the child,
        # a non-full anchestor or None is returned:
        result = e1.append(mml.MathElement(id='c2'))
        self.assertTrue(result is None)

        # Trying to append to an element that is "full" returns an error:
        with self.assertRaises(TypeError) as cm:
            result = e1.append(mml.MathElement())
        self.assertEqual(str(cm.exception),
                         'Element "MathElement(MathElement(), MathElement())"'
                         ' takes only 2 children.')

    def test_extend(self):
        e1 = mml.MathElement()
        c1 = mml.MathElement(id='c1')
        c2 = mml.MathElement(id='c2')
        e1.extend([c1, c2])
        self.assertEqual([*e1], [c1, c2])

    def test_pop(self):
        c1 = mml.MathElement(id='c1')
        c2 = mml.MathElement(id='c2')
        e1 = mml.MathElement(c1, c2)
        last_element = e1.pop()
        self.assertEqual(last_element, c2)
        self.assertEqual([*e1], [c1])
        first_element = e1.pop(0)
        self.assertEqual(first_element, c1)
        with self.assertRaises(IndexError):
            e1.pop()

    def test_in_block(self):
        # Return True, if `self` or an anchestor is a block-level element:
        e1 = mml.math(display='block')
        e2 = mml.math(mml.math(id='e3'), id='e2')
        self.assertTrue(e1.in_block())
        self.assertFalse(e2.in_block())
        self.assertFalse(e2[0].in_block())
        e1.append(e2)
        self.assertTrue(e2.in_block())
        self.assertTrue(e2[0].in_block())

    def test_indent_xml(self):
        """Modify `text` and `tail` to get indented XML output."""
        c1 = mml.math(id='c1')
        cc1 = mml.math(id='cc1')
        c2 = mml.math(cc1, id='c2')
        root = mml.math(c1, c2, id='root')
        self.assertTrue('\n' not in str(root))
        root.indent_xml()
        self.assertEqual(root.toxml(), self.prettyXML)
        # You can easily remove the indentation (but not the newlines):
        root.indent_xml(space='')
        self.assertEqual(c2.toxml(),
                         '<math id="c2">\n<math id="cc1"></math>\n</math>\n')
        # Reverting `indent_xml()` requires iterating over all descendants
        root.unindent_xml()
        self.assertEqual(c2.toxml(),
                         '<math id="c2"><math id="cc1"></math></math>')

    def test_unindent_xml(self):
        # see also last assertion in `test_indent_xml()`
        e1 = mml.math(mml.mtext('Hallo welt!\n'))
        e1.indent_xml()
        self.assertEqual(e1.toxml(),
                         '<math>\n  <mtext>Hallo welt!\n</mtext>\n</math>')
        # don't strip whitespace from MathToken's text attributes:
        e1.unindent_xml()
        self.assertEqual(e1.toxml(),
                         '<math><mtext>Hallo welt!\n</mtext></math>')

    def test_toxml(self):
        """XML representation of the element/subtree as `str`."""
        e1 = mml.math(mml.math(level=2), CLASS='root')
        self.assertEqual(e1.toxml(),
                         '<math class="root"><math level="2"></math></math>')


class MathSchemaTests(unittest.TestCase):
    """Test `MathSchema` and derived element classes."""

    def test__init__(self):
        # the optional `switch` argument is stored as internal attribute
        ms1 = mml.MathSchema(switch=True, id='ms1')
        self.assertEqual(repr(ms1), "MathSchema(switch=True, id='ms1')")
        # internal attributes are not exported to XML.
        self.assertEqual(ms1.toxml(),
                         '<MathSchema id="ms1"></MathSchema>')
        # the default value is dropped from ``repr()``
        ms1.switch = False
        self.assertEqual(repr(ms1), "MathSchema(id='ms1')")

    def test__init__full(self):
        # when initialized with `nchildren` children and `switch` True,
        # the children are switched and `switch` is reset:
        ms2 = mml.MathSchema(mml.mn(1), mml.mn(2), switch=True)
        self.assertEqual(repr(ms2), "MathSchema(mn('2'), mn('1'))")
        self.assertEqual(ms2.toxml(),
                         '<MathSchema><mn>2</mn><mn>1</mn></MathSchema>')

    def test_append(self):
        # appending normalizes the order before switching
        ms1 = mml.MathSchema(mml.mn(1), switch=True)
        ms1.append(mml.mn(2))
        self.assertEqual(repr(ms1), "MathSchema(mn('2'), mn('1'))")
        # appending to a closed element raises TypeError
        with self.assertRaises(TypeError) as cm:
            ms1.append(mml.mn(3))
        self.assertIn('takes only 2 children', str(cm.exception))


class MathTokenTests(unittest.TestCase):
    """Test MathToken and derived element classes."""

    def test__init__(self):
        # requires one argument `text` that is stored as the element's text
        self.assertEqual(repr(mml.mi('x')), "mi('x')")
        # the argument is converted to a `str`
        self.assertEqual(repr(mml.mn(42)), "mn('42')")
        # only one positional argument allowed:
        with self.assertRaises(TypeError):
            mml.mo('[', 'stretchy')
        # text must be `str` or numerical value:
        with self.assertRaises(ValueError) as cm:
            mml.mi(mml.mtext('out'))
        self.assertIn('MathToken element expects `str` or number',
                      str(cm.exception))

        # optional named arguments become XML attributes
        e1 = mml.mo('[', stretchy=False)
        self.assertEqual(e1.toxml(), '<mo stretchy="false">[</mo>')

    def test_append(self):
        # MathTokens don't take child elements.
        # Appending to MathTokens raises an exception.
        e1 = mml.mo('[')
        self.assertEqual(e1.nchildren, 0)
        with self.assertRaises(TypeError) as cm:
            e1.append(mml.mi('x'))
        self.assertEqual(str(cm.exception),
                         'Element "mo(\'[\')" does not take children.')
        with self.assertRaises(TypeError) as cm:
            e1[:] = [mml.mn(3)]
        self.assertEqual(str(cm.exception),
                         'Element "mo(\'[\')" does not take children.')


class mrowTests(unittest.TestCase):
    """Test the `mrow` element class."""

    def test_transfer_attributes(self):
        e1 = mml.mrow(level=1, CLASS='cls1', style='rule1;')
        e2 = mml.mrow(level=2, CLASS='cls2')
        e3 = mml.mrow(level=3, style='rule3')
        # e1.attrib -> e2.attrib
        e1.transfer_attributes(e2)
        self.assertEqual(e2.get('level'), '1')
        self.assertEqual(e2.get('class'), 'cls2 cls1')
        self.assertEqual(e2.get('style'), 'rule1;')
        # e3.attrib -> e1.attrib
        e3.transfer_attributes(e1)
        self.assertEqual(e1.get('level'), '3')
        self.assertEqual(e1.get('class'), 'cls1')
        self.assertEqual(e1.get('style'), 'rule1; rule3')

    def test_close_only_child(self):
        # Remove <mrow> if it has a parent and only one child element.
        # NOTE: this feature may be removed.
        c1 = mml.math(CLASS='c1')
        row1 = mml.mrow(c1, CLASS='row1')
        # if possible, attach child to parent
        row1.close()  # no parent, no removal
        self.assertEqual(c1.parent, row1)

        root = mml.math(row1)  # provide a parent
        row1.close()  # try again
        self.assertEqual(c1.parent, root)
        self.assertEqual(root.toxml(),
                         '<math><math class="c1 row1"></math></math>')


class MathMLElementTests(unittest.TestCase):
    """Test classes for "MathML Core" elements."""

    def test_mathml_elements(self):
        for line in mathml_elements.strip().splitlines():
            element, nchildren, description = line.split(maxsplit=2)
            element = element.strip('<>')
            if 'DEPRECATED' in description or 'NOT IMPLEMENTED' in description:
                continue
            cls = getattr(mml, element)
            if issubclass(cls, mml.MathToken):
                e = cls('x')
                self.assertEqual(e.toxml(), f'<{element}>x</{element}>')
            else:
                e = cls()
                self.assertEqual(e.toxml(), f'<{element}></{element}>')
            if nchildren == '*':
                self.assertTrue(e.nchildren is None,
                                f'{element}.nchildren == {e.nchildren}')
            else:
                self.assertEqual(e.nchildren, int(nchildren),
                                 f'{element}.nchildren == {e.nchildren}')


if __name__ == '__main__':
    unittest.main()
