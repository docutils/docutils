# :Id: $Id$
# :Copyright: 2024 Günter Milde.
#
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""MathML element classes.

The module is intended for programmatic generation of MathML
and covers the part of `MathML Core`_ that is required by
Docutil's *TeX math to MathML* converter.

This module is PROVISIONAL:
the API is not settled and may change with any minor Docutils version.

.. _MathML Core: https://www.w3.org/TR/mathml-core/
"""

# Usage:
#
# >>> from mathml_elements import *

GLOBAL_ATTRIBUTES = (
    'class',  # space-separated list of element classes
    # 'data-*',  # custom data attributes (see HTML)
    'dir',  # directionality ('ltr', 'rtl')
    'displaystyle',  # True: normal, False: compact
    'id',  # unique identifier
    # 'mathbackground',  # color definition, deprecated
    # 'mathcolor',  # color definition, deprecated
    # 'mathsize',  # font-size, deprecated
    'nonce',  # cryptographic nonce ("number used once")
    'scriptlevel',  # math-depth for the element
    'style',  # CSS styling declarations
    'tabindex',  # indicate if the element takes input focus
    )
"""Global MathML attributes

https://w3c.github.io/mathml-core/#global-attributes
"""


# Base classes
# ------------

class MathElement:
    """Base class for MathML elements."""

    nchildren = None
    """Expected number of children or None"""
    # cf. https://www.w3.org/TR/MathML3/chapter3.html#id.3.1.3.2
    parent = None
    """Parent node in MathML element tree."""
    _level = 0  # indentation level (static class variable)
    xml_entities = {
        # for invalid and invisible characters
        ord('<'): '&lt;',
        ord('>'): '&gt;',
        ord('&'): '&amp;',
        0x2061: '&ApplyFunction;',
    }

    def __init__(self, *children, **attributes):
        """Set up node with `children` and `attributes`.

        Attribute names are normalised to lowercase.
        You may use "CLASS" to set a "class" attribute.
        Attribute values are converted to strings
        (with True -> "true" and False -> "false").

        >>> math(CLASS='test', level=3, split=True)
        math(class='test', level='3', split='true')
        >>> math(CLASS='test', level=3, split=True).toprettyxml()
        '<math class="test" level="3" split="true">\n</math>'

        """
        self.attrib = {k.lower(): self.a_str(v)
                       for k, v in attributes.items()}
        self.children = []
        self.extend(children)

    @staticmethod
    def a_str(v):
        # Return string representation for attribute value `v`.
        if isinstance(v, bool):
            return str(v).lower()
        return str(v)

    def __repr__(self):
        content = [repr(item) for item in self]
        if hasattr(self, 'data'):
            content.append(repr(self.data))
        if getattr(self, 'switch', None):
            content.append('switch=True')
        content += [f'{k}={v!r}' for k, v in self.items() if v is not None]
        return self.__class__.__name__ + '(%s)' % ', '.join(content)

# Emulate dictionary access methods for attributes
# and list-like interface to the child elements
# (differs from `docutils.nodes.Element` dict/list interface).

    def get(self, key, default=None):
        return self.attrib.get(key, default)

    def set(self, key, value):
        self.attrib[key] = self.a_str(value)

    def items(self):
        return self.attrib.items()

    def subnodes(self):
        """Return iterator over all subnodes, including nested ones."""
        for child in self.children:
            yield child
            yield from child.subnodes()

    def __len__(self):
        return len(self.children)

    def __getitem__(self, key):
        return self.children.__getitem__(key)

    def __setitem__(self, key, element):
        element.parent = self
        self.children.__setitem__(key, element)

    def __delitem__(self, key):
        self.children.__delitem__(key)

    def __iter__(self):
        return self.children.__iter__()

    def full(self):
        """Return boolean indicating whether children may be appended."""
        return self.nchildren is not None and len(self) >= self.nchildren

    def close(self):
        """Close element and return first non-full anchestor or None."""
        self.nchildren = len(self)  # mark node as full
        parent = self.parent
        while parent is not None and parent.full():
            parent = parent.parent
        return parent

    def append(self, element):
        """Append `element` and return new "current node" (insertion point).

        Append as child element and set the internal `parent` attribute.

        If self is already full, raise TypeError.

        If self is full after appending, call `self.close()`
        (returns first non-full anchestor or None) else return `self`.
        """
        if self.full():
            raise TypeError(f'Element "{self}" already full!')
        self.children.append(element)
        element.parent = self
        if self.full():
            return self.close()
        return self

    def extend(self, elements):
        """Sequentially append `elements`. Return new "current node".

        Raise TypeError if overfull.
        """
        current_node = self
        for element in elements:
            current_node = self.append(element)
        return current_node

    def pop(self, index=-1):
        element = self[index]
        del self[index]
        return element

    def is_block(self):
        """Return true, if `self` or a parent has ``display='block'``."""
        try:
            return self.get('display') == 'block'
        except KeyError:
            try:
                return self.parent.is_block()
            except AttributeError:
                return False

    # Conversion to (pretty) XML string
    def toprettyxml(self):
        """Return XML representation of self as string."""
        return ''.join(self._xml())

    def _xml(self, level=0):
        if self.nchildren is not None and len(self) < self.nchildren:
            raise ValueError(f'Node {self.xml_starttag()} misses children.'
                             ' Incomplete source?')
        return [self.xml_starttag(),
                *self._xml_body(level),
                '</%s>' % self.__class__.__name__]

    def xml_starttag(self):
        attrs = (f'{k}="{v}"' for k, v in self.items() if v is not None)
        return '<%s>' % ' '.join((self.__class__.__name__, *attrs))

    def _xml_body(self, level=0):
        xml = []
        for child in self.children:
            xml.extend(['\n', '  ' * (level+1)])
            xml.extend(child._xml(level+1))
        xml.extend(['\n', '  ' * level])
        return xml

# >>> n2 = math(mn(2))
# >>> n2
# math(mn(2))
# >>> n2.toprettyxml()
# '<math>\n  <mn>2</mn>\n</math>'
# >>> len(n2)
# 1
# >>> n2.extend([mo('!')])
# math(mn(2), mo('!'))
# >>> eq3 = math(id='eq3', display='block')
# >>> eq3
# math(id='eq3', display='block')
# >>> eq3.toprettyxml()
# '<math id="eq3" display="block">\n</math>'
# >>> len(eq3)
# 0
# >>> math(CLASS='bold').xml_starttag()
# '<math class="bold">'
# >>> n2.is_block()
# False
# >>> node = n2.append(mrow())
# >>> node.is_block()
# False
# >>> eq3.is_block()
# True
# >>> node = eq3.append(mrow())
# >>> node.is_block()
# True
# >>> nested = math(math(math(CLASS='three'), CLASS='two'), CLASS='one')
# >>> [node for node in nested.subnodes()]
# [math(math(class='three'), class='two'), math(class='three')]


# Group sub-expressions in a horizontal row
#
# The elements <msqrt>, <mstyle>, <merror>, <mpadded>, <mphantom>,
# <menclose>, <mtd>, <mscarry>, and <math> treat their contents
# as a single inferred mrow formed from all their children.
# (https://www.w3.org/TR/mathml4/#presm_inferredmrow)
#
# MathML Core uses the term "anonymous mrow element".

class MathRow(MathElement):
    """Base class for elements treating content as a single mrow."""


# 2d Schemata

class MathSchema(MathElement):
    """Base class for schemata expecting 2 or more children.

    The special attribute `switch` indicates that the last two child
    elements are in reversed order and must be switched before XML-export.
    """
    nchildren = 2

    def __init__(self, *children, **kwargs):
        self.switch = kwargs.pop('switch', False)
        math.__init__(self, *children, **kwargs)

    def append(self, element):
        """Append element. Normalize order and close if full."""
        current_node = super().append(element)
        if self.switch and self.full():
            self[-1], self[-2] = self[-2], self[-1]
            self.switch = False
        return current_node

# >>> MathSchema(switch=True, display=True)
# MathSchema(switch=True, display='true')
# >>> MathSchema(MathElement(), switch=True)
# MathSchema(MathElement(), switch=True)
# >>> MathSchema(MathElement(id='c1'), MathElement(id='c2'), switch=True)
# MathSchema(MathElement(id='c2'), MathElement(id='c1'))
# >>> MathSchema(MathElement(), MathElement(), MathElement())
# Traceback (most recent call last):
# ...
# TypeError: Element "MathSchema(MathElement(), MathElement())" already full!


# Token elements represent the smallest units of mathematical notation which
# carry meaning.

class MathToken(MathElement):
    """Token Element: contains textual data instead of children.

    Expect text data on initialisation.
    """
    nchildren = 0

    def __init__(self, data, **attributes):
        self.data = data
        super().__init__(**attributes)

    def _xml_body(self, level=0):
        return [str(self.data).translate(self.xml_entities)]


# MathML element classes
# ----------------------

class math(MathRow):
    """Top-level MathML element, a single mathematical formula."""


# Token elements
# ~~~~~~~~~~~~~~

class mtext(MathToken):
    """Arbitrary text with no notational meaning."""


class mi(MathToken):
    """Identifier, such as a function name, variable or symbolic constant."""


class mn(MathToken):
    """Numeric literal.

    Normally a sequence of digits with a possible separator (a dot or a comma).
    """


class mo(MathToken):
    """Operator, Fence, Separator, or Accent.

    Besides operators in strict mathematical meaning, this element also
    includes "operators" like parentheses, separators like comma and
    semicolon, or "absolute value" bars.
    """

# >>> mo('<')
# mo('<')
# >>> mo('<')._xml()
# ['<mo>', '&lt;', '</mo>']


class mspace(MathElement):
    """Blank space, whose size is set by its attributes.

    Takes additional attributes `depth`, `height`, `width`.
    Takes no children and no text.

    See also `mphantom`.
    """
    nchildren = 0


# General Layout Schemata
# ~~~~~~~~~~~~~~~~~~~~~~~

class mrow(MathRow):
    """Generic element to group children as a horizontal row.

    Removed on closing if not required (see `mrow.close()`).
    """

    def transfer_attributes(self, other):
        # Update dictionary `other.attrib` with self.attrib.
        # List values (class, style) are appended to existing values,
        # other values replace existing values.
        for k, v in self.items():
            if k in ('class', 'style') and v:
                try:
                    other.attrib[k] += ' ' + v
                    continue
                except (KeyError, TypeError):
                    pass
            other.attrib[k] = v

    def close(self):
        """Close element and return first non-full parent or None.

        Remove <mrow> if it has only one child element.
        """
        parent = self.parent
        # replace `self` with single child
        if parent is not None and len(self) == 1:
            child = self[0]
            try:
                parent[parent.children.index(self)] = child
                child.parent = parent
            except (AttributeError, ValueError):
                return None
            self.transfer_attributes(child)
        return super().close()

# >>> row = mrow(mi('i', CLASS='boldmath'), mathvariant='normal', CLASS='test')
# >>> tree = math(row)
# >>> row.close()
# math(mi('i', class='boldmath test', mathvariant='normal'))


class mfrac(MathSchema):
    """Fractions or fraction-like objects such as binomial coefficients."""


class msqrt(MathRow):
    """Square root. See also `mroot`."""
    nchildren = 1  # \sqrt expects one argument or a group


class mroot(MathSchema):
    """Roots with an explicit index. See also `msqrt`."""


class mstyle(MathRow):
    """Style Change.

    In modern browsers, <mstyle> is equivalent to an <mrow> element.
    However, <mstyle> may still be relevant for compatibility with MathML
    implementations outside browsers.
    """


class merror(MathRow):
    """Display contents as error messages."""


class menclose(MathRow):
    """Renders content inside an enclosing notation...

    ... specified by the notation attribute.

    Non-standard: Do not use on the Web.
    """
    nchildren = 1  # \boxed expects one argument or a group


class mphantom(MathRow):
    """Placeholder: Rendered invisibly but dimensions are kept."""
    nchildren = 1  # \phantom expects one argument or a group


# Script and Limit Schemata
# ~~~~~~~~~~~~~~~~~~~~~~~~~

class msub(MathSchema):
    """Attach a subscript to an expression."""


class msup(MathSchema):
    """Attach a superscript to an expression."""


class msubsup(MathSchema):
    """Attach both a subscript and a superscript to an expression."""
    nchildren = 3

# >>> msub(mi('x'), mo('-'))
# msub(mi('x'), mo('-'))
# >>> msubsup(mi('base'), mi('sub'), mi('super'))
# msubsup(mi('base'), mi('sub'), mi('super'))
# >>> msubsup(mi('base'), mi('super'), mi('sub'), switch=True)
# msubsup(mi('base'), mi('sub'), mi('super'))


class munder(msub):
    """Attach an accent or a limit under an expression."""


class mover(msup):
    """Attach an accent or a limit over an expression."""


class munderover(msubsup):
    """Attach accents or limits both under and over an expression."""


# >>> munder(mi('lim'), mo('-'), accent='false')
# munder(mi('lim'), mo('-'), accent='false')
# >>> mu = munder(mo('-'), accent='false', switch=True)
# >>> mu
# munder(mo('-'), switch=True, accent='false')
# >>> mu.append(mi('lim'))
# >>> mu
# munder(mi('lim'), mo('-'), accent='false')
# >>> mu.append(mi('lim'))
# Traceback (most recent call last):
# TypeError: Element "munder(mi('lim'), mo('-'), accent='false')" already full!
# >>> munder(mo('-'), mi('lim'), accent='false', switch=True).toprettyxml()
# '<munder accent="false">\n  <mi>lim</mi>\n  <mo>-</mo>\n</munder>'


# Tabular Math
# ~~~~~~~~~~~~

class mtable(MathElement):
    """Table or matrix element."""


class mtr(MathRow):
    """Row in a table or a matrix."""


class mtd(MathRow):
    """Cell in a table or a matrix"""

# >>> t = mtable(displaystyle=True)
# >>> math(t).toprettyxml()
# '<math>\n  <mtable displaystyle="true">\n  </mtable>\n</math>'
