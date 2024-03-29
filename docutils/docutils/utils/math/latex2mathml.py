# :Id: $Id$
# :Copyright: © 2005 Jens Jørgen Mortensen [1]_
#             © 2010, 2021, 2024 Günter Milde.
#
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause
#
# .. [1] the original `rst2mathml.py` in `sandbox/jensj/latex_math`

"""Convert LaTex maths code into presentational MathML.

This module is provisional:
the API is not settled and may change with any minor Docutils version.
"""

# Usage:
#
# >>> from latex2mathml import *

import re
import unicodedata

from docutils.utils.math import (MathError, mathalphabet2unichar,
                                 tex2unichar, toplevel_code)


# Character data
# --------------

# LaTeX math macro to Unicode mappings.
# Character categories.

# identifiers -> <mi>

letters = {'hbar': 'ℏ'}  # Compatibility mapping: \hbar resembles italic ħ
#                          "unicode-math" unifies \hbar and \hslash to ℏ.
letters.update(tex2unichar.mathalpha)

ordinary = tex2unichar.mathord  # Miscellaneous symbols

# special case: Capital Greek letters: (upright in TeX style)
greek_capitals = {
    'Phi': '\u03a6', 'Xi': '\u039e', 'Sigma': '\u03a3',
    'Psi': '\u03a8', 'Delta': '\u0394', 'Theta': '\u0398',
    'Upsilon': '\u03d2', 'Pi': '\u03a0', 'Omega': '\u03a9',
    'Gamma': '\u0393', 'Lambda': '\u039b'}

# functions -> <mi>
functions = {
    # functions with a space in the name
    'liminf': 'lim\u202finf',
    'limsup': 'lim\u202fsup',
    'injlim': 'inj\u202flim',
    'projlim': 'proj\u202flim',
    # embellished function names (see handle_cmd() below)
    'varlimsup': 'lim',
    'varliminf': 'lim',
    'varprojlim': 'lim',
    'varinjlim': 'lim',
    # custom function name
    'operatorname': None,
}
functions.update((name, name) for name in
                 ('arccos', 'arcsin', 'arctan', 'arg',  'cos',
                  'cosh',   'cot',    'coth',   'csc',  'deg',
                  'det',    'dim',    'exp',    'gcd',  'hom',
                  'ker',    'lg',     'ln',     'log',  'Pr',
                  'sec',    'sin',    'sinh',   'tan',  'tanh'))
# Function with limits: 'lim', 'sup', 'inf', 'max', 'min':
# use <mo> to allow "movablelimits" attribute (see below).

# modulo operator/arithmetic
modulo_functions = {
    # cmdname: (binary, named, parentheses, padding)
    'bmod': (True,  True,  False, '0.278em'),  # a mod n
    'pmod': (False, True,  True,  '0.444em'),  # a  (mod n)
    'mod':  (False, True,  False, '0.667em'),  # a  mod n
    'pod':  (False, False, True,  '0.444em'),  # a  (n)
    }


# math font selection -> <mi mathvariant=...> or <mstyle mathvariant=...>
# TODO: the "mathvariant" attribute is not supported by Chromium and
# deprecated in the MathML specs (except with value "normal" for single chars).
math_alphabets = {
    # 'cmdname':  'mathvariant value'        # package
    'mathbb':     'double-struck',           # amssymb
    'mathbf':     'bold',
    'mathbfit':   'bold-italic',             # isomath
    'mathcal':    'script',
    'mathfrak':   'fraktur',                 # amssymb
    'mathit':     'italic',
    'mathrm':     'normal',
    'mathscr':    'script',                  # mathrsfs
    'mathsf':     'sans-serif',
    'mathsfbfit': 'sans-serif-bold-italic',  # isomath
    'mathsfit':   'sans-serif-italic',       # isomath
    'mathtt':     'monospace',
    # unsupported: bold-fraktur
    #              bold-script
    #              bold-sans-serif
}

# operator, fence, or separator -> <mo>

stretchables = {
    # extensible delimiters allowed in left/right cmds
    'backslash':   '\\',
    'uparrow':     '\u2191',  # ↑ UPWARDS ARROW
    'downarrow':   '\u2193',  # ↓ DOWNWARDS ARROW
    'updownarrow': '\u2195',  # ↕ UP DOWN ARROW
    'Uparrow':     '\u21d1',  # ⇑ UPWARDS DOUBLE ARROW
    'Downarrow':   '\u21d3',  # ⇓ DOWNWARDS DOUBLE ARROW
    'Updownarrow': '\u21d5',  # ⇕ UP DOWN DOUBLE ARROW
    'lmoustache':  '\u23b0',  # ⎰ … CURLY BRACKET SECTION
    'rmoustache':  '\u23b1',  # ⎱ … LEFT CURLY BRACKET SECTION
    'arrowvert':   '\u23d0',  # ⏐ VERTICAL LINE EXTENSION
    'bracevert':   '\u23aa',  # ⎪ CURLY BRACKET EXTENSION
    'lvert':      '|',        # left  |
    'lVert':      '\u2016',   # left  ‖
    'rvert':      '|',        # right |
    'rVert':      '\u2016',   # right ‖
    'Arrowvert':  '\u2016',   # ‖
}
stretchables.update(tex2unichar.mathfence)
stretchables.update(tex2unichar.mathopen)   # Braces
stretchables.update(tex2unichar.mathclose)  # Braces

# >>> print(' '.join(sorted(set(stretchables.values()))))
# [ \ ] { | } ‖ ↑ ↓ ↕ ⇑ ⇓ ⇕ ⌈ ⌉ ⌊ ⌋ ⌜ ⌝ ⌞ ⌟ ⎪ ⎰ ⎱ ⏐ ⟅ ⟆ ⟦ ⟧ ⟨ ⟩ ⟮ ⟯ ⦇ ⦈

operators = {
    # negated symbols without pre-composed Unicode character
    'nleqq':      '\u2266\u0338',  # ≦̸
    'ngeqq':      '\u2267\u0338',  # ≧̸
    'nleqslant':  '\u2a7d\u0338',  # ⩽̸
    'ngeqslant':  '\u2a7e\u0338',  # ⩾̸
    'ngtrless':   '\u2277\u0338',  # txfonts
    'nlessgtr':   '\u2276\u0338',  # txfonts
    'nsubseteqq': '\u2AC5\u0338',  # ⫅̸
    'nsupseteqq': '\u2AC6\u0338',  # ⫆̸
    # compatibility definitions:
    'centerdot': '\u2B1D',  # BLACK VERY SMALL SQUARE | mathbin
    'varnothing': '\u2300',  # ⌀ DIAMETER SIGN | empty set
    'varpropto': '\u221d',  # ∝ PROPORTIONAL TO | sans serif
    'triangle': '\u25B3',  # WHITE UP-POINTING TRIANGLE | mathord
    'triangledown': '\u25BD',  # WHITE DOWN-POINTING TRIANGLE | mathord
    # alias commands:
    'dotsb': '\u22ef',  # ⋯ with binary operators/relations
    'dotsc': '\u2026',  # … with commas
    'dotsi': '\u22ef',  # ⋯ with integrals
    'dotsm': '\u22ef',  # ⋯ multiplication dots
    'dotso': '\u2026',  # … other dots
    # functions with movable limits (requires <mo>)
    'lim': 'lim',
    'sup': 'sup',
    'inf': 'inf',
    'max': 'max',
    'min': 'min',
}
operators.update(tex2unichar.mathbin)    # Binary symbols
operators.update(tex2unichar.mathrel)    # Relation symbols, arrow symbols
operators.update(tex2unichar.mathpunct)  # Punctuation
operators.update(tex2unichar.mathop)     # Variable-sized symbols
operators.update(stretchables)


# special cases

thick_operators = {
    # style='font-weight: bold;'
    'thicksim':       '\u223C',  # ∼
    'thickapprox':    '\u2248',  # ≈
}

small_operators = {
    # mathsize='75%'
    'shortmid':       '\u2223',  # ∣
    'shortparallel':  '\u2225',  # ∥
    'nshortmid':      '\u2224',  # ∤
    'nshortparallel': '\u2226',  # ∦
    'smallfrown':     '\u2322',  # ⌢ FROWN
    'smallsmile':     '\u2323',  # ⌣ SMILE
    'smallint':       '\u222b',  # ∫ INTEGRAL
}

# Operators and functions with limits above/below in display formulas
# and in index position inline (movablelimits=True)
movablelimits = ('bigcap', 'bigcup', 'bigodot', 'bigoplus', 'bigotimes',
                 'bigsqcup', 'biguplus', 'bigvee', 'bigwedge',
                 'coprod', 'intop', 'ointop', 'prod', 'sum',
                 'lim', 'max', 'min', 'sup', 'inf')
# Depending on settings, integrals may also be in this category.
# (e.g. if "amsmath" is loaded with option "intlimits", see
#  http://mirror.ctan.org/macros/latex/required/amsmath/amsldoc.pdf)
# movablelimits.extend(('fint', 'iiiint', 'iiint', 'iint', 'int', 'oiint',
#                       'oint', 'ointctrclockwise', 'sqint',
#                       'varointclockwise',))

# horizontal space -> <mspace>

spaces = {'qquad':         '2em',        # two \quad
          'quad':          '1em',        # 18 mu
          'thickspace':    '0.2778em',   # 5mu = 5/18em
          ';':             '0.2778em',   # 5mu thickspace
          ' ':             '0.25em',     # inter word space
          'medspace':      '0.2222em',   # 4mu = 2/9em
          ':':             '0.2222em',   # 4mu medspace
          'thinspace':     '0.1667em',   # 3mu = 1/6em
          ',':             '0.1667em',   # 3mu thinspace
          'negthinspace':  '-0.1667em',  # -3mu = -1/6em
          '!':             '-0.1667em',  # negthinspace
          'negmedspace':   '-0.2222em',  # -4mu = -2/9em
          'negthickspace': '-0.2778em',  # -5mu = -5/18em
          }

# accents: -> <mo stretchy="false"> in <mover>
accents = {
    # TeX:      spacing    combining
    'acute':    '´',     # '\u0301'
    'bar':      'ˉ',     # '\u0304'
    'breve':    '˘',     # '\u0306'
    'check':    'ˇ',     # '\u030C'
    'dot':      '˙',     # '\u0307'
    'ddot':     '¨',     # '\u0308'
    'dddot':    '˙˙˙',   # '\u20DB'  # or … ?
    'ddddot':   '˙˙˙˙',  # '\u20DC'  # or ¨¨ ?
    'grave':    '`',     # '\u0300'
    'hat':      'ˆ',     # '\u0302'
    'mathring': '˚',     # '\u030A'
    'tilde':    '~',     # '\u0303'  # tilde ~ or small tilde ˜?
    'vec':      '→',     # '\u20d7'  # → too heavy, use scriptlevel="+1"
}

# limits etc. -> <mo> in <mover> or <munder>
over = {
    # TeX:                  (char,     offset-correction/em)
    'overbrace':            ('\u23DE', -0.2),  # DejaVu Math -0.6
    'overleftarrow':        ('\u2190', -0.2),
    'overleftrightarrow':   ('\u2194', -0.2),
    'overline':             ('_',      -0.2),  # \u2012 does not stretch
    'overrightarrow':       ('\u2192', -0.2),
    'widehat':              ('^',      -0.5),
    'widetilde':            ('~',      -0.3),
}
under = {'underbrace':          ('\u23DF',  0.1),  # DejaVu Math -0.7
         'underleftarrow':      ('\u2190', -0.2),
         'underleftrightarrow': ('\u2194', -0.2),
         'underline':           ('_',      -0.8),
         'underrightarrow':     ('\u2192', -0.2),
         }

# Character translations
# ----------------------
# characters with preferred alternative in mathematical use
# cf. https://www.w3.org/TR/MathML3/chapter7.html#chars.anomalous
anomalous_chars = {'-': '\u2212',  # HYPHEN-MINUS -> MINUS SIGN
                   ':': '\u2236',  # COLON -> RATIO
                   '~': '\u00a0',  # NO-BREAK SPACE
                   }

# blackboard bold (Greek characters not working with "mathvariant" (Firefox 78)
mathbb = {'Γ': '\u213E',    # ℾ
          'Π': '\u213F',    # ℿ
          'Σ': '\u2140',    # ⅀
          'γ': '\u213D',    # ℽ
          'π': '\u213C',    # ℼ
          }

# Matrix environments
matrices = {
    # name:    fences
    'matrix':  ('', ''),
    'smallmatrix':  ('', ''),  # smaller, see begin_environment()!
    'pmatrix': ('(', ')'),
    'bmatrix': ('[', ']'),
    'Bmatrix': ('{', '}'),
    'vmatrix': ('|', '|'),
    'Vmatrix': ('\u2016', '\u2016'),  # ‖
    'aligned': ('', ''),
    'cases':   ('{', ''),
}

layout_styles = {
    'displaystyle':      {'displaystyle': True,  'scriptlevel': 0},
    'textstyle':         {'displaystyle': False, 'scriptlevel': 0},
    'scriptstyle':       {'displaystyle': False, 'scriptlevel': 1},
    'scriptscriptstyle': {'displaystyle': False, 'scriptlevel': 2},
    }
# See also https://www.w3.org/TR/MathML3/chapter3.html#presm.scriptlevel

fractions = {
    # name:   style_attrs, frac_attrs
    'frac':   ({}, {}),
    'cfrac':  ({'displaystyle': True,  'scriptlevel': 0,
                'CLASS': 'cfrac'}, {}),  # in LaTeX with padding
    'dfrac':  (layout_styles['displaystyle'], {}),
    'tfrac':  (layout_styles['textstyle'], {}),
    'binom':  ({}, {'linethickness': 0}),
    'dbinom': (layout_styles['displaystyle'], {'linethickness': 0}),
    'tbinom': (layout_styles['textstyle'], {'linethickness': 0}),
}

delimiter_sizes = ['', '1.2em', '1.623em', '2.047em', '2.470em']
bigdelimiters = {'left':  0,
                 'right': 0,
                 'bigl':  1,
                 'bigr':  1,
                 'Bigl':  2,
                 'Bigr':  2,
                 'biggl': 3,
                 'biggr': 3,
                 'Biggl': 4,
                 'Biggr': 4,
                 }


# MathML element classes
# ----------------------

class math:
    """Base class for MathML elements and root of MathML trees."""

    nchildren = None
    """Expected number of children or None"""
    # cf. https://www.w3.org/TR/MathML3/chapter3.html#id.3.1.3.2
    parent = None
    """Parent node in MathML DOM tree."""
    _level = 0  # indentation level (static class variable)
    xml_entities = {
        # for invalid and invisible characters
        ord('<'): '&lt;',
        ord('>'): '&gt;',
        ord('&'): '&amp;',
        0x2061:   '&ApplyFunction;',
    }
    global_attributes = (
        'class',  # space-separated list of element classes
        # 'data-*',  # custom data attributes (see HTML)
        # 'dir',  # directionality ('ltr', 'rtl')
        'displaystyle',  # True: normal, False: compact
        # 'id',  # unique identifier
        # 'mathbackground',  # color definition, deprecated
        # 'mathcolor',  # color definition, deprecated
        # 'mathsize',  # font-size, deprecated
        # 'nonce',  # cryptographic nonce ("number used once")
        'scriptlevel',  # math-depth for the element
        'style',  # CSS styling declarations
        # 'tabindex',  # indicate if the element takes input focus
        )
    """Global MathML attributes

    https://w3c.github.io/mathml-core/#global-attributes
    """

    def __init__(self, *children, **attributes):
        """Set up node with `children` and `attributes`.

        Attributes are downcased to allow using CLASS to set "class" value.
        >>> math(mn(3), CLASS='test')
        math(mn(3), class='test')
        >>> math(CLASS='test').toprettyxml()
        '<math class="test">\n</math>'

        """
        self.children = []
        self += children
        self.attributes = {}
        for key in attributes.keys():
            # Use .lower() to allow argument `CLASS` for attribute `class`
            # (Python keyword). MathML uses only lowercase attributes.
            self.attributes[key.lower()] = attributes[key]

    def __repr__(self):
        content = [repr(item) for item in self.children]
        if hasattr(self, 'data'):
            content.append(repr(self.data))
        if getattr(self, 'switch', None):
            content.append('switch=True')
        content += ["%s=%r"%(k, v) for k, v in self.attributes.items()
                    if v is not None]
        return self.__class__.__name__ + '(%s)' % ', '.join(content)

    def __len__(self):
        return len(self.children)

    # emulate dictionary-like access to attributes
    # see `docutils.nodes.Element` for dict/list interface
    def __getitem__(self, key):
        return self.attributes[key]

    def __setitem__(self, key, item):
        self.attributes[key] = item

    def get(self, *args, **kwargs):
        return self.attributes.get(*args, **kwargs)

    def full(self):
        """Return boolean indicating whether children may be appended."""
        return self.nchildren is not None and len(self) >= self.nchildren

    def append(self, child):
        """Append child and return self or first non-full parent.

        If self is full, go up the tree and return first non-full node or
        `None`.
        """
        if self.full():
            raise MathError(f'Node {self} already full!')
        self.children.append(child)
        child.parent = self
        if self.full():
            return self.close()
        return self

    def extend(self, children):
        for child in children:
            self.append(child)
        return self

    __iadd__ = extend  # alias for ``+=`` operator

    def close(self):
        """Close element and return first non-full parent or None."""
        parent = self.parent
        while parent is not None and parent.full():
            parent = parent.parent
        return parent

    def subnodes(self):
        """Return iterator over all subnodes, including nested ones."""
        for child in self.children:
            yield child
            yield from child.subnodes()

    # Conversion to (pretty) XML string
    def toprettyxml(self):
        """Return XML representation of self as string."""
        return ''.join(self._xml())

    def _xml(self, level=0):
        if self.nchildren is not None and len(self) < self.nchildren:
            raise MathError(f'Node {self.xml_starttag()} misses children.'
                            ' Incomplete source?')
        return [self.xml_starttag(),
                *self._xml_body(level),
                '</%s>' % self.__class__.__name__]

    def xml_starttag(self):
        attrs = (f'{k}="{self.a_str(v)}"'
                 for k, v in self.attributes.items() if v is not None)
        return '<%s>' % ' '.join((self.__class__.__name__, *attrs))

    @staticmethod
    def a_str(v):
        # Return string representation for attribute value `v`.
        return str(v).replace('True', 'true').replace('False', 'false')

    def _xml_body(self, level=0):
        xml = []
        for child in self.children:
            xml.extend(['\n', '  ' * (level+1)])
            xml.extend(child._xml(level+1))
        xml.extend(['\n', '  ' * level])
        return xml

    # auxiliary methods:

    def is_block(self):
        """Return true, if `self` or a parent has ``display='block'``."""
        try:
            return self['display'] == 'block'
        except KeyError:
            try:
                return self.parent.is_block()
            except AttributeError:
                return False

# >>> n2 = math(mn(2))
# >>> n2
# math(mn(2))
# >>> n2.toprettyxml()
# '<math>\n  <mn>2</mn>\n</math>'
# >>> len(n2)
# 1
# >>> n2 += [mo('!')]
# >>> n2
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


class mtable(math): pass

# >>> mt = mtable(displaystyle=True)
# >>> mt
# mtable(displaystyle=True)
# >>> math(mt).toprettyxml()
# '<math>\n  <mtable displaystyle="true">\n  </mtable>\n</math>'


class mrow(math):
    """Group sub-expressions as a horizontal row."""

    def transfer_attributes(self, other):
        # Update dictionary `other.attributes` with self.attributes.
        # String attributes (class, style) are appended to existing values,
        # other attributes (displaystyle, scriptlevel) replace them.
        for k, v in self.attributes.items():
            if k in ('class', 'style') and v:
                try:
                    other.attributes[k] += ' ' + v
                    continue
                except (KeyError, TypeError):
                    pass
            other.attributes[k] = v

    def close(self):
        """Close element and return first non-full parent or None.

        Remove <mrow>, if it is single child and the parent infers an mrow
        or if it has only one child element.
        """
        parent = self.parent
        if isinstance(parent, MathRowSchema) and parent.nchildren == 1:
            parent.children = self.children
            parent.nchildren = len(parent.children)
            for child in self.children:
                child.parent = parent
            self.transfer_attributes(parent)
            return parent.close()
        # replace `self` with single child
        if len(self) == 1:
            child = self.children[0]
            try:
                parent.children[parent.children.index(self)] = child
                child.parent = parent
            except (AttributeError, ValueError):
                return None
            self.transfer_attributes(child)
        return super().close()

# >>> row = mrow(displaystyle=False, CLASS='mathscr')
# >>> tree = math(msqrt(row, displaystyle=True))
# >>> row.close()  # remove mrow and transfer attributes to parent
# math(msqrt(displaystyle=False, class='mathscr'))
# >>> row = mrow(mi('i', CLASS='boldmath'), mathvariant='normal', CLASS='test')
# >>> tree = math(row)
# >>> row.close()
# math(mi('i', class='boldmath test', mathvariant='normal'))


# The elements <msqrt>, <mstyle>, <merror>, <mpadded>, <mphantom>, <menclose>,
# <mtd>, <mscarry>, and <math> treat their contents as a single inferred mrow
# formed from all their children.
class MathRowSchema(math):
    """Base class for elements treating content as a single inferred mrow."""
    # In MathML Core, this is called "anonymous mrow element".


class mtr(MathRowSchema):
    """MathML table/matrix row element."""


class mtd(MathRowSchema):
    """MathML table/matrix data cell element."""


class merror(MathRowSchema):
    pass


class menclose(MathRowSchema):
    nchildren = 1  # \boxed expects one argument or a group


class mphantom(MathRowSchema):
    nchildren = 1  # \phantom expects one argument or a group


class msqrt(MathRowSchema):
    nchildren = 1  # \sqrt expects one argument or a group


class mstyle(MathRowSchema):
    """Style Change. Deprecated in MathML Core.

    Use mrow instead.
    """


class MathToken(math):
    """Token Element: contains textual data instead of children.

    Base class for mi, mn, mo, and mtext.
    """
    nchildren = 0

    def __init__(self, data, **attributes):
        self.data = data
        super().__init__(**attributes)

    def _xml_body(self, level=0):
        return [str(self.data).translate(self.xml_entities)]


class mi(MathToken): pass
class mn(MathToken): pass
class mo(MathToken): pass
class mtext(MathToken): pass


# >>> mo('<')
# mo('<')
# >>> mo('<')._xml()
# ['<mo>', '&lt;', '</mo>']

class MathSchema(math):
    """Base class for schemata expecting 2 or more children.

    The special attribute `switch` indicates that the last two child
    elements are in reversed order and must be switched before XML-export.
    """

    nchildren = 2

    def __init__(self, *children, **kwargs):
        self.switch = kwargs.pop('switch', False)
        math.__init__(self, *children, **kwargs)

    def append(self, child):
        current_node = super().append(child)
        # normalize order if full
        if self.switch and self.full():
            self.children[-1], self.children[-2] = self.children[-2], self.children[-1]
            self.switch = False
        return current_node


class msub(MathSchema): pass
class msup(MathSchema): pass


class msubsup(MathSchema):
    nchildren = 3


# >>> msub(mi('x'), mo('-'))
# msub(mi('x'), mo('-'))
# >>> msubsup(mi('base'), mi('sub'), mi('super'))
# msubsup(mi('base'), mi('sub'), mi('super'))
# >>> msubsup(mi('base'), mi('super'), mi('sub'), switch=True)
# msubsup(mi('base'), mi('sub'), mi('super'))

class munder(msub): pass
class mover(msup): pass


# >>> munder(mi('lim'), mo('-'), accent=False)
# munder(mi('lim'), mo('-'), accent=False)
# >>> mu = munder(mo('-'), accent=False, switch=True)
# >>> mu
# munder(mo('-'), switch=True, accent=False)
# >>> mu.append(mi('lim'))
# >>> mu
# munder(mi('lim'), mo('-'), accent=False)
# >>> mu.append(mi('lim'))
# Traceback (most recent call last):
# docutils.utils.math.MathError: Node munder(mi('lim'), mo('-'), accent=False) already full!
# >>> munder(mo('-'), mi('lim'), accent=False, switch=True).toprettyxml()
# '<munder accent="false">\n  <mi>lim</mi>\n  <mo>-</mo>\n</munder>'

class munderover(msubsup): pass


class mroot(MathSchema):
    nchildren = 2


class mfrac(math):
    nchildren = 2


class mspace(math):
    nchildren = 0


# LaTeX to MathML translation
# ---------------------------

# auxiliary functions
# ~~~~~~~~~~~~~~~~~~~

def tex_cmdname(string):
    """Return leading TeX command name and remainder of `string`.

    >>> tex_cmdname('mymacro2') # up to first non-letter
    ('mymacro', '2')
    >>> tex_cmdname('name 2') # strip trailing whitespace
    ('name', '2')
    >>> tex_cmdname('_2') # single non-letter character
    ('_', '2')

    """
    m = re.match(r'([a-zA-Z]+) *(.*)', string)
    if m is None:
        m = re.match(r'(.?)(.*)', string)
    return m.group(1), m.group(2)


# Test:
#
# >>> tex_cmdname('name_2') # first non-letter terminates
# ('name', '_2')
# >>> tex_cmdname(' next') # leading whitespace is returned
# (' ', 'next')
# >>> tex_cmdname('1 2') # whitespace after non-letter is kept
# ('1', ' 2')
# >>> tex_cmdname('') # empty string
# ('', '')


def tex_number(string):
    """Return leading number literal and remainder of `string`.

    >>> tex_number('123.4')
    ('123.4', '')

    """
    m = re.match(r'([0-9.,]*[0-9]+)(.*)', string)
    if m is None:
        return '', string
    return m.group(1), m.group(2)


# Test:
#
# >>> tex_number(' 23.4b') # leading whitespace -> no number
# ('', ' 23.4b')
# >>> tex_number('23,400/2') # comma separator included
# ('23,400', '/2')
# >>> tex_number('23. 4/2') # trailing separator not included
# ('23', '. 4/2')
# >>> tex_number('4, 2') # trailing separator not included
# ('4', ', 2')
# >>> tex_number('1 000.4')
# ('1', ' 000.4')


def tex_token(string):
    """Return first simple TeX token and remainder of `string`.

    >>> tex_token('\\command{without argument}')
    ('\\command', '{without argument}')
    >>> tex_token('or first character')
    ('o', 'r first character')

    """
    m = re.match(r"""((?P<cmd>\\[a-zA-Z]+)\s* # TeX command, skip whitespace
                      |(?P<chcmd>\\.)          # one-character TeX command
                      |(?P<ch>.?))            # first character (or empty)
                     (?P<remainder>.*$)    # remaining part of string
                 """, string, re.VERBOSE)
    cmd, chcmd, ch, remainder = m.group('cmd', 'chcmd', 'ch', 'remainder')
    return cmd or chcmd or ch, remainder

# Test:
#
# >>> tex_token('{opening bracket of group}')
# ('{', 'opening bracket of group}')
# >>> tex_token('\\skip whitespace after macro name')
# ('\\skip', 'whitespace after macro name')
# >>> tex_token('. but not after single char')
# ('.', ' but not after single char')
# >>> tex_token('') # empty string.
# ('', '')
# >>> tex_token('\{escaped bracket')
# ('\\{', 'escaped bracket')


def tex_group(string):
    """Return first TeX group or token and remainder of `string`.

    >>> tex_group('{first group} returned without brackets')
    ('first group', ' returned without brackets')

    """
    split_index = 0
    nest_level = 0   # level of {{nested} groups}
    escape = False   # the next character is escaped (\)

    if not string.startswith('{'):
        # special case: there is no group, return first token and remainder
        return string[:1], string[1:]
    for c in string:
        split_index += 1
        if escape:
            escape = False
        elif c == '\\':
            escape = True
        elif c == '{':
            nest_level += 1
        elif c == '}':
            nest_level -= 1
        if nest_level == 0:
            break
    else:
        raise MathError('Group without closing bracket!')
    return string[1:split_index-1], string[split_index:]


# >>> tex_group('{} empty group')
# ('', ' empty group')
# >>> tex_group('{group with {nested} group} ')
# ('group with {nested} group', ' ')
# >>> tex_group('{group with {nested group}} at the end')
# ('group with {nested group}', ' at the end')
# >>> tex_group('{{group} {with {{complex }nesting}} constructs}')
# ('{group} {with {{complex }nesting}} constructs', '')
# >>> tex_group('{group with \\{escaped\\} brackets}')
# ('group with \\{escaped\\} brackets', '')
# >>> tex_group('{group followed by closing bracket}} from outer group')
# ('group followed by closing bracket', '} from outer group')
# >>> tex_group('No group? Return first character.')
# ('N', 'o group? Return first character.')
# >>> tex_group(' {also whitespace}')
# (' ', '{also whitespace}')


def tex_token_or_group(string):
    """Return first TeX group or token and remainder of `string`.

    >>> tex_token_or_group('\\command{without argument}')
    ('\\command', '{without argument}')
    >>> tex_token_or_group('first character')
    ('f', 'irst character')
    >>> tex_token_or_group(' also whitespace')
    (' ', 'also whitespace')
    >>> tex_token_or_group('{first group} keep rest')
    ('first group', ' keep rest')

    """
    arg, remainder = tex_token(string)
    if arg == '{':
        arg, remainder = tex_group(string.lstrip())
    return arg, remainder

# >>> tex_token_or_group('\{no group but left bracket')
# ('\\{', 'no group but left bracket')


def tex_optarg(string):
    """Return optional argument and remainder.

    >>> tex_optarg('[optional argument] returned without brackets')
    ('optional argument', ' returned without brackets')
    >>> tex_optarg('{empty string, if there is no optional arg}')
    ('', '{empty string, if there is no optional arg}')

    """
    m = re.match(r"""\s*                            # leading whitespace
                 \[(?P<optarg>(\\]|[^\[\]]|\\])*)\] # [group] without nested groups
                 (?P<remainder>.*$)
                 """, string, re.VERBOSE)
    if m is None and not string.startswith('['):
        return '', string
    try:
        return m.group('optarg'), m.group('remainder')
    except AttributeError:
        raise MathError(f'Could not extract optional argument from "{string}"!')

# Test:
# >>> tex_optarg(' [optional argument] after whitespace')
# ('optional argument', ' after whitespace')
# >>> tex_optarg('[missing right bracket')
# Traceback (most recent call last):
#     ...
# docutils.utils.math.MathError: Could not extract optional argument from "[missing right bracket"!
# >>> tex_optarg('[group with [nested group]]')
# Traceback (most recent call last):
#     ...
# docutils.utils.math.MathError: Could not extract optional argument from "[group with [nested group]]"!


def parse_latex_math(node, string):
    """Append MathML conversion of `string` to `node` and return it.

    >>> parse_latex_math(math(), r'\alpha')
    math(mi('α'))
    >>> parse_latex_math(mrow(), r'x_{n}')
    mrow(msub(mi('x'), mi('n')))

    """
    # Normalize white-space:
    string = ' '.join(string.split())
    tree = node

    while len(string) > 0:
        # Take off first character:
        c, string = string[0], string[1:]

        if c == ' ':
            continue  # whitespace is ignored in LaTeX math mode
        if c == '\\':  # start of a LaTeX macro
            cmdname, string = tex_cmdname(string)
            node, string = handle_cmd(cmdname, node, string)
        elif c in "_^":
            node = handle_script_or_limit(node, c)
        elif c == '{':
            new_node = mrow()
            node.append(new_node)
            node = new_node
        elif c == '}':
            node = node.close()
        elif c == '&':
            new_node = mtd()
            node.close().append(new_node)
            node = new_node
        elif c.isalpha():
            node = node.append(mi(c))
        elif c.isdigit():
            number, string = tex_number(string)
            node = node.append(mn(c+number))
        elif c in anomalous_chars:
            # characters with a special meaning in LaTeX math mode
            # fix spacing before "unary" minus.
            attributes = {}
            if c == '-' and node.children:
                previous_node = node.children[-1]
                if (getattr(previous_node, 'data', '-') in '([='
                    or previous_node.get('class') == 'mathopen'):
                    attributes['form'] = 'prefix'
            node = node.append(mo(anomalous_chars[c], **attributes))
        elif c in "/()[]|":
            node = node.append(mo(c, stretchy=False))
        elif c in "+*=<>,.!?`';@":
            node = node.append(mo(c))
        else:
            raise MathError(f'Unsupported character: "{c}"!')
            # TODO: append as <mi>?
    return tree

# Test:

# >>> print(parse_latex_math(math(), ''))
# math()
# >>> parse_latex_math(math(), ' \\sqrt{ \\alpha}')
# math(msqrt(mi('α')))
# >>> parse_latex_math(math(), '23.4x')
# math(mn('23.4'), mi('x'))
# >>> parse_latex_math(math(), '\\sqrt 2 \\ne 3')
# math(msqrt(mn('2')), mo('≠'), mn('3'))
# >>> parse_latex_math(math(), '\\sqrt{2 + 3} < 3')
# math(msqrt(mn('2'), mo('+'), mn('3')), mo('<'), mn('3'))
# >>> parse_latex_math(math(), '\\sqrt[3]{2 + 3}')
# math(mroot(mrow(mn('2'), mo('+'), mn('3')), mn('3')))
# >>> parse_latex_math(math(), '\max_x') # function takes limits
# math(munder(mo('max', movablelimits=True), mi('x')))
# >>> parse_latex_math(math(), 'x^j_i') # ensure correct order: base, sub, sup
# math(msubsup(mi('x'), mi('i'), mi('j')))
# >>> parse_latex_math(math(), '\int^j_i') # ensure correct order
# math(msubsup(mo('∫'), mi('i'), mi('j')))
# >>> parse_latex_math(math(), 'x_{\\alpha}')
# math(msub(mi('x'), mi('α')))
# >>> parse_latex_math(math(), 'x_\\text{in}')
# math(msub(mi('x'), mtext('in')))
# >>> parse_latex_math(math(), '2⌘')
# Traceback (most recent call last):
# docutils.utils.math.MathError: Unsupported character: "⌘"!


def handle_cmd(name, node, string):  # noqa: C901 TODO make this less complex
    """Process LaTeX command `name` followed by `string`.

    Append result to `node`.
    If needed, parse `string` for command argument.
    Return new current node and remainder of `string`:

    >>> handle_cmd('hbar', math(), r' \frac')
    (math(mi('ℏ')), ' \\frac')
    >>> handle_cmd('hspace', math(), r'{1ex} (x)')
    (math(mspace(width='1ex')), ' (x)')

    """

    # Token elements
    # ==============

    # identifier  ->  <mi>

    if name in letters:
        new_node = mi(letters[name])
        if name in greek_capitals:
            # upright in "TeX style" but MathML sets them italic ("ISO style").
            # CSS styling does not change the font style in Firefox 78.
            # Use 'mathvariant="normal"'?
            new_node['class'] = 'capital-greek'
        node = node.append(new_node)
        return node, string

    if name in ordinary:
        # <mi mathvariant="normal"> well supported by Chromium but
        # Firefox 115.5.0 puts additional space around the symbol, e.g.
        # <mi mathvariant="normal">∂</mi><mi>t</mi> looks like ∂ t, not ∂t
        # return node.append(mi(ordinary[name], mathvariant='normal')), string
        return node.append(mi(ordinary[name])), string

    if name in functions:
        # use <mi> followed by invisible function applicator character
        # (see https://www.w3.org/TR/MathML3/chapter3.html#presm.mi)
        if name == 'operatorname':
            # custom function name, e.g. ``\operatorname{abs}(x)``
            # TODO: \operatorname* -> with limits
            arg, string = tex_token_or_group(string)
            new_node = mi(arg, mathvariant='normal')
        else:
            new_node = mi(functions[name])
        # embellished function names:
        if name == 'varliminf':    # \underline\lim
            new_node = munder(new_node, mo('_'))
        elif name == 'varlimsup':  # \overline\lim
            new_node = mover(new_node, mo('¯'), accent=False)
        elif name == 'varprojlim':  # \underleftarrow\lim
            new_node = munder(new_node, mo('\u2190'))
        elif name == 'varinjlim':  # \underrightarrow\lim
            new_node = munder(new_node, mo('\u2192'))

        node = node.append(new_node)
        # add ApplyFunction when appropriate (not \sin^2(x), say)
        # cf. https://www.w3.org/TR/MathML3/chapter3.html#presm.mi
        if string and string[0] not in ('^', '_'):
            node = node.append(mo('\u2061'))  # &ApplyFunction;
        return node, string

    if name in modulo_functions:
        (binary, named, parentheses, padding) = modulo_functions[name]
        if binary:
            node = node.append(mo('mod', lspace=padding, rspace=padding))
            return node, string
        # left padding
        if node.is_block():
            padding = '1em'
        node = node.append(mspace(width=padding))
        if parentheses:
            node = node.append(mo('(', stretchy=False))
        if named:
            node = node.append(mi('mod'))
            node = node.append(mspace(width='0.333em'))
        arg, string = tex_token_or_group(string)
        node = parse_latex_math(node, arg)
        if parentheses:
            node = node.append(mo(')', stretchy=False))
        return node, string

    # font changes or mathematical alphanumeric characters

    if name in ('boldsymbol', 'pmb'):  # \pmb is "poor mans bold"
        new_node = mrow(CLASS='boldsymbol')
        node.append(new_node)
        return new_node, string

    if name in math_alphabets:
        return handle_math_alphabet(name, node, string)

    # operator, fence, or separator  ->  <mo>

    if name == 'colon':  # trailing punctuation, not binary relation
        node = node.append(mo(':', form='postfix', lspace='0', rspace='0.28em'))
        return node, string

    if name == 'idotsint':  # AMS shortcut for ∫︀···∫︀
        node = parse_latex_math(node, r'\int\dotsi\int')
        return node, string

    if name in thick_operators:
        node = node.append(mo(thick_operators[name], style='font-weight: bold'))
        return node, string

    if name in small_operators:
        node = node.append(mo(small_operators[name], mathsize='75%'))
        return node, string

    if name in operators:
        attributes = {}
        if name in movablelimits and string and string[0] in ' _^':
            attributes['movablelimits'] = True
        elif name in ('lvert', 'lVert'):
            attributes['class'] = 'mathopen'
        node = node.append(mo(operators[name], **attributes))
        return node, string

    if name in bigdelimiters:
        delimiter_attributes = {}
        size = delimiter_sizes[bigdelimiters[name]]
        delimiter, string = tex_token_or_group(string)
        if delimiter not in '()[]/|.':
            try:
                delimiter = stretchables[delimiter.lstrip('\\')]
            except KeyError:
                raise MathError(f'Unsupported "\\{name}" delimiter '
                                f'"{delimiter}"!')
        if size:
            delimiter_attributes['maxsize'] = size
            delimiter_attributes['minsize'] = size
            delimiter_attributes['symmetric'] = True
        if name == 'left' or name.endswith('l'):
            row = mrow()
            node.append(row)
            node = row
        if delimiter != '.':  # '.' stands for "empty delimiter"
            node.append(mo(delimiter, **delimiter_attributes))
        if name == 'right' or name.endswith('r'):
            node = node.close()
        return node, string

    if name == 'not':
        # negation: LaTeX just overlays next symbol with "/".
        arg, string = tex_token(string)
        if arg == '{':
            return node, '{\\not ' + string
        if arg.startswith('\\'):  # LaTeX macro
            try:
                arg = operators[arg[1:]]
            except KeyError:
                raise MathError(rf'"\not" cannot negate: "{arg}"!')
        arg = unicodedata.normalize('NFC', arg+'\u0338')
        node = node.append(mo(arg))
        return node, string

    # arbitrary text (usually comments)  ->  <mtext>
    if name in ('text', 'mbox', 'textrm'):
        arg, string = tex_token_or_group(string)
        parts = arg.split('$')  # extract inline math
        for i, part in enumerate(parts):
            if i % 2 == 0:  # i is even
                part = re.sub('(^ | $)', '\u00a0', part)
                node = node.append(mtext(part))
            else:
                parse_latex_math(node, part)
        return node, string

    # horizontal space -> <mspace>
    if name in spaces:
        node = node.append(mspace(width='%s'%spaces[name]))
        return node, string

    if name in ('hspace', 'mspace'):
        arg, string = tex_group(string)
        if arg.endswith('mu'):
            # unit "mu" (1mu=1/18em) not supported by MathML
            arg = '%sem' % (float(arg[:-2])/18)
        node = node.append(mspace(width='%s'%arg))
        return node, string

    if name == 'phantom':
        new_node = mphantom()
        node.append(new_node)
        return new_node, string

    if name == 'boxed':
        new_node = menclose(notation='box')
        node.append(new_node)
        return new_node, string

    # Complex elements (Layout schemata)
    # ==================================

    if name == 'sqrt':
        radix, string = tex_optarg(string)
        if radix:
            indexnode = mrow()
            new_node = mroot(indexnode, switch=True)
            parse_latex_math(indexnode, radix)
            indexnode.close()
        else:
            new_node = msqrt()
        node.append(new_node)
        return new_node, string

    if name in fractions:
        (style_atts, frac_atts) = fractions[name]
        if name == 'cfrac':
            optarg, string = tex_optarg(string)
            optargs = {'l': 'left', 'r': 'right'}
            if optarg in optargs:
                frac_atts = frac_atts.copy()
                frac_atts['numalign'] = optargs[optarg]  # "numalign" is deprecated
                frac_atts['class'] = 'numalign-' + optargs[optarg]
        new_node = frac = mfrac(**frac_atts)
        if name.endswith('binom'):
            new_node = mrow(mo('('), new_node, mo(')'), CLASS='binom')
            new_node.nchildren = 3
        if style_atts:
            new_node = mstyle(new_node, **style_atts)
        node.append(new_node)
        return frac, string

    if name == '\\':  # end of a row
        entry = mtd()
        new_node = mtr(entry)
        node.close().close().append(new_node)
        return entry, string

    if name in accents:
        accent_node = mo(accents[name], stretchy=False)
        # mi() would be simpler, but semantically wrong
        # --- https://w3c.github.io/mathml-core/#operator-fence-separator-or-accent-mo
        if name == 'vec':
            accent_node['scriptlevel'] = '+1'  # scale down arrow
        new_node = mover(accent_node, accent=True, switch=True)
        node.append(new_node)
        return new_node, string

    if name in over:
        # set "accent" to False (otherwise dots on i and j are dropped)
        # but to True on accent node get "textstyle" (full size) symbols on top
        new_node = mover(mo(over[name][0], accent=True),
                         switch=True, accent=False)
        node.append(new_node)
        return new_node, string

    if name == 'overset':
        new_node = mover(switch=True)
        node.append(new_node)
        return new_node, string

    if name in under:
        new_node = munder(mo(under[name][0]), switch=True)
        node.append(new_node)
        return new_node, string

    if name == 'underset':
        new_node = munder(switch=True)
        node.append(new_node)
        return new_node, string

    if name in ('xleftarrow', 'xrightarrow'):
        subscript, string = tex_optarg(string)
        base = mo(operators['long'+name[1:]])
        if subscript:
            new_node = munderover(base)
            sub_node = parse_latex_math(mrow(), subscript)
            if len(sub_node) == 1:
                sub_node = sub_node.children[0]
            new_node.append(sub_node)
        else:
            new_node = mover(base)
        node.append(new_node)
        return new_node, string

    if name in layout_styles:  # 'displaystyle', 'textstyle', ...
        new_node = mstyle(**layout_styles[name])
        new_node.nchildren = None
        if isinstance(node, mrow) and len(node) == 0:
            # replace node with new_node
            node.parent.children[node.parent.children.index(node)] = new_node
            new_node.parent = node.parent
        elif node.__class__.__name__ == 'math':
            node.append(new_node)
        else:
            raise MathError(rf'Declaration "\{name}" must be first command '
                            'in a group!')
        return new_node, string

    if name.endswith('limits'):
        arg, remainder = tex_token(string)
        if arg in '_^':  # else ignore
            string = remainder
            node = handle_script_or_limit(node, arg, limits=name)
        return node, string

    # Environments

    if name == 'begin':
        return begin_environment(node, string)

    if name == 'end':
        return end_environment(node, string)

    raise MathError(rf'Unknown LaTeX command "\{name}".')

# >>> handle_cmd('left', math(), '[a\\right]')
# (mrow(mo('[')), 'a\\right]')
# >>> handle_cmd('left', math(), '. a)') # empty \left
# (mrow(), ' a)')
# >>> handle_cmd('left', math(), '\\uparrow a)') # cmd
# (mrow(mo('↑')), 'a)')
# >>> handle_cmd('not', math(), '\\equiv \\alpha)') # cmd
# (math(mo('≢')), '\\alpha)')
# >>> handle_cmd('text', math(), '{ for } i>0') # group
# (math(mtext('\xa0for\xa0')), ' i>0')
# >>> handle_cmd('text', math(), '{B}T') # group
# (math(mtext('B')), 'T')
# >>> handle_cmd('text', math(), '{number of apples}}') # group
# (math(mtext('number of apples')), '}')
# >>> handle_cmd('text', math(), 'i \\sin(x)') # single char
# (math(mtext('i')), ' \\sin(x)')
# >>> handle_cmd('sin', math(), '(\\alpha)')
# (math(mi('sin'), mo('\u2061')), '(\\alpha)')
# >>> handle_cmd('sin', math(), ' \\alpha')
# (math(mi('sin'), mo('\u2061')), ' \\alpha')
# >>> handle_cmd('operatorname', math(), '{abs}(x)')
# (math(mi('abs', mathvariant='normal'), mo('\u2061')), '(x)')
# >>> handle_cmd('overline', math(), '{981}')
# (mover(mo('_', accent=True), switch=True, accent=False), '{981}')
# >>> handle_cmd('bar', math(), '{x}')
# (mover(mo('ˉ', stretchy=False), switch=True, accent=True), '{x}')
# >>> handle_cmd('xleftarrow', math(), r'[\alpha]{10}')
# (munderover(mo('⟵'), mi('α')), '{10}')
# >>> handle_cmd('xleftarrow', math(), r'[\alpha=5]{10}')
# (munderover(mo('⟵'), mrow(mi('α'), mo('='), mn('5'))), '{10}')
# >>> handle_cmd('left', math(), '< a)')
# Traceback (most recent call last):
# docutils.utils.math.MathError: Unsupported "\left" delimiter "<"!
# >>> handle_cmd('not', math(), '{< b} c') #  LaTeX ignores the braces, too.
# (math(), '{\\not < b} c')


def handle_math_alphabet(name, node, string):
    attributes = {}
    if name == 'mathscr':
        attributes['class'] = 'mathscr'
    arg, string = tex_token_or_group(string)
    # Shortcut for text arg like \mathrm{out}
    # with more than one letter, <mi> defaults to "normal" font
    if name == 'mathrm' and arg.isalpha() and len(arg) > 1:
        node = node.append(mi(arg))
        return node, string
    # Parse as <style> node children
    container = mrow(**attributes)
    node.append(container)
    parse_latex_math(container, arg)
    ma2ch = getattr(mathalphabet2unichar,
                    name.replace('mathscr', 'mathcal'), {})
    for subnode in container.subnodes():
        if isinstance(subnode, (mi, mn)):
            subnode.data = ma2ch.get(subnode.data, subnode.data)
        if isinstance(subnode, mi) and name == 'mathrm' and subnode.data.isalpha():
            subnode.attributes['mathvariant'] = 'normal'
    return container.close(), string

# >>> handle_math_alphabet('mathrm', math(), '\\alpha')
# (math(mi('α', mathvariant='normal')), '')
# >>> handle_math_alphabet('mathbb', math(), '{R} = 3')
# (math(mi('ℝ')), ' = 3')
# >>> handle_math_alphabet('mathcal', math(), '{F = 3}')
# (math(mrow(mi('ℱ'), mo('='), mn('3'))), '')
# >>> handle_math_alphabet('mathrm', math(), '{out} = 3')  # use shortcut
# (math(mi('out')), ' = 3')
# >>> handle_math_alphabet('mathrm', math(), '{V = 3}')  # force normal
# (math(mrow(mi('V', mathvariant='normal'), mo('='), mn('3'))), '')


def handle_script_or_limit(node, c, limits=''):
    """Append script or limit element to `node`."""
    child = node.children.pop()
    if limits == 'limits':
        child['movablelimits'] = False
    elif (limits == 'movablelimits'
          or getattr(child, 'data', '') in movablelimits):
        child['movablelimits'] = True

    if c == '_':
        if isinstance(child, mover):
            new_node = munderover(*child.children, switch=True)
        elif isinstance(child, msup):
            new_node = msubsup(*child.children, switch=True)
        elif (limits in ('limits', 'movablelimits')
              or limits == '' and child.get('movablelimits', None)):
            new_node = munder(child)
        else:
            new_node = msub(child)
    elif c == '^':
        if isinstance(child, munder):
            new_node = munderover(*child.children)
        elif isinstance(child, msub):
            new_node = msubsup(*child.children)
        elif (limits in ('limits', 'movablelimits')
              or limits == '' and child.get('movablelimits', None)):
            new_node = mover(child)
        else:
            new_node = msup(child)
    node.append(new_node)
    return new_node


def begin_environment(node, string):
    name, string = tex_group(string)
    if name in matrices:
        left_delimiter = matrices[name][0]
        attributes = {}
        if left_delimiter:
            wrapper = mrow(mo(left_delimiter))
            if name == 'cases':
                wrapper = mrow(mo(left_delimiter, rspace='0.17em'))
                attributes['columnalign'] = 'left'
                attributes['class'] = 'cases'
            node.append(wrapper)
            node = wrapper
        elif name == 'smallmatrix':
            attributes['rowspacing'] = '0.02em'
            attributes['columnspacing'] = '0.333em'
            wrapper = mstyle(scriptlevel='1')
            node.append(wrapper)
            node = wrapper
        elif name == 'aligned':
            attributes['class'] = 'ams-align'
        # TODO: array, aligned & alignedat take an optional [t], [b], or [c].
        entry = mtd()
        node.append(mtable(mtr(entry), **attributes))
        node = entry
    else:
        raise MathError(f'Environment "{name}" not supported!')
    return node, string


def end_environment(node, string):
    name, string = tex_group(string)
    if name in matrices:
        node = node.close().close().close()  # close: mtd, mdr, mtable
        right_delimiter = matrices[name][1]
        if right_delimiter:
            node = node.append(mo(right_delimiter))
            node = node.close()
        elif name == 'cases':
            node = node.close()
    else:
        raise MathError(f'Environment "{name}" not supported!')
    return node, string


# Return the number of "equation_columns" in `code_lines`. cf. "alignat"
# in http://mirror.ctan.org/macros/latex/required/amsmath/amsldoc.pdf
def tex_equation_columns(rows):
    tabs = max(row.count('&') - row.count(r'\&') for row in rows)
    if tabs == 0:
        return 0
    return int(tabs/2 + 1)

# >>> tex_equation_columns(['a = b'])
# 0
# >>> tex_equation_columns(['a &= b'])
# 1
# >>> tex_equation_columns(['a &= b & a \in S'])
# 2
# >>> tex_equation_columns(['a &= b & c &= d'])
# 2


# Return dictionary with attributes to style an <mtable> as align environment:
# Not used with HTML. Replaced by CSS rule for "mtable.ams-align" in
# "minimal.css" as "columnalign" is disregarded by Chromium and webkit.
def align_attributes(rows):
    atts = {'class': 'ams-align',
            'displaystyle': True}
    # get maximal number of non-escaped "next column" markup characters:
    tabs = max(row.count('&') - row.count(r'\&') for row in rows)
    if tabs:
        aligns = ['right', 'left'] * tabs
        spacing = ['0', '2em'] * tabs
        atts['columnalign'] = ' '.join(aligns[:tabs+1])
        atts['columnspacing'] = ' '.join(spacing[:tabs])
    return atts

# >>> align_attributes(['a = b'])
# {'class': 'ams-align', 'displaystyle': True}
# >>> align_attributes(['a &= b'])
# {'class': 'ams-align', 'displaystyle': True, 'columnalign': 'right left', 'columnspacing': '0'}
# >>> align_attributes(['a &= b & a \in S'])
# {'class': 'ams-align', 'displaystyle': True, 'columnalign': 'right left right', 'columnspacing': '0 2em'}
# >>> align_attributes(['a &= b & c &= d'])
# {'class': 'ams-align', 'displaystyle': True, 'columnalign': 'right left right left', 'columnspacing': '0 2em 0'}
# >>> align_attributes([r'a &= b & c &= d \& e'])
# {'class': 'ams-align', 'displaystyle': True, 'columnalign': 'right left right left', 'columnspacing': '0 2em 0'}
# >>> align_attributes([r'a &= b & c &= d & e'])
# {'class': 'ams-align', 'displaystyle': True, 'columnalign': 'right left right left right', 'columnspacing': '0 2em 0 2em'}


def tex2mathml(tex_math, as_block=False):
    """Return string with MathML code corresponding to `tex_math`.

    Set `as_block` to ``True`` for displayed formulas.
    """
    # Set up tree
    math_tree = math(xmlns='http://www.w3.org/1998/Math/MathML')
    node = math_tree
    if as_block:
        math_tree['display'] = 'block'
        rows = toplevel_code(tex_math).split(r'\\')
        if len(rows) > 1:
            # emulate "align*" environment with a math table
            node = mtd()
            math_tree.append(mtable(mtr(node), CLASS='ams-align',
                                    displaystyle=True))
    parse_latex_math(node, tex_math)
    return math_tree.toprettyxml()

# >>> print(tex2mathml('3'))
# <math xmlns="http://www.w3.org/1998/Math/MathML">
#   <mn>3</mn>
# </math>
# >>> print(tex2mathml('3', as_block=True))
# <math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
#   <mn>3</mn>
# </math>
# >>> print(tex2mathml(r'a & b \\ c & d', as_block=True))
# <math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
#   <mtable class="ams-align" displaystyle="true">
#     <mtr>
#       <mtd>
#         <mi>a</mi>
#       </mtd>
#       <mtd>
#         <mi>b</mi>
#       </mtd>
#     </mtr>
#     <mtr>
#       <mtd>
#         <mi>c</mi>
#       </mtd>
#       <mtd>
#         <mi>d</mi>
#       </mtd>
#     </mtr>
#   </mtable>
# </math>
# >>> print(tex2mathml(r'a \\ b', as_block=True))
# <math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
#   <mtable class="ams-align" displaystyle="true">
#     <mtr>
#       <mtd>
#         <mi>a</mi>
#       </mtd>
#     </mtr>
#     <mtr>
#       <mtd>
#         <mi>b</mi>
#       </mtd>
#     </mtr>
#   </mtable>
# </math>


# TODO: look up more symbols from tr25, e.g.
#
#
# Table 2.8 Using Vertical Line or Solidus Overlay
#   some of the negated forms of mathematical relations that can only be
#   encoded by using either U+0338 COMBINING LONG SOLIDUS OVERLAY or U+20D2
#   COMBINING LONG VERTICAL LINE OVERLAY . (For issues with using 0338 in
#   MathML, see Section 3.2.7, Combining Marks.
#
# Table 2.9 Variants of Mathematical Symbols using VS1?
#
# Sequence      Description
# 0030 + VS1    DIGIT ZERO - short diagonal stroke form
# 2205 + VS1    EMPTY SET - zero with long diagonal stroke overlay form
# 2229 + VS1    INTERSECTION - with serifs
# 222A + VS1    UNION - with serifs
# 2268 + VS1    LESS-THAN BUT NOT EQUAL TO - with vertical stroke
# 2269 + VS1    GREATER-THAN BUT NOT EQUAL TO - with vertical stroke
# 2272 + VS1    LESS-THAN OR EQUIVALENT TO - following the slant of the lower leg
# 2273 + VS1    GREATER-THAN OR EQUIVALENT TO - following the slant of the lower leg
# 228A + VS1    SUBSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
# 228B + VS1    SUPERSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
# 2293 + VS1    SQUARE CAP - with serifs
# 2294 + VS1    SQUARE CUP - with serifs
# 2295 + VS1    CIRCLED PLUS - with white rim
# 2297 + VS1    CIRCLED TIMES - with white rim
# 229C + VS1    CIRCLED EQUALS - equal sign inside and touching the circle
# 22DA + VS1    LESS-THAN slanted EQUAL TO OR GREATER-THAN
# 22DB + VS1    GREATER-THAN slanted EQUAL TO OR LESS-THAN
# 2A3C + VS1    INTERIOR PRODUCT - tall variant with narrow foot
# 2A3D + VS1    RIGHTHAND INTERIOR PRODUCT - tall variant with narrow foot
# 2A9D + VS1    SIMILAR OR LESS-THAN - following the slant of the upper leg
# 2A9E + VS1    SIMILAR OR GREATER-THAN - following the slant of the upper leg
# 2AAC + VS1    SMALLER THAN OR slanted EQUAL
# 2AAD + VS1    LARGER THAN OR slanted EQUAL
# 2ACB + VS1    SUBSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
# 2ACC + VS1    SUPERSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
