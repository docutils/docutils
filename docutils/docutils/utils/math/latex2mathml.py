#!/usr/bin/env python
# -*- coding: utf-8 -*-

# :Id: $Id$
# :Copyright: © 2005 Jens Jørgen Mortensen [1]_
#             © 2010, 2021 Günter Milde.
#
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause


"""Convert LaTex maths code into presentational MathML."""

# .. [1] the original `rst2mathml.py` in `sandbox/jensj/latex_math`
#
# Usage:
#
# >>> from latex2mathml import *

import collections
import re
import sys
if sys.version_info >= (3, 0):
    unicode = str  # noqa

from docutils.utils.math import tex2unichar


# Character data
# --------------

# Named XML entities for invalid and invisible characters
xml_entities = {ord('<'): u'&lt;',
                ord('>'): u'&gt;',
                ord('&'): u'&amp;',
                0x2061:   u'&ApplyFunction;',
               }

# LaTeX math macro to Unicode mappings.
# Character categories.

# identifiers -> <mi>

letters = tex2unichar.mathalpha

# special case: Capital Greek letters: (upright in TeX style)
greek_capitals = {
    'Phi':u'\u03a6', 'Xi':u'\u039e', 'Sigma':u'\u03a3',
    'Psi':u'\u03a8', 'Delta':u'\u0394', 'Theta':u'\u0398',
    'Upsilon':u'\u03d2', 'Pi':u'\u03a0', 'Omega':u'\u03a9',
    'Gamma':u'\u0393', 'Lambda':u'\u039b'}

# functions -> <mi>
functions = {# functions with a space in the name
             'liminf': u'lim\u202finf',
             'limsup': u'lim\u202fsup',
             'injlim': u'inj\u202flim',
             'projlim': u'proj\u202flim',
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


# math font selection -> <mi mathvariant=...> or <mstyle mathvariant=...>
math_alphabets = {# 'cmdname': 'mathvariant value'  # package
                  'boldsymbol': 'bold',
                  'mathbb': 'double-struck',        # amssymb
                  'mathbf': 'bold',
                  'mathcal': 'script',
                  'mathfrak': 'fraktur',            # amssymb
                  'mathit': 'italic',
                  'mathrm': 'normal',
                  'mathscr': 'script',              # mathrsfs
                  'mathsf': 'sans-serif',
                  'mathtt': 'monospace',
                  'mathbfit': 'bold-italic',        # isomath
                  'mathsfit': 'sans-serif-italic',  # isomath
                  'mathsfbfit': 'sans-serif-bold-italic',  # isomath
                  # unsupported: bold-fraktur
                  #              bold-script
                  #              bold-sans-serif
                 }

# operator, fence, or separator -> <mo>


math_fences = {# mathfence aliases with adapted spacing
               'lvert':      u'|',      # left  |
               'lVert':      u'\u2016', # left  ‖
               'rvert':      u'|',      # right |
               'rVert':      u'\u2016', # right ‖
               'Arrowvert':  u'\u2016', # ‖
              }

operators = tex2unichar.mathbin         # Binary symbols
operators.update(tex2unichar.mathrel)   # Relation symbols, arrow symbols
operators.update(tex2unichar.mathord)   # Miscellaneous symbols
operators.update(tex2unichar.mathop)    # Variable-sized symbols
operators.update(tex2unichar.mathopen)  # Braces
operators.update(tex2unichar.mathclose) # Braces
operators.update(tex2unichar.mathfence)
operators.update(math_fences)
operators.update({# negated symbols without pre-composed Unicode character
                  'nleqq':      u'\u2266\u0338', # ≦̸
                  'ngeqq':      u'\u2267\u0338', # ≧̸
                  'nleqslant':  u'\u2a7d\u0338', # ⩽̸
                  'ngeqslant':  u'\u2a7e\u0338', # ⩾̸
                  'nsubseteqq': u'\u2AC5\u0338', # ⫅̸
                  'nsupseteqq': u'\u2AC6\u0338', # ⫆̸
                  # alias commands:
                  'dotsb': u'\u22ef', # ⋯ with binary operators/relations
                  'dotsc': u'\u2026', # … with commas
                  'dotsi': u'\u22ef', # ⋯ with integrals
                  'dotsm': u'\u22ef', # ⋯ multiplication dots
                  'dotso': u'\u2026', # … other dots
                  # functions with movable limits (requires <mo>)
                  'lim': 'lim',
                  'sup': 'sup',
                  'inf': 'inf',
                  'max': 'max',
                  'min': 'min',
                 })

# special cases

thick_operators = {# style='font-weight: bold;'
                   'thicksim':   u'\u223C', # ∼
                   'thickapprox':u'\u2248', # ≈
                  }

small_operators = {# mathsize='75%'
                   'shortmid':       u'\u2223', # ∣
                   'shortparallel':  u'\u2225', # ∥
                   'nshortmid':      u'\u2224', # ∤
                   'nshortparallel': u'\u2226', # ∦
                   'smallfrown':     u'\u2322', # ⌢ FROWN
                   'smallsmile':     u'\u2323', # ⌣ SMILE
                   'smallint':       u'\u222b', # ∫ INTEGRAL
                  }

# Operators and functions with limits above/below in display formulas
# and in index position inline (movablelimits="true")
displaylimits = [operators[name] for name in
                 ('bigcap', 'bigcup', 'bigodot', 'bigoplus', 'bigotimes',
                  'bigsqcup', 'biguplus', 'bigvee', 'bigwedge',
                  'coprod', 'prod', 'sum',
                  'lim', 'max', 'min', 'sup', 'inf')]
# Depending on settings, integrals may also be in this category.
# (e.g. if "amsmath" is loaded with option "intlimits", see
#  http://mirror.ctan.org/macros/latex/required/amsmath/amsldoc.pdf)
# displaylimits.extend(('fint', 'iiiint', 'iiint', 'iint', 'int', 'oiint',
#                       'oint', 'ointctrclockwise', 'sqint',
#                       'varointclockwise',))

# >>> print(' '.join(displaylimits))
# ⋂ ⋃ ⨀ ⨁ ⨂ ⨆ ⨄ ⋁ ⋀ ∐ ∏ ∑ lim max min sup inf

# pre-composed characters for negated symbols
# see https://www.w3.org/TR/xml-entity-names/#combining
negatables = {'=': u'\u2260',
              r'\in': u'\u2209',
              r'\equiv': u'\u2262'}

# extensible delimiters allowed in left/right cmds
stretchables = {'backslash':   '\\',
                'uparrow':     u'\u2191', # ↑ UPWARDS ARROW
                'downarrow':   u'\u2193', # ↓ DOWNWARDS ARROW
                'updownarrow': u'\u2195', # ↕ UP DOWN ARROW
                'Uparrow':     u'\u21d1', # ⇑ UPWARDS DOUBLE ARROW
                'Downarrow':   u'\u21d3', # ⇓ DOWNWARDS DOUBLE ARROW
                'Updownarrow': u'\u21d5', # ⇕ UP DOWN DOUBLE ARROW
               }
stretchables.update(tex2unichar.mathfence)
stretchables.update(tex2unichar.mathopen)
stretchables.update(tex2unichar.mathclose)
stretchables.update(math_fences)

# >>> print(' '.join(sorted(set(stretchables.values()))))
# [ \ ] { | } ‖ ↑ ↓ ↕ ⇑ ⇓ ⇕ ⌈ ⌉ ⌊ ⌋ ⌜ ⌝ ⌞ ⌟ ⟅ ⟆ ⟦ ⟧ ⟨ ⟩ ⟮ ⟯ ⦇ ⦈


# horizontal space -> <mspace>

spaces = {'qquad':         '2em',       # two \quad
          'quad':          '1em',       # 18 mu
          'qquad':         '2em',       # two \quad
          'thickspace':    '0.2778em',  # 5mu = 5/18em
          'medspace':      '0.2222em',  # 4mu = 2/9em
          'thinspace':     '0.1667em',  # 3mu = 1/6em
          'negthinspace':  '-0.1667em', # -3mu = -1/6em
          'negmedspace':   '-0.2222em', # -4mu = -2/9em
          'negthickspace': '-0.2778em', # -5mu = -5/18em
          ' ':             '0.25em',    # inter word space
          ';':             '0.2778em',  # thickspace
          ':':             '0.2222em',  # medspace
          ',':             '0.1667em',  # thinspace
          '!':             '-0.1667em', # negthinspace
         }

# accents -> <mover accent="true">
#        TeX        spacing    combining
accents = {'acute':    u'´',      # u'\u0301',
           'bar':      u'ˉ',      # u'\u0304',
           'breve':    u'˘',      # u'\u0306',
           'check':    u'ˇ',      # u'\u030C',
           'dot':      u'˙',      # u'\u0307',
           'ddot':     u'¨',      # u'\u0308',
           'dddot':               u'\u20DB',  # '…' too wide
           'grave':    u'`',      # u'\u0300',
           'hat':      u'ˆ',      # u'\u0302',
           'mathring': u'˚',      # u'\u030A',
           'tilde':    u'˜',      # u'\u0303',
       }

# limits etc. -> <mover accent="false"> or <munder>
over = {# 'ddot':     u'..',
        # 'dddot':                u'…',  # too wide if accent="true"
        'overbrace':            u'\u23DE', # TOP CURLY BRACKET
        'overleftarrow':        u'\u2190',
        'overleftrightarrow':   u'\u2194',
        'overline':             u'¯',
        'overrightarrow':       u'\u2192',
        'vec':                  u'\u2192', # → too heavy if accent="true"
        'widehat':              u'^',
        'widetilde':            u'~'}
under = {'underbrace':          u'\u23DF',
         'underleftarrow':      u'\u2190',
         'underleftrightarrow': u'\u2194',
         'underline':           u'_',
         'underrightarrow':     u'\u2192'}

# Character translations
# ----------------------
# characters with preferred alternative in mathematical use
# cf. https://www.w3.org/TR/MathML3/chapter7.html#chars.anomalous
anomalous_chars = {'-': u'\u2212', # HYPHEN-MINUS -> MINUS SIGN
                   ':': u'\u2236', # COLON -> RATIO
                   '~': u'\u00a0', # NO-BREAK SPACE
                  }

# blackboard bold (Greek characters not working with "mathvariant" (Firefox 78)
mathbb = {u'Γ': u'\u213E',    # ℾ
          u'Π': u'\u213F',    # ℿ
          u'Σ': u'\u2140',    # ⅀
          u'γ': u'\u213D',    # ℽ
          u'π': u'\u213C',    # ℼ
         }


matrices = {'matrix':  ('', ''),
            'smallmatrix':  ('', ''), # smaller, see begin_environment()!
            'pmatrix': ('(', ')'),
            'bmatrix': ('[', ']'),
            'Bmatrix': ('{', '}'),
            'vmatrix': ('|', '|'),
            'Vmatrix': (u'\u2016', u'\u2016'), # ‖
            'cases':   ('{', ''),
           }

layout_styles = {
    'displaystyle':      {'displaystyle': 'true',  'scriptlevel': '0'},
    'textstyle':         {'displaystyle': 'false', 'scriptlevel': '0'},
    'scriptstyle':       {'displaystyle': 'false', 'scriptlevel': '1'},
    'scriptscriptstyle': {'displaystyle': 'false', 'scriptlevel': '2'},
    }
# See also https://www.w3.org/TR/MathML3/chapter3.html#presm.scriptlevel

fractions = {# name:   style_attrs, frac_attrs
             'frac':   ({}, {}),
             'cfrac':  ({'displaystyle': 'true',  'scriptlevel': '0',
                         'CLASS': 'cfrac'}, {}), # in LaTeX with padding
             'dfrac':  (layout_styles['displaystyle'], {}),
             'tfrac':  (layout_styles['textstyle'], {}),
             'binom':  ({}, {'linethickness': 0}),
             'dbinom': (layout_styles['displaystyle'], {'linethickness': 0}),
             'tbinom': (layout_styles['textstyle'], {'linethickness': 0}),
            }

delimiter_sizes = {'left':  '',
                   'right': '',
                   'bigl':  '1.2em',
                   'bigr':  '1.2em',
                   'Bigl':  '1.623em',
                   'Bigr':  '1.623em',
                   'biggl': '2.047em',
                   'biggr': '2.047em',
                   'Biggl': '2.470em',
                   'Biggr': '2.470em',
                  }

# MathML element classes
# ----------------------

class math(object):
    """Base class for MathML elements and root of MathML trees."""

    nchildren = None
    """Expected number of children or None"""
    # cf. https://www.w3.org/TR/MathML3/chapter3.html#id.3.1.3.2
    parent = None
    """Parent node in MathML DOM tree."""
    _level = 0 # indentation level (static class variable)

    def __init__(self, *children, **attributes):
        """Set up node with `children` and `attributes`.

        Attributes are downcased: Use CLASS to get "class" in XML
        >>> math(mn(3), CLASS='test')
        math(mn(3), CLASS='test')
        >>> math(CLASS='test').toprettyxml()
        '<math class="test">\n</math>'

        """
        self.children = []
        self.extend(children)

        self.attributes = collections.OrderedDict()
        # sort attributes for predictable functional tests
        # as self.attributes.update(attributes) does not keep order in Python < 3.6
        for key in sorted(attributes.keys()):
            self.attributes[key] = attributes[key]

    def __repr__(self):
        content = [repr(item) for item in getattr(self, 'children', [])]
        if hasattr(self, 'data'):
            content.append(repr(self.data))
        if isinstance(self, MathSchema) and self.switch:
            content.append('switch=True')
        if hasattr(self, 'attributes'):
            content += ["%s='%s'"%(k, v) for k, v in self.attributes.items()]
        return self.__class__.__name__ + '(%s)' % ', '.join(content)

    def __len__(self):
        return len(self.children)

    # emulate dictionary-like access to attributes
    # see `docutils.nodes.Element` for dict/list interface
    def __getitem__(self, key):
        return self.attributes[key]
    def __setitem__(self, key, item):
        self.attributes[key] = item

    def full(self):
        """Return boolean indicating whether children may be appended."""
        return (self.nchildren is not None
                and len(self) >= self.nchildren)

    def append(self, child):
        """Append child and return self or first non-full parent.

        If self is full, go up the tree and return first non-full node or
        `None`.
        """
        if self.full():
            raise SyntaxError('Node %s already full!' % self)
        self.children.append(child)
        child.parent = self
        if self.full():
            return self.close()
        return self

    def extend(self, children):
        for child in children:
            self.append(child)
        return self

    def close(self):
        """Close element and return first non-full parent or None."""
        parent = self.parent
        while parent is not None and parent.full():
            parent = parent.parent
        return parent

    def toprettyxml(self):
        """Return XML representation of self as string."""
        return ''.join(self._xml())

    def _xml(self, level=0):
        return ([self.xml_starttag()]
                + self._xml_body(level)
                + ['</%s>' % self.__class__.__name__])

    def xml_starttag(self):
        # Use k.lower() to allow argument `CLASS` for attribute `class`
        # (Python keyword). MathML uses only lowercase attributes.
        attrs = ['%s="%s"'%(k.lower(), v) for k, v in self.attributes.items()]
        return '<%s>' % ' '.join([self.__class__.__name__] + attrs)

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
# >>> eq3 = math(id='eq3')
# >>> eq3
# math(id='eq3')
# >>> eq3.toprettyxml()
# '<math id="eq3">\n</math>'
# >>> len(eq3)
# 0
# >>> math(CLASS='bold').xml_starttag()
# '<math class="bold">'

class mtable(math): pass

# >>> mtable(displaystyle='true')
# mtable(displaystyle='true')
# >>> math(mtable(displaystyle='true')).toprettyxml()
# '<math>\n  <mtable displaystyle="true">\n  </mtable>\n</math>'

class mrow(math):
    """Group sub-expressions as a horizontal row."""

    def close(self):
        """Close element and return first non-full parent or None.

        Remove <mrow>, if it is single child and the parent infers an mrow
        or if it has only one child element.
        """
        parent = self.parent
        if isinstance(parent, MathRowSchema) and parent.nchildren == 1:
            parent.nchildren = None
            parent.children = self.children
            for child in self.children:
                child.parent = parent
            return parent.close()
        if len(self) == 1:
            try:
                parent.children[parent.children.index(self)] = self.children[0]
                self.children[0].parent = parent
            except (AttributeError, ValueError):
                return self.children[0]
        return super(mrow, self).close()

# >>> mrow(displaystyle='false')
# mrow(displaystyle='false')

# The elements <msqrt>, <mstyle>, <merror>, <mpadded>, <mphantom>, <menclose>,
# <mtd>, <mscarry>, and <math> treat their contents as a single inferred mrow
# formed from all their children.
class MathRowSchema(math):
    """Base class for elements treating content as a single inferred mrow."""
class mtr(MathRowSchema): pass
class mtd(MathRowSchema): pass
class mphantom(MathRowSchema):
    nchildren = 1 # \phantom expects one argument or a group
class mstyle(MathRowSchema):
    nchildren = 1 # \mathrm, ... expect one argument or a group
class msqrt(MathRowSchema):
    nchildren = 1 # \sqrt expects one argument or a group
class menclose(MathRowSchema):
    nchildren = 1 # \boxed expects one argument or a group

class MathToken(math):
    """Token Element: contains textual data instead of children.

    Base class for mo, mi, and mn.
    """
    nchildren = 0

    def __init__(self, data, **attributes):
        self.data = data
        super(MathToken, self).__init__(**attributes)

    def _xml_body(self, level=0):
        return [unicode(self.data).translate(xml_entities)]

class mi(MathToken): pass
class mn(MathToken): pass
class mo(MathToken): pass
class mtext(MathToken): pass

# >>> mo(u'<')
# mo('<')
# >>> mo(u'<')._xml()
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
        new_current = super(MathSchema, self).append(child)
        # normalize order if full
        if self.switch and self.full():
            self.children[-1], self.children[-2] = self.children[-2], self.children[-1]
            # self.children.reverse()
            self.switch = False
        return new_current

class msub(MathSchema): pass
class msup(MathSchema): pass
class msubsup(MathSchema):
    nchildren = 3

class munder(MathSchema): pass
class mover(MathSchema): pass
class munderover(MathSchema):
    nchildren = 3

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
# SyntaxError: Node munder(mi('lim'), mo('-'), accent='false') already full!
# >>> munder(mo('-'), mi('lim'), accent='false', switch=True).toprettyxml()
# '<munder accent="false">\n  <mi>lim</mi>\n  <mo>-</mo>\n</munder>'
# >>> msub(mi('x'), mo('-'))
# msub(mi('x'), mo('-'))
# >>> msubsup(mi('base'), mi('sub'), mi('super'))
# msubsup(mi('base'), mi('sub'), mi('super'))
# >>> msubsup(mi('base'), mi('super'), mi('sub'), switch=True)
# msubsup(mi('base'), mi('sub'), mi('super'))

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

    >>> tex_token('{first simple group} returned without brackets')
    ('first simple group', ' returned without brackets')
    >>> tex_token('\\command{without argument}')
    ('\\command', '{without argument}')
    >>> tex_token(' first non-white character')
    ('f', 'irst non-white character')

    """
    m = re.match(r"""\s*                  # leading whitespace
                 {(?P<token>(\\}|[^{}]|\\{)*)} # {group} without nested groups
                 (?P<remainder>.*$)
                 """, string, re.VERBOSE)
    if m is None:
        m = re.match(r"""\s*              # leading whitespace
                      (?P<token>\\([a-zA-Z]+))\s*   # \cmdname
                      (?P<remainder>.*$)
                 """, string, re.VERBOSE)
    if m is None:
        m = re.match(r"""\s*              # leading whitespace
                     (?P<token>.?)        # first character or empty string
                     (?P<remainder>.*$)
                 """, string, re.VERBOSE)

    return m.group('token'), m.group('remainder')

# Test:
#
# >>> tex_token('{opening bracket of group with {nested group}}')
# ('{', 'opening bracket of group with {nested group}}')
# >>> tex_token('{group with \\{escaped\\} brackets}')
# ('group with \\{escaped\\} brackets', '')
# >>> tex_token('{group followed by closing bracket}} from outer group')
# ('group followed by closing bracket', '} from outer group')
# >>> tex_token(' {skip leading whitespace}')
# ('skip leading whitespace', '')
# >>> tex_token('  \\skip{leading whitespace}')
# ('\\skip', '{leading whitespace}')
# >>> tex_token('\\skip whitespace after macro name')
# ('\\skip', 'whitespace after macro name')
# >>> tex_token('') # empty string.
# ('', '')

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
        raise SyntaxError('Could not extract optional argument from %r' % string)

# Test:
# >>> tex_optarg(' [optional argument] after whitespace')
# ('optional argument', ' after whitespace')
# >>> tex_optarg('[missing right bracket')
# Traceback (most recent call last):
# SyntaxError: Could not extract optional argument from '[missing right bracket'
# >>> tex_optarg('[group with [nested group]]')
# Traceback (most recent call last):
# SyntaxError: Could not extract optional argument from '[group with [nested group]]'


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
        # Take of first character:
        c, string = string[0], string[1:]

        if c == ' ':
            continue  # whitespace is ignored in LaTeX math mode
        if c == '\\': # start of a LaTeX macro
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
            node = node.append(mo(anomalous_chars[c]))
            # TODO: fix spacing before "unary" minus.
            # set form='prefix' if preceded by "(", "{", ...?
        elif c in "/()[]|":
            node = node.append(mo(c, stretchy='false'))
        elif c in "+*=<>,.!?';@":
            node = node.append(mo(c))
        else:
            raise SyntaxError(u'Unsupported character: "%s"' % c)
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
# math(munder(mo('max', movablelimits='true'), mi('x')))
# >>> parse_latex_math(math(), 'x^j_i') # ensure correct order: base, sub, sup
# math(msubsup(mi('x'), mi('i'), mi('j')))
# >>> parse_latex_math(math(), '\int^j_i') # ensure correct order
# math(msubsup(mo('∫'), mi('i'), mi('j')))
# >>> parse_latex_math(math(), 'x_{\\alpha}')
# math(msub(mi('x'), mi('α')))
# >>> parse_latex_math(math(), 'x_\\text{in}')
# math(msub(mi('x'), mtext('in')))

def handle_cmd(name, node, string):
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
        if name in greek_capitals:
            # upright in "TeX style" but MathML sets them italic ("ISO style").
            # CSS styling does not change the font style in Firefox 78.
            # Use 'mathvariant="normal"'?
            node = node.append(mi(greek_capitals[name], CLASS='capital-greek'))
        else:
            node = node.append(mi(letters[name]))
        return node, string

    if (name in functions):
        # use <mi> followed by invisible function applicator character
        # (see https://www.w3.org/TR/MathML3/chapter3.html#presm.mi)
        if name == 'operatorname':
            # custom function name, e.g. ``\operatorname{abs}(x)``
            # TODO: \operatorname* -> with limits
            arg, string = tex_token(string)
            new_node = mi(arg, mathvariant='normal')
        else:
            new_node = mi(functions[name])
        # embellished function names:
        if name == 'varliminf':    # \underline\lim
            new_node = munder(new_node, mo(u'_'))
        elif name == 'varlimsup':  # \overline\lim
            new_node = mover(new_node, mo(u'¯'), accent='false')
        elif name == 'varprojlim': # \underleftarrow\lim
            new_node = munder(new_node, mo(u'\u2190'))
        elif name == 'varinjlim':  # \underrightarrow\lim
            new_node = munder(new_node, mo(u'\u2192'))

        node = node.append(new_node)
        # add ApplyFunction when appropriate (not \sin^2(x), say)
        # cf. https://www.w3.org/TR/MathML3/chapter3.html#presm.mi
        if string and string[0] not in ('^', '_'):
            node = node.append(mo(u'\u2061')) # &ApplyFunction;
        return node, string

    if name in math_alphabets:
        arg, remainder = tex_token(string)
        if arg[0] == '\\':
            # convert single letters (so the isalpha() test below works).
            arg = letters.get(arg[1:], arg)
        if name == 'mathbb':
            # mathvariant="double-struck" is ignored for Greek letters
            # (tested in Firefox 78). Use literal Unicode characters.
            arg = mathbb.get(arg, arg)
        if name == 'boldsymbol':
            attributes = {'style': 'font-weight: bold'}
        else:
            attributes = {'mathvariant': math_alphabets[name]}
        if name == 'mathscr':
            attributes['class'] = 'mathscr'

        # one symbol (single letter, name, or ⅀)
        if arg.isalpha() or arg == u'\u2140':
            node = node.append(mi(arg, **attributes))
            return node, remainder
        # Wrap in <style>
        style = mstyle(**attributes)
        node.append(style)
        return style, string


    # operator, fence, or separator  ->  <mo>

    if name == 'colon': # trailing punctuation, not binary relation
        node = node.append(mo(':', form='postfix', lspace='0', rspace='0.28em'))
        return node, string

    if name in thick_operators:
        node = node.append(mo(thick_operators[name], style='font-weight: bold'))
        return node, string

    if name in small_operators:
        node = node.append(mo(small_operators[name], mathsize='75%'))
        return node, string

    if name in operators:
        node = node.append(mo(operators[name]))
        return node, string

    if name in delimiter_sizes:
        delimiter_attributes = {}
        size = delimiter_sizes[name]
        delimiter, string = tex_token(string)
        if delimiter not in '()[]/|.':
            try:
                delimiter = stretchables[delimiter.lstrip('\\')]
            except KeyError:
                raise SyntaxError(u'Missing "\\%s" delimiter!' % name)
        if size:
            delimiter_attributes['maxsize'] = size
            delimiter_attributes['minsize'] = size
        if name == 'left' or name.endswith('l'):
            row = mrow()
            node.append(row)
            node = row
        if delimiter != '.': # '.' stands for "empty delimiter"
            node.append(mo(delimiter, **delimiter_attributes))
        if name == 'right' or name.endswith('r'):
            node = node.close()
        return node, string

    if name == 'not':
        arg, string = tex_token(string)
        try:
            node = node.append(mo(negatables[arg]))
        except KeyError:
            raise SyntaxError(u'"\\not: Cannot negate: %s!'%arg)
        return node, string

    # arbitrary text (usually comments)  ->  <mtext>
    if name in ('text', 'mbox', 'textrm'):
        arg, string = tex_token(string)
        parts = arg.split('$') # extract inline math
        for i, part in enumerate(parts):
            if i % 2 == 0: # i is even
                part = re.sub('(^ | $)', u'\u00a0', part)
                node = node.append(mtext(part))
            else:
                parse_latex_math(node, part)
        return node, string

    # horizontal space -> <mspace>
    if name in spaces:
        node = node.append(mspace(width='%s'%spaces[name]))
        return node, string

    if name == 'hspace':
        arg, string = tex_token(string)
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
                frac_atts['numalign'] = optargs[optarg]
        new_node = frac = mfrac(**frac_atts)
        if name.endswith('binom'):
            new_node = mrow(mo('('), new_node, mo(')'))
            new_node.close()
        if style_atts:
            new_node = mstyle(new_node, **style_atts)
        node.append(new_node)
        return frac, string

    if name == '\\': # end of a row
        entry = mtd()
        new_node = mtr(entry)
        node.close().close().append(new_node)
        return entry, string

    if name in accents or name in over:
        try:
            ch = over[name]
            acc = 'false'
        except KeyError:
            ch = accents[name]
            acc = 'true'
        new_node = mover(mo(ch), switch=True, accent=acc)
        node.append(new_node)
        return new_node, string

    if name == 'overset':
        new_node = mover(switch=True)
        node.append(new_node)
        return new_node, string

    if name in under:
        new_node = munder(mo(under[name]), switch=True)
        node.append(new_node)
        return new_node, string

    if name == 'underset':
        new_node = munder(switch=True)
        node.append(new_node)
        return new_node, string

    if name in ('xleftarrow', 'xrightarrow'):
        subscript, string = tex_optarg(string)
        base = mo(operators[name[1:]])
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

    if name in layout_styles: # 'displaystyle', 'textstyle', ...
        new_node = mstyle(**layout_styles[name])
        new_node.nchildren = None
        if isinstance(node, mrow) and len(node) == 0:
            # replace node with new_node
            node.parent.children[node.parent.children.index(node)] = new_node
            new_node.parent = node.parent
        elif node.__class__.__name__ == 'math':
            node.append(new_node)
        else:
            raise SyntaxError(u'Declaration "\\%s" must be first command '
                              u'in a group.' % name)
        return new_node, string

    if name.endswith('limits'):
        arg, remainder = tex_token(string)
        if arg in '_^': # else ignore
            string = remainder
            node = handle_script_or_limit(node, arg, limits=name)
        return node, string

    # Environments

    if name == 'begin':
        return begin_environment(node, string)

    if name == 'end':
        return end_environment(node, string)



    raise SyntaxError(u'Unknown LaTeX command: ' + name)

# >>> handle_cmd('left', math(), '[a\\right]')
# (mrow(mo('[')), 'a\\right]')
# >>> handle_cmd('left', math(), '. a)') # emtpy \left
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
# >>> handle_cmd('mathrm', math(), '\\alpha')
# (math(mi('α', mathvariant='normal')), '')
# >>> handle_cmd('mathrm', math(), '{out} = 3')
# (math(mi('out', mathvariant='normal')), ' = 3')
# >>> handle_cmd('overline', math(), '{981}')
# (mover(mo('¯'), switch=True, accent='false'), '{981}')
# >>> handle_cmd('bar', math(), '{x}')
# (mover(mo('ˉ'), switch=True, accent='true'), '{x}')
# >>> handle_cmd('xleftarrow', math(), r'[\alpha]{10}')
# (munderover(mo('←'), mi('α')), '{10}')
# >>> handle_cmd('xleftarrow', math(), r'[\alpha=5]{10}')
# (munderover(mo('←'), mrow(mi('α'), mo('='), mn('5'))), '{10}')


def handle_script_or_limit(node, c, limits=''):
    """Append script or limit element to `node`."""
    child = node.children.pop()
    if limits == 'limits':
        child['movablelimits'] = 'false'
    elif (limits == 'displaylimits'
          or getattr(child, 'data', '') in displaylimits):
        child['movablelimits'] = 'true'

    if c == '_':
        if isinstance(child, msup):
            new_node = msubsup(*child.children, switch=True)
        elif isinstance(child, mover):
            new_node = munderover(*child.children, switch=True)
        elif (limits in ('limits', 'displaylimits')
              or limits == ''
              and getattr(child, 'data', '') in displaylimits):
            new_node = munder(child)
        else:
            new_node = msub(child)
    elif c == '^':
        if isinstance(child, msub):
            new_node = msubsup(*child.children)
        elif isinstance(child, munder):
            new_node = munderover(*child.children)
        elif (limits in ('limits', 'displaylimits')
              or limits == ''
              and getattr(child, 'data', '') in displaylimits):
            new_node = mover(child)
        else:
            new_node = msup(child)
    node.append(new_node)
    return new_node


def begin_environment(node, string):
    name, string = tex_token(string)
    if name in matrices:
        left_delimiter = matrices[name][0]
        attributes = {}
        if left_delimiter:
            wrapper = mrow(mo(left_delimiter))
            node.append(wrapper)
            node = wrapper
        elif name == 'smallmatrix':
            attributes['rowspacing'] = '0.2em' # unimplemented in Firefox!
            attributes['columnspacing'] = '0.333em'
            wrapper = mstyle(scriptlevel=1)
            node.append(wrapper)
            node = wrapper
        entry = mtd()
        node.append(mtable(mtr(entry), **attributes))
        node = entry
    else:
        raise SyntaxError(u'Environment not supported!')
    return node, string


def end_environment(node, string):
    name, string = tex_token(string)
    if name in matrices:
        node = node.close().close().close() # close: mtd, mdr, mtable
        right_delimiter = matrices[name][1]
        if right_delimiter:
            node = node.append(mo(right_delimiter))
            node = node.close()
        elif name == 'cases':
            node = node.close()
    else:
        raise SyntaxError(u'Environment not supported!')
    return node, string


def tex2mathml(tex_math, inline=True):
    """Return string with MathML code corresponding to `tex_math`.

    Set `inline` to False for displayed math.
    """
    # Set up tree
    tree = math(xmlns='http://www.w3.org/1998/Math/MathML')
    if inline:
        node = tree
    else:
        # block: emulate align* environment with a math table
        tree['display'] = 'block'
        node = mtd()
        tree.append(mtable(mtr(node), displaystyle='true', CLASS='align'))

    parse_latex_math(node, tex_math)
    return tree.toprettyxml()

# >>> print(tex2mathml('3'))
# <math xmlns="http://www.w3.org/1998/Math/MathML">
#   <mn>3</mn>
# </math>
# >>> print(tex2mathml('3', inline=False))
# <math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
#   <mtable class="align" displaystyle="true">
#     <mtr>
#       <mtd>
#         <mn>3</mn>
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
