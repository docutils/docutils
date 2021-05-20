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
functions = dict((name, name) for name in 
                 ('arccos', 'arcsin', 'arctan', 'arg', 'cos',  'cosh',
                  'cot',    'coth',   'csc',    'deg', 'det',  'dim',
                  'exp',    'gcd',    'hom',    'inf', 'ker',  'lg',
                  'ln',     'log',    'max', 'min',  'Pr',
                  'sec',    'sin',    'sinh',   'sup', 'tan',  'tanh'))
functions.update({# functions with a space in the name
                  'liminf': u'lim\u202finf', 'limsup': u'lim\u202fsup',
                  'injlim': u'inj\u202flim', 'projlim': u'proj\u202flim',
                  # embellished function names (see handle_cmdname() below)
                  'varlimsup': 'lim', 'varliminf': 'lim',
                  'varprojlim': 'lim', 'varinjlim': 'lim',
                  # custom function name
                  'operatorname': None})

# function with limits, use <mo> to allow "movablelimits" attribute
functions_with_limits = dict((name, name) for name in 
                             ('lim', 'sup', 'inf', 'max', 'min'))

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

operators = tex2unichar.mathbin         # Binary symbols
operators.update(tex2unichar.mathrel)   # Relation symbols, arrow symbols
operators.update(tex2unichar.mathord)   # Miscellaneous symbols
operators.update(tex2unichar.mathop)    # Variable-sized symbols
operators.update(tex2unichar.mathopen)  # Braces
operators.update(tex2unichar.mathclose) # Braces
operators.update(tex2unichar.mathfence)
operators.update(functions_with_limits)
operators.update({# negated symbols without pre-composed Unicode character
                  'nleqq':      u'\u2266\u0338', # ≦̸
                  'ngeqq':      u'\u2267\u0338', # ≧̸
                  'nleqslant':  u'\u2a7d\u0338', # ⩽̸
                  'ngeqslant':  u'\u2a7e\u0338', # ⩾̸
                  'nsubseteqq': u'\u2AC5\u0338', # ⫅̸
                  'nsupseteqq': u'\u2AC6\u0338', # ⫆̸
                  # alias commands:
                  'lvert': u'|',      # pairing delimiters
                  'lVert': u'\u2016', # ‖
                  'rvert': u'|',
                  'rVert': u'\u2016',
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
                   'smallfrown': u'\u2322', # ⌢ FROWN
                   'smallsmile': u'\u2323', # ⌣ SMILE
                   'smallint':   u'\u222b', # ∫ INTEGRAL
                  }

# Operators and functions with limits
# over/under in display formulas and in index position inline
sumintprod = [operators[name] for name in
              ('coprod', 'fatsemi', 'fint', 'iiiint', 'iiint',
               'iint', 'int', 'oiint', 'oint', 'ointctrclockwise',
               'prod', 'sqint', 'sum', 'varointclockwise',
               'lim', 'sup', 'inf', 'max', 'min')]

# pre-composed characters for negated symbols
# see https://www.w3.org/TR/xml-entity-names/#combining
negatables = {'=': u'\u2260',
              r'\in': u'\u2209',
              r'\equiv': u'\u2262'}

# extensible delimiters allowed in left/right cmds
stretchables = {'(': '(',
                ')': ')',
                '[': '[',
                ']': ']',
                '/': '/',
                r'\backslash': '\\',
                '|': '|',
                '.': '', # emty fence
                r'\uparrow': u'\u2191', # ↑ UPWARDS ARROW
                r'\downarrow': u'\u2193', # ↓ DOWNWARDS ARROW
                r'\updownarrow': u'\u2195', # ↕ UP DOWN ARROW
                r'\Uparrow': u'\u21d1', # ⇑ UPWARDS DOUBLE ARROW
                r'\Downarrow': u'\u21d3', # ⇓ DOWNWARDS DOUBLE ARROW
                r'\Updownarrow': u'\u21d5', # ⇕ UP DOWN DOUBLE ARROW
             }
for (key, value) in tex2unichar.mathfence.items():
    stretchables['\\'+key] = value
for (key, value) in tex2unichar.mathopen.items():
    stretchables['\\'+key] = value
for (key, value) in tex2unichar.mathclose.items():
    stretchables['\\'+key] = value
# shorter with {**something} syntax, new in 3.5
# if sys.version_info >= (3, 5):
#     for (key, value) in {**tex2unichar.mathclose,
#                          **tex2unichar.mathopen,
#                          **tex2unichar.mathfence}.items():
#         stretchables['\\'+key] = value

# >>> print(' '.join(sorted(set(stretchables.values()))))
#  ( ) / [ \ ] { | } ‖ ↑ ↓ ↕ ⇑ ⇓ ⇕ ⌈ ⌉ ⌊ ⌋ ⌜ ⌝ ⌞ ⌟ ⟅ ⟆ ⟦ ⟧ ⟨ ⟩ ⟮ ⟯ ⦇ ⦈


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
          r'\Gamma': u'\u213E', # ℾ
          r'\Pi':    u'\u213F', # ℿ
          r'\Sigma': u'\u2140', # ⅀
          r'\gamma': u'\u213D', # ℽ
          r'\pi':    u'\u213C', # ℼ
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
        for child in children:
            self.append(child)

        self.attributes = collections.OrderedDict()
        # sort attributes for predictable functional tests
        # as self.attributes.update(attributes) does not keep order in Python < 3.6
        for key in sorted(attributes.keys()):
            self.attributes[key] = attributes[key]

    def __repr__(self):
        content = [repr(item) for item in getattr(self, 'children', [])]
        if hasattr(self, 'data'):
            content.append(repr(self.data))
        if isinstance(self, MathScriptOrLimit) and self.switch:
            content.append('switch=True')
        if hasattr(self, 'attributes'):
            content += ["%s='%s'"%(k, v) for k, v in self.attributes.items()]
        return self.__class__.__name__ + '(%s)' % ', '.join(content)

    def __len__(self):
        return len(self.children)

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

class mrow(math):
    """Group sub-expressions as a horizontal row."""

# >>> mrow(displaystyle='false')
# mrow(displaystyle='false')

class mtable(math): pass

# >>> mtable(displaystyle='true')
# mtable(displaystyle='true')
# >>> math(mtable(displaystyle='true')).toprettyxml()
# '<math>\n  <mtable displaystyle="true">\n  </mtable>\n</math>'

# The elements <msqrt>, <mstyle>, <merror>, <mpadded>, <mphantom>, <menclose>,
# <mtd>, <mscarry>, and <math> treat their contents as a single inferred mrow
# formed from all their children.
class msqrt(mrow): pass
class mstyle(mrow): pass
class mtr(mrow): pass
class mtd(mrow): pass

class MathToken(math):
    """Token Element: contains data instead of children.

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

class MathScriptOrLimit(math):
    """Base class for script and limit schemata."""
    nchildren = 2

    def __init__(self, *children, **kwargs):
        """Set up sub/superscript or limit elements.

        The special attribute `switch` tells that the
        last two child elements are in reversed order
        and must be switched before XML-export.
        """
        self.switch = kwargs.pop('switch', False)
        math.__init__(self, *children, **kwargs)

    def append(self, child):
        new_current = super(MathScriptOrLimit, self).append(child)
        # normalize order if full
        if self.switch and self.full():
            self.children[-1], self.children[-2] = self.children[-2], self.children[-1]
            # self.children.reverse()
            self.switch = False
        return new_current

class msub(MathScriptOrLimit): pass
class msup(MathScriptOrLimit): pass
class msubsup(MathScriptOrLimit):
    nchildren = 3

class munder(MathScriptOrLimit): pass
class mover(MathScriptOrLimit): pass
class munderover(MathScriptOrLimit):
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

class mroot(MathScriptOrLimit):
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
    """Return first number literal and remainder of `string`.

    >>> tex_number('123.4')
    ('123.4', '')

    """
    m = re.match(r'([0-9.,]+)(.*)', string)
    if m is None:
        return '', string
    return m.group(1), m.group(2)

# Test:
#
# >>> tex_number(' 123.4')
# ('', ' 123.4')
# >>> tex_number('23,400')
# ('23,400', '')
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
# >>> tex_optarg('[optional argument] after whitespace')
# ('optional argument', ' after whitespace')
# >>> tex_optarg('[missing right bracket')
# Traceback (most recent call last):
# SyntaxError: Could not extract optional argument from '[missing right bracket'
# >>> tex_optarg('[group with [nested group]]')
# Traceback (most recent call last):
# SyntaxError: Could not extract optional argument from '[group with [nested group]]'




def parse_latex_math(node, string):
    """Append MathML conversion of `string` to `node`.

    Return current node.

    >>> parse_latex_math(math(), r'\alpha')
    math(mi('α'))
    >>> parse_latex_math(math(), r'{')
    mrow()

    Set `inline` to False for displayed math.
    """
    # Normalize white-space:
    string = ' '.join(string.split())

    while len(string) > 0:
        # Take of first character:
        c, string = string[0], string[1:]

        if c == ' ':
            continue  # whitespace is ignored in LaTeX math mode
        if c == '\\': # start of a LaTeX macro
            cmdname, string = tex_cmdname(string)
            node, string = handle_cmdname(cmdname, node, string)
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
        elif c in "/()[]|":
            node = node.append(mo(c, stretchy='false'))
        # use dedicated mathematical operator characters
        elif c in anomalous_chars:
            node = node.append(mo(anomalous_chars[c]))
        elif c in "+*=<>,.!?';@":
            node = node.append(mo(c))
        else:
            raise SyntaxError(u'Illegal character: "%s"' % c)
    return node

# Test:

# >>> parse_latex_math(math(xmlns='http://www.w3.org/1998/Math/MathML'), '')
# math(xmlns='http://www.w3.org/1998/Math/MathML')
# >>> parse_latex_math(math(), ' \\sqrt{ \\alpha}')
# math(msqrt(mi('α')))
# >>> parse_latex_math(math(), '23.4x')
# math(mn('23.4'), mi('x'))
# >>> parse_latex_math(math(), '\\sqrt 2 \\ne 3')
# math(msqrt(mn('2')), mo('≠'), mn('3'))
# >>> parse_latex_math(math(), '\\sqrt{2 + 3} < 3')
# math(msqrt(mn('2'), mo('+'), mn('3')), mo('<'), mn('3'))
# >>> parse_latex_math(math(), '\max_x') # function takes limits
# math(munder(mi('max', movablelimits='true'), mi('x')))
# >>> parse_latex_math(math(), 'x^j_i') # ensure correct order: base, sub, sup
# math(msubsup(mi('x'), mi('i'), mi('j')))
# >>> parse_latex_math(math(), '\int^j_i') # ensure correct order
# math(munderover(mo('∫', movablelimits='true'), mi('i'), mi('j')))

def handle_cmdname(name, node, string):
    """Process LaTeX command `name` followed by `string`.

    Append result to `node`.
    If needed, parse `string` for command argument.
    Return new current node and remainder of `string`:

    >>> handle_cmdname('hbar', math(), r' \frac')
    (math(mi('ℏ')), ' \\frac')
    >>> handle_cmdname('hspace', math(), r'{1ex} (x)')
    (math(mspace(width='1ex')), ' (x)')

    """

    # Token elements
    # ==============

    # identifier  ->  <mi>

    if name in letters:
        if name in greek_capitals:
            node = node.append(mi(greek_capitals[name], CLASS='capital-greek'))
            # upright in "TeX style" but MathML sets them italic ("ISO style").
            # CSS styling does not change the font style in Firefox 78.
            # Use 'mathvariant="normal"'?
        else:
            node = node.append(mi(letters[name]))
        return node, string

    if (name in functions):
        # use <mi> followed by invisible function applicator character
        # (see https://www.w3.org/TR/MathML3/chapter3.html#presm.mi)
        if name == 'operatorname':
            # custom function name ``\operatorname{abs}(x)``
            arg, string = tex_token(string)
            new_node = mi(arg, mathvariant='normal')
        else:
            new_node = mi(functions[name])
        # compound function symbols:
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
            if name == 'mathbb':
                # mathvariant="double-struck" is ignored for Greek letters
                # (tested in Firefox 78). Use literal Unicode characters.
                arg = mathbb.get(arg, arg)
            # convert single letters (so they can be set with <mi>)
            arg = letters.get(arg[1:], arg)

        if name == 'boldsymbol':
            attributes = {'style': 'font-weight: bold'}
        else:
            attributes = {'mathvariant': math_alphabets[name]}
        if name == 'mathscr':
            # alternative script letter shapes
            attributes['style'] = 'font-family: STIX'

        # one symbol (single letter, name, or number)
        if arg.isalpha():
            node = node.append(mi(arg, **attributes))
            return node, remainder
        if arg.replace('.', '').replace(',', '').isdigit():
            node = node.append(mn(arg, **attributes))
            return node, remainder
        if len(arg) == 1 and arg != '{':
            node = node.append(mo(arg, **attributes))
            return node, remainder

        # Wrap in <style>
        style = mstyle(**attributes)
        node.append(style)
        return style, string[1:] # take of the opening '{', <mrow> is inferred


    # operator, fence, or separator  ->  <mo>

    if name == 'colon': # trailing punctuation, not binary relation
        node = node.append(mo(':', lspace='0', rspace='0.28em'))
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

    if name in ('left', 'right'):
        arg, string = tex_token(string)
        try:
            delimiter = stretchables[arg]
        except KeyError:
            raise SyntaxError(u'Missing %s delimiter!' % name)
        if name == 'left':
            row = mrow()
            node.append(row)
            node = row
        if delimiter: # may be empty (source '.')
            node.append(mo(delimiter))
        if name == 'right':
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
        text = re.sub('(^ | $)', u'\u00a0', arg)
        node = node.append(mtext(text))
        return node, string

    # horizontal space -> <mspace>

    if name in spaces:
        node = node.append(mspace(width='%s'%spaces[name]))
        return node, string

    if name == 'hspace':
        arg, string = tex_token(string)
        node = node.append(mspace(width='%s'%arg))
        return node, string

    # Complex elements (Layout schemata)
    # ==================================

    if name == 'sqrt':
        # TODO: optional arg -> <mroot> <mn>2</mn> <mn>3</mn> </mroot>
        degree, string = tex_optarg(string)
        if degree:
            indexnode = mrow()
            parse_latex_math(indexnode, degree)
            new_node = mroot(indexnode, switch=True)
        else:
            new_node = msqrt()
            if string.startswith('{'): # argument is a group
                string = string[1:] # mrow implied, skip opening bracket
            else: # no group, enclose only one element
                new_node.nchildren = 1
        node.append(new_node)
        return new_node, string

    if name == 'frac':
        new_node = mfrac()
        node.append(new_node)
        return new_node, string

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
            sub_node = mrow()
            parse_latex_math(sub_node, subscript)
            if len(sub_node) == 1:
                sub_node = sub_node.children[0]
            new_node.append(sub_node)
        else:
            new_node = mover(base)
        node.append(new_node)
        return new_node, string

    if name == 'begin':
        env_name, string = tex_token(string)
        if env_name == 'matrix':
            entry = mtd()
            table = mtable(mtr(entry))
            node.append(table)
            node = entry
        elif env_name == 'cases':
            entry = mtd()
            cases = mrow(mo('{'), mtable(mtr(entry)))
            node.append(cases)
            node = entry
        else:
            raise SyntaxError(u'Environment not supported! '
                        u'Supported environments: "matrix", "cases".')
        return node, string

    if name == 'end':
        env_name, string = tex_token(string)
        if env_name == 'matrix':
            node = node.close().close().close()
        elif env_name == 'cases':
            node = node.close().close().close().close()
        else:
            raise SyntaxError(u'Environment not supported! '
                        u'Supported environments: "matrix", "cases".')
        return node, string

    raise SyntaxError(u'Unknown LaTeX command: ' + name)


# >>> handle_cmdname('left', math(), '[a\\right]')
# (mrow(mo('[')), 'a\\right]')
# >>> handle_cmdname('left', math(), '. a)') # emtpy \left
# (mrow(), ' a)')
# >>> handle_cmdname('left', math(), '\\uparrow a)') # cmd
# (mrow(mo('↑')), 'a)')
# >>> handle_cmdname('not', math(), '\\equiv \\alpha)') # cmd
# (math(mo('≢')), '\\alpha)')
# >>> handle_cmdname('text', math(), '{ for } i>0') # group
# (math(mtext('\xa0for\xa0')), ' i>0')
# >>> handle_cmdname('text', math(), '{B}T') # group
# (math(mtext('B')), 'T')
# >>> handle_cmdname('text', math(), '{number of apples}}') # group
# (math(mtext('number of apples')), '}')
# >>> handle_cmdname('text', math(), 'i \\sin(x)') # single char
# (math(mtext('i')), ' \\sin(x)')
# >>> handle_cmdname('sin', math(), '(\\alpha)')
# (math(mi('sin'), mo('\u2061')), '(\\alpha)')
# >>> handle_cmdname('sin', math(), ' \\alpha')
# (math(mi('sin'), mo('\u2061')), ' \\alpha')
# >>> handle_cmdname('operatorname', math(), '{abs}(x)')
# (math(mi('abs', mathvariant='normal'), mo('\u2061')), '(x)')
# >>> handle_cmdname('mathrm', math(), '\\alpha')
# (math(mi('α', mathvariant='normal')), '')
# >>> handle_cmdname('mathrm', math(), '{out} = 3')
# (math(mi('out', mathvariant='normal')), ' = 3')
# >>> handle_cmdname('overline', math(), '{981}')
# (mover(mo('¯'), switch=True, accent='false'), '{981}')
# >>> handle_cmdname('bar', math(), '{x}')
# (mover(mo('ˉ'), switch=True, accent='true'), '{x}')
# >>> handle_cmdname('xleftarrow', math(), r'[\alpha]{10}')
# (munderover(mo('←'), mi('α')), '{10}')
# >>> handle_cmdname('xleftarrow', math(), r'[\alpha=5]{10}')
# (munderover(mo('←'), mrow(mi('α'), mo('='), mn('5'))), '{10}')


def handle_script_or_limit(node, c):
    """Append script or limit element to `node`."""
    child = node.children.pop()
    if c == '_':
        if isinstance(child, msup):
            new_node = msubsup(*child.children, switch=True)
        elif isinstance(child, mover):
            new_node = munderover(*child.children, switch=True)
        # elif isinstance(child, MathToken) and child.data in sumintprod:
        elif getattr(child, 'data', '') in sumintprod:
            child.attributes['movablelimits'] = 'true'
            new_node = munder(child)
        else:
            new_node = msub(child)
    elif c == '^':
        if isinstance(child, msub):
            new_node = msubsup(*child.children)
        elif isinstance(child, munder):
            new_node = munderover(*child.children)
        elif isinstance(child, MathToken) and child.data in sumintprod:
            child.attributes['movablelimits'] = 'true'
            new_node = mover(child)
        else:
            new_node = msup(child)
    node.append(new_node)
    return new_node


def tex2mathml(tex_math, inline=True):
    """Return string with MathML code corresponding to `tex_math`.

    Set `inline` to False for displayed math.
    """
    # Set up tree
    tree = node = math(xmlns='http://www.w3.org/1998/Math/MathML')
    if not inline:
        # block: emulate align* environment with a math table
        tree.attributes['display']='block'
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
