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


"""Convert LaTex maths code into presentational MathML.
"""

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
#
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

# functions -> <mi> + ApplyFunction
functions = ['arccos', 'arcsin', 'arctan', 'arg', 'cos',  'cosh',
             'cot',    'coth',   'csc',    'deg', 'det',  'dim',
             'exp',    'gcd',    'hom',    'inf', 'ker',  'lg',
             'ln',     'log',    'max', 'min',  'Pr',
             'sec',    'sin',    'sinh',   'sup', 'tan',  'tanh']
functions = dict((name, name) for name in functions)
functions.update({# functions with a space in the name
                  'liminf': u'lim\u202finf', 'limsup': u'lim\u202fsup',
                  'injlim': u'inj\u202flim', 'projlim': u'proj\u202flim',
                  # embellished function names (see handle_keyword() below)
                  'varlimsup': 'lim', 'varliminf': 'lim',
                  'varprojlim': 'lim', 'varinjlim': 'lim'})

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
operators.update({# negated symbols without pre-composed Unicode character
                  'nleqq':      u'\u2266\u0338', # ≦̸
                  'ngeqq':      u'\u2267\u0338', # ≧̸
                  'nleqslant':  u'\u2a7d\u0338', # ⩽̸
                  'ngeqslant':  u'\u2a7e\u0338', # ⩾̸
                  'nsubseteqq': u'\u2AC5\u0338', # ⫅̸
                  'nsupseteqq': u'\u2AC6\u0338', # ⫆̸
                  # use <mo> to allow "movablelimits" attribute
                  'lim':        u'lim',
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

left_delimiters = {# rspace='0', lspace='0.22em'
                   'lvert': '|',
                   'lVert': u'\u2016' # ‖ DOUBLE VERTICAL LINE
                  }

right_delimiters = {# lspace='0', rspace='0.22em'
                    'rvert': '|',
                    'rVert': u'\u2016', # ‖ DOUBLE VERTICAL LINE
                   }

# operators with limits either over/under or in index position
sumintprod = ['coprod', 'fatsemi', 'fint', 'iiiint', 'iiint',
              'iint', 'int', 'oiint', 'oint', 'ointctrclockwise',
              'prod', 'sqint', 'sum', 'varointclockwise']
sumintprod += [operators[name] for name in sumintprod]
sumintprod.append('lim')

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

# limits etc. -> <mover> rsp. <munder>
over = {'overbrace':            u'\u23DE', # TOP CURLY BRACKET
        'overleftarrow':        u'\u2190',
        'overleftrightarrow':   u'\u2194',
        'overline':             u'¯',
        'overrightarrow':       u'\u2192',
        'vec':                  u'\u2192', # → (too heavy if accent="true")
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
anomalous_chars = {'-': u'\u2212', # HYPHEN-MINUS -> MINUS SIGN
                   ':': u'\u2236', # COLON -> RATIO
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
    """Base class for MathML elements."""

    nchildren = None
    """Expected number of children or None"""
    parent = None
    """Parent node in MathML DOM tree."""
    _level = 0 # indentation level (static class variable)

    def __init__(self, *children, **attributes):
        """math(*children **attributes) -> MathML element
        """

        self.children = []
        for child in children:
            self.append(child)

        self.attributes = collections.OrderedDict()
        # sort attributes for predictable functional tests
        # as self.attributes.update(attributes) does not keep order in Python < 3.6
        for key in sorted(attributes.keys()):
            self.attributes.setdefault(key, attributes[key])

    def __repr__(self):
        content = [repr(item) for item in getattr(self, 'children', [])]
        if hasattr(self, 'data'):
            content.append(repr(self.data))
        if isinstance(self, MathScriptOrLimit) and self.reversed:
            content.append('reversed=True')
        if hasattr(self, 'attributes'):
            content += ["%s='%s'"%(k, v) for k, v in self.attributes.items()]
        return self.__class__.__name__ + '(%s)' % ', '.join(content)

    def full(self):
        """Room for more children?"""
        return (self.nchildren is not None
                and len(self.children) >= self.nchildren)

    def append(self, child):
        """append(child) -> element

        Appends child and returns self if self is not full or first
        non-full parent."""

        assert not self.full()
        self.children.append(child)
        child.parent = self
        node = self
        while node is not None and node.full():
            node = node.parent
        return node

    def close(self):
        """close() -> parent

        Close element and return first non-full element."""

        parent = self.parent
        while parent.full():
            parent = parent.parent
        return parent

    def xml(self):
        """xml() -> xml-string"""

        return self.xml_start() + self.xml_body() + self.xml_end()

    def xml_start(self):
        # Use k.lower() to allow argument `CLASS` for attribute `class`
        # (Python keyword). MathML uses only lowercase attributes.
        attrs = ['%s="%s"'%(k.lower(), v) for k, v
                 in getattr(self, 'attributes', {}).items()]
        if not isinstance(self, MathToken): # token elements
            math._level += 1
        return ['<%s>' % ' '.join([self.__class__.__name__] + attrs)]

    def xml_end(self):
        xml = []
        if not isinstance(self, MathToken): # except token elements
            math._level -= 1
            xml.append('\n' + '  ' * math._level)
        xml.append('</%s>' % self.__class__.__name__)
        return xml

    def xml_body(self):
        xml = []
        last_child = None
        for child in self.children:
            if not (isinstance(last_child, MathToken) and isinstance(child, MathToken)):
                xml.append('\n' + '  ' * math._level)
            xml.extend(child.xml())
            last_child = child
        return xml

# >>> math(mn(2))
# math(mn(2))
# >>> math(mn(2)).xml()
# ['<math>', '\n  ', '<mn>', '2', '</mn>', '\n', '</math>']
#
# >>> math(id='eq3')
# math(id='eq3')
# >>> math(id='eq3').xml()
# ['<math id="eq3">', '\n', '</math>']
#
# use CLASS to get "class" in XML
# >>> math(CLASS='test')
# math(CLASS='test')
# >>> math(CLASS='test').xml()
# ['<math class="test">', '\n', '</math>']

# >>> math(xmlns='http://www.w3.org/1998/Math/MathML').xml()
# ['<math xmlns="http://www.w3.org/1998/Math/MathML">', '\n', '</math>']

class mrow(math):
    """Group sub-expressions as a horizontal row."""

# >>> mrow(displaystyle='false')
# mrow(displaystyle='false')

class mtable(math): pass

# >>> mtable(displaystyle='true')
# mtable(displaystyle='true')
# >>> math(mtable(displaystyle='true')).xml()
# ['<math>', '\n  ', '<mtable displaystyle="true">', '\n  ', '</mtable>', '\n', '</math>']


# The elements <msqrt>, <mstyle>, <merror>, <mpadded>, <mphantom>, <menclose>,
# <mtd>, <mscarry>, and <math> treat their contents as a single inferred mrow
# formed from all their children.
class msqrt(mrow): pass
class mstyle(mrow): pass
class mtr(mrow): pass
class mtd(mrow): pass

class MathToken(math):
    """Token Element: Base class for mo, mi, and mn.
    """
    nchildren = 0
    entity_table = {ord('<'): u'&lt;', ord('>'): u'&gt;'}

    def __init__(self, data, **attributes):
        self.data = data
        super(MathToken, self).__init__(**attributes)

    def xml_body(self):
        return [unicode(self.data).translate(self.entity_table)]

class mi(MathToken): pass
class mn(MathToken): pass
class mo(MathToken): pass
class mtext(MathToken): pass

# >>> mo(u'<')
# mo('<')
# >>> mo(u'<').xml()
# ['<mo>', '&lt;', '</mo>']

class MathScriptOrLimit(math):
    """Base class for script and limit schemata."""
    nchildren = 2

    def __init__(self, *children, **kwargs):
        self.reversed = kwargs.pop('reversed', False)
        math.__init__(self, *children, **kwargs)

    def xml(self):
        if self.reversed:
            self.children.reverse()
            self.reversed = False
        return super(MathScriptOrLimit, self).xml()

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
# >>> munder(mo('-'), mi('lim'), accent='false', reversed=True)
# munder(mo('-'), mi('lim'), reversed=True, accent='false')
# >>> ''.join(munder(mo('-'), mi('lim'), accent='false', reversed=True).xml())
# '<munder accent="false">\n  <mi>lim</mi><mo>-</mo>\n</munder>'
# >>> msub(mi('x'), mo('-'))
# msub(mi('x'), mo('-'))


class mroot(math):
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
    """Return leading TeX command name from `string`.

      >>> tex_cmdname('name2') # up to first non-letter
      ('name', '2')
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

#
# --- https://www.w3.org/TR/MathML3/chapter3.html#id.3.1.3.2

def tex_token(string):
    """Take first simple TeX token from `string`.

    Return token and remainder.

      >>> tex_token('{first simple group} {without brackets}')
      ('first simple group', ' {without brackets}')
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


def parse_latex_math(string, inline=True):
    """parse_latex_math(string [,inline]) -> MathML-tree

    Return a MathML-tree parsed from `string`.
    Set `inline` to False for displayed math.

      >>> parse_latex_math('\\alpha')
      math(mi('α'), xmlns='http://www.w3.org/1998/Math/MathML')

    """

    # Normalize white-space:
    string = ' '.join(string.split())

    # Set up: tree is the whole tree and node is the current element.
    if inline:
        node = math(xmlns='http://www.w3.org/1998/Math/MathML')
        tree = node
    else:
        # block: emulate align* environment with a math table
        node = mtd()
        lines = mtable(mtr(node), displaystyle='true', CLASS='align')
        tree = math(lines, display='block',
                    xmlns='http://www.w3.org/1998/Math/MathML')

    while len(string) > 0:
        # Take of first character:
        c, string = string[0], string[1:]

        if c == ' ':
            continue  # whitespace is ignored in LaTeX math mode

        if c == '\\': # start of a LaTeX macro
            cmdname, string = tex_cmdname(string)
            node, string = handle_keyword(cmdname, node, string)
        elif c.isalpha():
            node = node.append(mi(c))
        elif c.isdigit():
            node = node.append(mn(c))
        elif c in "/()[]|":
            node = node.append(mo(c, stretchy='false'))
        # use dedicated mathematical operator characters
        elif c in anomalous_chars:
            node = node.append(mo(anomalous_chars[c]))
        elif c in "+*=<>,.!?';@":
            node = node.append(mo(c))
        elif c == '_':
            child = node.children.pop()
            if isinstance(child, msup):
                sub = msubsup(*child.children, reversed=True)
            elif isinstance(child, MathToken) and child.data in sumintprod:
                child.attributes['movablelimits'] = 'true'
                sub = munder(child)
            else:
                sub = msub(child)
            node.append(sub)
            node = sub
        elif c == '^':
            child = node.children.pop()
            if isinstance(child, msub):
                sup = msubsup(*child.children)
            elif isinstance(child, MathToken) and child.data in sumintprod:
                child.attributes['movablelimits'] = 'true'
                sup = mover(child)
            elif (isinstance(child, munder) and
                  child.children[0].data in sumintprod):
                sup = munderover(*child.children)
            else:
                sup = msup(child)
            node.append(sup)
            node = sup
        elif c == '{':
            row = mrow()
            node.append(row)
            node = row
        elif c == '}':
            node = node.close()
        elif c == '&':
            entry = mtd()
            node.close().append(entry)
            node = entry
        else:
            raise SyntaxError(u'Illegal character: "%s"' % c)
    return tree


# Test:

# >>> parse_latex_math('', inline=True)
# math(xmlns='http://www.w3.org/1998/Math/MathML')
# >>> parse_latex_math('', inline=False)
# math(mtable(mtr(mtd()), CLASS='align', displaystyle='true'), display='block', xmlns='http://www.w3.org/1998/Math/MathML')
# >>> parse_latex_math(' \\sqrt{ \\alpha}')
# math(msqrt(mi('α')), xmlns='http://www.w3.org/1998/Math/MathML')
# >>> parse_latex_math('\\alpha', inline=False)
# math(mtable(mtr(mtd(mi('α'))), CLASS='align', displaystyle='true'), display='block', xmlns='http://www.w3.org/1998/Math/MathML')
# >>> parse_latex_math('\\sqrt 2 \\ne 3')
# math(msqrt(mn('2')), mo('≠'), mn('3'), xmlns='http://www.w3.org/1998/Math/MathML')
# >>> parse_latex_math('\\sqrt{2 + 3} < 3')
# math(msqrt(mn('2'), mo('+'), mn('3')), mo('<'), mn('3'), xmlns='http://www.w3.org/1998/Math/MathML')

def handle_keyword(name, node, string):
    """Process LaTeX macro `name` followed by `string`.

    If needed, parse `string` for macro argument.
    Return updated current node and remainder:

      >>> handle_keyword('hbar', math(), r' \frac')
      (math(mi('ℏ')), ' \\frac')
      >>> handle_keyword('hspace', math(), r'{1ex} (x)')
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

    if (name in functions or name == 'operatorname'):
        # use <mi> followed by invisible function applicator character
        # (see https://www.w3.org/TR/MathML3/chapter3.html#presm.mi)
        if name == 'operatorname':
            # custom function name ``\operatorname{abs}(x)``
            arg, string = tex_token(string)
            identifier = mi(arg, mathvariant='normal')
        else:
            identifier = mi(functions[name])
        # functions with embellished names
        if name == 'varliminf':    # \underline\lim
            identifier = munder(identifier, mo(u'_'))
        elif name == 'varlimsup':  # \overline\lim
            identifier = mover(identifier, mo(u'¯'), accent='false')
        elif name == 'varprojlim': # \underleftarrow\lim
            identifier = munder(identifier, mo(u'\u2190'))
        elif name == 'varinjlim':  # \underrightarrow\lim
            identifier = munder(identifier, mo(u'\u2192'))

        node = node.append(identifier)
        # TODO: only add ApplyFunction when appropriate (not \sin ^2(x), say)
        node = node.append(mo('&ApplyFunction;')) # '\u2061'
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

    if name in left_delimiters: # opening delimiters
        node = node.append(mo(left_delimiters[name], rspace='0'))
        return node, string

    if name in right_delimiters: # closing delimiters
        node = node.append(mo(right_delimiters[name], lspace='0', ))
        return node, string

    if name == 'colon': # trailing punctuation, not binary relation
        node = node.append(mo(':', lspace='0', rspace='0.28em'))
        return node, string

    if name in thick_operators:
        node = node.append(mo(thick_operators[name], style='font-weight: bold'))
        return node, string

    if name in small_operators:
        node = node.append(mo(small_operators[name], mathsize='75%'))
        return node, string

    if name in sumintprod:
        node = node.append(mo(operators[name]))
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
        if delimiter:
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
        # text = arg.replace(' ', '&nbsp;', 1).replace(' ', '&nbsp;', 1)
        text = re.sub('(^ | $)', '&nbsp;', arg)
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
        sqrt = msqrt()
        if string.startswith('{'): # argument is a group
            string = string[1:] # mrow implied, skip opening bracket
        else: # no group, enclose only one element
            sqrt.nchildren = 1
        node.append(sqrt)
        return sqrt, string

    if name == 'frac':
        frac = mfrac()
        node.append(frac)
        return frac, string

    if name == '\\': # end of a row
        entry = mtd()
        row = mtr(entry)
        node.close().close().append(row)
        return entry, string

    if name in accents or name in over:
        try:
            ch = over[name]
            acc = 'false'
        except KeyError:
            ch = accents[name]
            acc = 'true'
        ovr = mover(mo(ch), reversed=True, accent=acc)
        node.append(ovr)
        return ovr, string

    if name in under:
        ovr = munder(mo(under[name]), reversed=True)
        node.append(ovr)
        return ovr, string

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


# >>> handle_keyword('left', math(), '[a\\right]')
# (mrow(mo('[')), 'a\\right]')
# >>> handle_keyword('left', math(), '. a)') # emtpy \left
# (mrow(), ' a)')
# >>> handle_keyword('left', math(), '\\uparrow a)') # cmd
# (mrow(mo('↑')), 'a)')
# >>> handle_keyword('not', math(), '\\equiv \\alpha)') # cmd
# (math(mo('≢')), '\\alpha)')
# >>> handle_keyword('text', math(), '{ for } i>0') # group
# (math(mtext('&nbsp;for&nbsp;')), ' i>0')
# >>> handle_keyword('text', math(), '{B}T') # group
# (math(mtext('B')), 'T')
# >>> handle_keyword('text', math(), '{number of apples}}') # group
# (math(mtext('number of apples')), '}')
# >>> handle_keyword('text', math(), 'i \\sin(x)') # single char
# (math(mtext('i')), ' \\sin(x)')
# >>> handle_keyword('sin', math(), '(\\alpha)')
# (math(mi('sin'), mo('&ApplyFunction;')), '(\\alpha)')
# >>> handle_keyword('sin', math(), ' \\alpha')
# (math(mi('sin'), mo('&ApplyFunction;')), ' \\alpha')
# >>> handle_keyword('operatorname', math(), '{abs}(x)')
# (math(mi('abs', mathvariant='normal'), mo('&ApplyFunction;')), '(x)')
# >>> handle_keyword('mathrm', math(), '\\alpha')
# (math(mi('α', mathvariant='normal')), '')
# >>> handle_keyword('mathrm', math(), '{out} = 3')
# (math(mi('out', mathvariant='normal')), ' = 3')
# >>> handle_keyword('overline', math(), '{981}')
# (mover(mo('¯'), reversed=True, accent='false'), '{981}')
# >>> handle_keyword('bar', math(), '{x}')
# (mover(mo('ˉ'), reversed=True, accent='true'), '{x}')


def tex2mathml(tex_math, inline=True):
    """Return string with MathML code corresponding to `tex_math`.

    `inline`=True is for inline math and `inline`=False for displayed math.
    """

    mathml_tree = parse_latex_math(tex_math, inline=inline)
    return ''.join(mathml_tree.xml())
