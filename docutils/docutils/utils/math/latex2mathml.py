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
# >>> import latex2mathml as l2m

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
             'lim',    'liminf', 'limsup', 'ln',  'log',  'max',
             'min',    'Pr',     'sec',    'sin', 'sinh', 'sup',
             'tan',    'tanh',   'injlim', 'varinjlim',   'projlim',
             'varlimsup', 'varliminf', 'varprojlim']

# font selection -> <mi mathvariant=...>
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

# blackboar bold (Greek characters not working with "mathvariant" (Firefox 78)
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

# operator, fence, or separator -> <mo>

operators = tex2unichar.mathbin         # Binary symbols
operators.update(tex2unichar.mathrel)   # Relation symbols, arrow symbols
operators.update(tex2unichar.mathord)   # Miscellaneous symbols
operators.update(tex2unichar.mathop)    # Variable-sized symbols
operators.update(tex2unichar.mathopen)  # Braces
operators.update(tex2unichar.mathclose) # Braces
operators.update(tex2unichar.mathfence)

# >>> '{' in l2m.operators.values()
# True

# special cases

sumintprod = ''.join([operators[symbol] for symbol in
                      ['sum', 'int', 'oint', 'prod']])

# >>> print(l2m.sumintprod)
# ∑∫∮∏

#
negatables = {'=': u'\u2260',
              r'\in': u'\u2209',
              r'\equiv': u'\u2262'}

# cmds/characters allowed in left/right cmds
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

# >>> print(' '.join(sorted(set(l2m.stretchables.values()))))
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

# accents -> <mover>
#        TeX        spacing    combining
over = {'acute':    u'\u00B4', # u'\u0301',
        'bar':      u'\u00AF', # u'\u0304',
        'breve':    u'\u02D8', # u'\u0306',
        'check':    u'\u02C7', # u'\u030C',
        'dot':      u'\u02D9', # u'\u0307',
        'ddot':     u'\u00A8', # u'\u0308',
        'dddot':               u'\u20DB',
        'grave':    u'`',      # u'\u0300',
        'hat':      u'^',      # u'\u0302',
        'mathring': u'\u02DA', # u'\u030A',
        'overleftrightarrow':  u'\u20e1',
        # 'overline':        # u'\u0305',
        'tilde':    u'\u02DC', # u'\u0303',
        'vec':               u'\u20D7'}


# all supported math-characters:
#mathcharacters = dict(letters)
#mathcharacters.update(operators)
#mathcharacters.update(tex2unichar.space)
#
## >>> l2m.mathcharacters['alpha']
## 'α'
## >>> l2m.mathcharacters['{']
## '{'
## >>> l2m.mathcharacters['pm']
## '±'

# MathML element classes
# ----------------------

class math(object):
    """Base class for MathML elements."""

    nchildren = 1000000
    """Required/Supported number of children"""
    _level = 0 # indentation level (static class variable)

    def __init__(self, children=None, inline=None, **kwargs):
        """math([children]) -> MathML element

        children can be one child or a list of children."""

        self.children = []
        if children is not None:
            if not isinstance(children, list):
                children = [children]
            for child in children:
                self.append(child)

        self.attributes = collections.OrderedDict()
        if inline is not None:
            self.attributes['xmlns'] = 'http://www.w3.org/1998/Math/MathML'
        if inline is False:
            self.attributes['display'] = 'block'
            # self.attributes['displaystyle'] = 'true'
        # sort kwargs for predictable functional tests
        # as self.attributes.update(kwargs) does not keep order in Python < 3.6
        for key in sorted(kwargs.keys()):
            self.attributes.setdefault(key, kwargs[key])

    def __repr__(self):
        content = [repr(item) for item in getattr(self, 'children', [])]
        if hasattr(self, 'data'):
            content.append(repr(self.data))
        if hasattr(self, 'attributes'):
            content += ["%s='%s'"%(k, v) for k, v in self.attributes.items()]
        return self.__class__.__name__ + '(%s)' % ', '.join(content)

    def full(self):
        """Room for more children?"""

        return len(self.children) >= self.nchildren

    def append(self, child):
        """append(child) -> element

        Appends child and returns self if self is not full or first
        non-full parent."""

        assert not self.full()
        self.children.append(child)
        child.parent = self
        node = self
        while node.full():
            node = node.parent
        return node

    def delete_child(self):
        """delete_child() -> child

        Delete last child and return it."""

        child = self.children[-1]
        del self.children[-1]
        return child

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
        if not isinstance(self, mx): # token elements
            math._level += 1
        return ['<%s>' % ' '.join([self.__class__.__name__] + attrs)]

    def xml_end(self):
        xml = []
        if not isinstance(self, mx): # except token elements
            math._level -= 1
            xml.append('\n' + '  ' * math._level)
        xml.append('</%s>' % self.__class__.__name__)
        return xml

    def xml_body(self):
        xml = []
        last_child = None
        for child in self.children:
            if not (isinstance(last_child, mx) and isinstance(child, mx)):
                xml.append('\n' + '  ' * math._level)
            xml.extend(child.xml())
            last_child = child
        return xml

# >>> l2m.math(l2m.mn(2))
# math(mn(2))
# >>> l2m.math(l2m.mn(2)).xml()
# ['<math>', '\n  ', '<mn>', '2', '</mn>', '\n', '</math>']
#
# >>> l2m.math(id='eq3')
# math(id='eq3')
# >>> l2m.math(id='eq3').xml()
# ['<math id="eq3">', '\n', '</math>']
#
# use CLASS to get "class" in XML
# >>> l2m.math(CLASS='test')
# math(CLASS='test')
# >>> l2m.math(CLASS='test').xml()
# ['<math class="test">', '\n', '</math>']

# >>> l2m.math(inline=True)
# math(xmlns='http://www.w3.org/1998/Math/MathML')
# >>> l2m.math(inline=True).xml()
# ['<math xmlns="http://www.w3.org/1998/Math/MathML">', '\n', '</math>']
# >>> l2m.math(inline=False)
# math(xmlns='http://www.w3.org/1998/Math/MathML', display='block')
# >>> l2m.math(inline=False).xml()
# ['<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">', '\n', '</math>']

class mrow(math): pass

# >>> l2m.mrow(displaystyle='false')
# mrow(displaystyle='false')

class mtable(math): pass

# >>> l2m.mtable(displaystyle='true')
# mtable(displaystyle='true')
# >>> l2m.math(l2m.mtable(displaystyle='true')).xml()
# ['<math>', '\n  ', '<mtable displaystyle="true">', '\n  ', '</mtable>', '\n', '</math>']
class mtr(mrow): pass
class mtd(mrow): pass

class mx(math):
    """Token Element: Base class for mo, mi, and mn.
    """
    nchildren = 0
    entity_table = {ord('<'): u'&lt;', ord('>'): u'&gt;'}

    def __init__(self, data, **kwargs):
        self.data = data
        super(mx, self).__init__(**kwargs)

    def xml_body(self):
        return [unicode(self.data).translate(self.entity_table)]

class mi(mx): pass
class mn(mx): pass
class mo(mx): pass
class mtext(mx): pass

# >>> l2m.mo(u'<')
# mo('<')
# >>> l2m.mo(u'<').xml()
# ['<mo>', '&lt;', '</mo>']

class msub(math):
    nchildren = 2

class msup(math):
    nchildren = 2

class msqrt(math):
    nchildren = 1

class mroot(math):
    nchildren = 2

class mfrac(math):
    nchildren = 2

class msubsup(math):
    nchildren = 3
    def __init__(self, children=None, reversed=False):
        self.reversed = reversed
        math.__init__(self, children)

    def xml(self):
        if self.reversed:
            ## self.children[1:3] = self.children[2:0:-1]
            self.children[1:3] = [self.children[2], self.children[1]]
            self.reversed = False
        return super(msubsup, self).xml()

class mspace(math):
    nchildren = 0

class mstyle(math):
    def __init__(self, children=None, nchildren=None, **kwargs):
        if nchildren is not None:
            self.nchildren = nchildren
        math.__init__(self, children)
        self.attributes = kwargs

class mover(math):
    nchildren = 2
    def __init__(self, children=None, reversed=False):
        self.reversed = reversed
        math.__init__(self, children)

    def xml(self):
        if self.reversed:
            self.children.reverse()
            self.reversed = False
        return math.xml(self)

class munder(math):
    nchildren = 2

class munderover(math):
    nchildren = 3
    def __init__(self, children=None):
        math.__init__(self, children)


# LaTeX to MathML translation
# ---------------------------

# auxiliary functions
# ~~~~~~~~~~~~~~~~~~~

def tex_cmdname(string):
    """Return leading TeX command name from `string`.

      >>> l2m.tex_cmdname('name2') # up to first non-letter
      ('name', '2')
      >>> l2m.tex_cmdname('name 2') # strip trailing whitespace
      ('name', '2')
      >>> l2m.tex_cmdname('_2') # single non-letter character
      ('_', '2')

    """
    m = re.match(r'([a-zA-Z]+) *(.*)', string)
    if m is None:
        m = re.match(r'(.?)(.*)', string)
    return m.group(1), m.group(2)

# Test:
#
# >>> l2m.tex_cmdname('name_2') # first non-letter terminates
# ('name', '_2')
# >>> l2m.tex_cmdname(' next') # leading whitespace is returned
# (' ', 'next')
# >>> l2m.tex_cmdname('1 2') # whitespace after non-letter is kept
# ('1', ' 2')
# >>> l2m.tex_cmdname('') # empty string
# ('', '')


# TODO: check for Inferred <mrow>s:

# The elements <msqrt>, <mstyle>, <merror>, <mpadded>, <mphantom>, <menclose>,
# <mtd, mscarry>, and <math> treat their contents as a single inferred mrow
# formed from all their children
#
# --- https://www.w3.org/TR/MathML3/chapter3.html#id.3.1.3.2

def tex_token(string):
    """Take first simple TeX token from `string`.

    Return token and remainder.

      >>> l2m.tex_token('{first simple group} {without brackets}')
      ('first simple group', ' {without brackets}')
      >>> l2m.tex_token('\\command{without argument}')
      ('\\command', '{without argument}')
      >>> l2m.tex_token(' first non-white character')
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
# >>> l2m.tex_token('{opening bracket of group with {nested group}}')
# ('{', 'opening bracket of group with {nested group}}')
# >>> l2m.tex_token('{group with \\{escaped\\} brackets}')
# ('group with \\{escaped\\} brackets', '')
# >>> l2m.tex_token('{group followed by closing bracket}} from outer group')
# ('group followed by closing bracket', '} from outer group')
# >>> l2m.tex_token(' {skip leading whitespace}')
# ('skip leading whitespace', '')
# >>> l2m.tex_token('  \\skip{leading whitespace}')
# ('\\skip', '{leading whitespace}')
# >>> l2m.tex_token('\\skip whitespace after macro name')
# ('\\skip', 'whitespace after macro name')
# >>> l2m.tex_token('') # empty string.
# ('', '')


def parse_latex_math(string, inline=True):
    """parse_latex_math(string [,inline]) -> MathML-tree

    Return a MathML-tree parsed from `string`.
    Set `inline` to False for displayed math.

      >>> l2m.parse_latex_math('\\alpha')
      math(mrow(mi('α')), xmlns='http://www.w3.org/1998/Math/MathML')

    """

    # Normalize white-space:
    string = ' '.join(string.split())

    # Set up: tree is the whole tree and node is the current element.
    if inline:
        node = mrow()
        tree = math(node, inline=True)
    else:
        # block: emulate align* environment with a math table
        node = mtd()
        content = mtable(mtr(node), displaystyle='true', CLASS='align')
        tree = math(content, inline=False)

    while len(string) > 0:
        # Take of first character:
        c, string = string[0], string[1:]

        if c == ' ':
            continue

        if c == '\\': # start of a LaTeX macro
            cmdname, string = tex_cmdname(string)
            node, string = handle_keyword(cmdname, node, string)
        elif c.isalpha():
            node = node.append(mi(c))
        elif c.isdigit():
            node = node.append(mn(c))
        elif c in "/()[]|":
            node = node.append(mo(c, stretchy='false'))
        elif c in "+-*=<>,.!?':;@":
            node = node.append(mo(c))
        elif c == '_':
            child = node.delete_child()
            if isinstance(child, msup):
                sub = msubsup(child.children, reversed=True)
            elif isinstance(child, mo) and child.data in sumintprod:
                sub = munder(child)
            else:
                sub = msub(child)
            node.append(sub)
            node = sub
        elif c == '^':
            child = node.delete_child()
            if isinstance(child, msub):
                sup = msubsup(child.children)
            elif isinstance(child, mo) and child.data in sumintprod:
                sup = mover(child)
            elif (isinstance(child, munder) and
                  child.children[0].data in sumintprod):
                sup = munderover(child.children)
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

# >>> l2m.parse_latex_math(' \\sqrt{ \\alpha}')
# math(mrow(msqrt(mrow(mi('α')))), xmlns='http://www.w3.org/1998/Math/MathML')
# >>> l2m.parse_latex_math('\\alpha', inline=False)
# math(mtable(mtr(mtd(mi('α'))), CLASS='align', displaystyle='true'), xmlns='http://www.w3.org/1998/Math/MathML', display='block')


def handle_keyword(name, node, string):
    """Process LaTeX macro `name` followed by `string`.

    If needed, parse `string` for macro argument.
    Return updated current node and remainder:

      >>> l2m.handle_keyword('hbar', l2m.math(), r' \frac')
      (math(mi('ℏ')), ' \\frac')
      >>> l2m.handle_keyword('hspace', l2m.math(), r'{1ex} (x)')
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

    if name in functions:
        # use <mi> followed by invisible function applicator character
        # (see https://www.w3.org/TR/MathML3/chapter3.html#presm.mi)
        node = node.append(mi(name))
        node = node.append(mo('&ApplyFunction;'))
        return node, string

    if name == 'operatorname':
        # custom function name ``\operatorname{abs}(x)``
        arg, string = tex_token(string)
        node = node.append(mi(arg, mathvariant='normal'))
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
            kwargs = {'style': 'font-weight: bold'}
        else:
            kwargs = {'mathvariant': math_alphabets[name]}

        # one symbol (single letter, name, or number)
        if arg.isalpha():
            node = node.append(mi(arg, **kwargs))
            return node, remainder
        if arg.replace('.', '').replace(',', '').isdigit():
            node = node.append(mn(arg, **kwargs))
            return node, remainder
        if len(arg) == 1 and arg != '{':
            node = node.append(mo(arg, **kwargs))
            return node, remainder

        # Wrap in <style>
        style = mstyle(**kwargs)
        node.append(style)
        return style, string[1:] # take of the opening '{', <mrow> is inferred


    # operator, fence, or separator  ->  <mo>

    if name == 'colon': # "normal" colon, not binary operator
        node = node.append(mo(':', lspace='0', rspace='0.28em'))
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
            raise SyntaxError(u'Expected something to negate: "\\not ..."!')
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
        node = entry
        return node, string

    if name in over:
        ovr = mover(mo(over[name]), reversed=True)
        node.append(ovr)
        node = ovr
        return node, string

    if name == 'begin':
        env_name, string = tex_token(string)
        if env_name == 'matrix':
            entry = mtd()
            table = mtable(mtr(entry))
            node.append(table)
            node = entry
        elif env_name == 'cases':
            entry = mtd()
            cases = mrow([mo('{'), mtable(mtr(entry))])
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


# >>> l2m.handle_keyword('left', l2m.math(), '[a\\right]')
# (mrow(mo('[')), 'a\\right]')
# >>> l2m.handle_keyword('left', l2m.math(), '. a)') # emtpy \left
# (mrow(), ' a)')
# >>> l2m.handle_keyword('left', l2m.math(), '\\uparrow a)') # cmd
# (mrow(mo('↑')), 'a)')
# >>> l2m.handle_keyword('not', l2m.math(), '\\equiv \\alpha)') # cmd
# (math(mo('≢')), '\\alpha)')
# >>> l2m.handle_keyword('text', l2m.math(), '{ for } i>0') # group
# (math(mtext('&nbsp;for&nbsp;')), ' i>0')
# >>> l2m.handle_keyword('text', l2m.math(), '{B}T') # group
# (math(mtext('B')), 'T')
# >>> l2m.handle_keyword('text', l2m.math(), '{number of apples}}') # group
# (math(mtext('number of apples')), '}')
# >>> l2m.handle_keyword('text', l2m.math(), 'i \\sin(x)') # single char
# (math(mtext('i')), ' \\sin(x)')
# >>> l2m.handle_keyword('sin', l2m.math(), '(\\alpha)')
# (math(mi('sin'), mo('&ApplyFunction;')), '(\\alpha)')
# >>> l2m.handle_keyword('sin', l2m.math(), ' \\alpha')
# (math(mi('sin'), mo('&ApplyFunction;')), ' \\alpha')
# >>> l2m.handle_keyword('operatorname', l2m.math(), '{abs}(x)')
# (math(mi('abs', mathvariant='normal'), mo('&ApplyFunction;')), '(x)')
# >>> l2m.handle_keyword('mathrm', l2m.math(), '\\alpha')
# (math(mi('α', mathvariant='normal')), '')
# >>> l2m.handle_keyword('mathrm', l2m.math(), '{out} = 3')
# (math(mi('out', mathvariant='normal')), ' = 3')

def tex2mathml(tex_math, inline=True):
    """Return string with MathML code corresponding to `tex_math`.

    `inline`=True is for inline math and `inline`=False for displayed math.
    """

    mathml_tree = parse_latex_math(tex_math, inline=inline)
    return ''.join(mathml_tree.xml())
