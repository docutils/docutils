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


# Command lists and dictionaries
# ------------------------------

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

greek_capitals = { # Capital Greek letters: (upright in TeX style)
    'Phi':u'\u03a6', 'Xi':u'\u039e', 'Sigma':u'\u03a3',
    'Psi':u'\u03a8', 'Delta':u'\u0394', 'Theta':u'\u0398',
    'Upsilon':u'\u03d2', 'Pi':u'\u03a0', 'Omega':u'\u03a9',
    'Gamma':u'\u0393', 'Lambda':u'\u039b'}

letters = tex2unichar.mathalpha

special = tex2unichar.mathbin         # Binary symbols
special.update(tex2unichar.mathrel)   # Relation symbols, arrow symbols
special.update(tex2unichar.mathord)   # Miscellaneous symbols
special.update(tex2unichar.mathop)    # Variable-sized symbols
special.update(tex2unichar.mathopen)  # Braces
special.update(tex2unichar.mathclose) # Braces
special.update(tex2unichar.mathfence)

sumintprod = ''.join([special[symbol] for symbol in
                      ['sum', 'int', 'oint', 'prod']])

# >>> print(l2m.sumintprod)
# ∑∫∮∏

functions = ['arccos', 'arcsin', 'arctan', 'arg', 'cos',  'cosh',
             'cot',    'coth',   'csc',    'deg', 'det',  'dim',
             'exp',    'gcd',    'hom',    'inf', 'ker',  'lg',
             'lim',    'liminf', 'limsup', 'ln',  'log',  'max',
             'min',    'Pr',     'sec',    'sin', 'sinh', 'sup',
             'tan',    'tanh',   'injlim', 'varinjlim',   'projlim',
             'varlimsup', 'varliminf', 'varprojlim']


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

mathbb = {
          '0': u'\U0001D7D8', # 𝟘
          '1': u'\U0001D7D9', # 𝟙
          '2': u'\U0001D7DA', # 𝟚
          '3': u'\U0001D7DB', # 𝟛
          '4': u'\U0001D7DC', # 𝟜
          '5': u'\U0001D7DD', # 𝟝
          '6': u'\U0001D7DE', # 𝟞
          '7': u'\U0001D7DF', # 𝟟
          '8': u'\U0001D7E0', # 𝟠
          '9': u'\U0001D7E1', # 𝟡
          'A': u'\U0001D538', # 𝔸
          'B': u'\U0001D539', # 𝔹
          'C': u'\u2102',     # ℂ
          'D': u'\U0001D53B', # 𝔻
          'E': u'\U0001D53C', # 𝔼
          'F': u'\U0001D53D', # 𝔽
          'G': u'\U0001D53E', # 𝔾
          'H': u'\u210D',     # ℍ
          'I': u'\U0001D540', # 𝕀
          'J': u'\U0001D541', # 𝕁
          'K': u'\U0001D542', # 𝕂
          'L': u'\U0001D543', # 𝕃
          'M': u'\U0001D544', # 𝕄
          'N': u'\u2115',     # ℕ
          'O': u'\U0001D546', # 𝕆
          'P': u'\u2119',     # ℙ
          'Q': u'\u211A',     # ℚ
          'R': u'\u211D',     # ℝ
          'S': u'\U0001D54A', # 𝕊
          'T': u'\U0001D54B', # 𝕋
          'U': u'\U0001D54C', # 𝕌
          'V': u'\U0001D54D', # 𝕍
          'W': u'\U0001D54E', # 𝕎
          'X': u'\U0001D54F', # 𝕏
          'Y': u'\U0001D550', # 𝕐
          'Z': u'\u2124',     # ℤ
          'a': u'\U0001D552', # 𝕒
          'b': u'\U0001D553', # 𝕓
          'c': u'\U0001D554', # 𝕔
          'd': u'\U0001D555', # 𝕕
          'e': u'\U0001D556', # 𝕖
          'f': u'\U0001D557', # 𝕗
          'g': u'\U0001D558', # 𝕘
          'h': u'\U0001D559', # 𝕙
          'i': u'\U0001D55A', # 𝕚
          'j': u'\U0001D55B', # 𝕛
          'k': u'\U0001D55C', # 𝕜
          'l': u'\U0001D55D', # 𝕝
          'm': u'\U0001D55E', # 𝕞
          'n': u'\U0001D55F', # 𝕟
          'o': u'\U0001D560', # 𝕠
          'p': u'\U0001D561', # 𝕡
          'q': u'\U0001D562', # 𝕢
          'r': u'\U0001D563', # 𝕣
          's': u'\U0001D564', # 𝕤
          't': u'\U0001D565', # 𝕥
          'u': u'\U0001D566', # 𝕦
          'v': u'\U0001D567', # 𝕧
          'w': u'\U0001D568', # 𝕨
          'x': u'\U0001D569', # 𝕩
          'y': u'\U0001D56A', # 𝕪
          'z': u'\U0001D56B', # 𝕫
          u'Γ': u'\u213E',    # ℾ
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
    """
    name = re.match(r'([a-zA-Z]+|.?)', string)
    return name.group(0)

# >>> l2m.tex_cmdname('m2') # first non-letter terminates
# 'm'
# >>> l2m.tex_cmdname('m_2') # first non-letter terminates
# 'm'
# >>> l2m.tex_cmdname('m 2') # first non-letter terminates
# 'm'
# >>> l2m.tex_cmdname('_2') # single non-letter character
# '_'
# >>> l2m.tex_cmdname(' 2') # single non-letter character
# ' '
# >>> l2m.tex_cmdname('') # empty string
# ''

def tex_token(string):
    """Return first simple TeX token from `string`.
    """
    token = re.match(r"""({(\\}|[^{}]|\\{)*} # {group} without nested groups
                          |\\([a-zA-Z]+|.)   # or \cmdname
                          |.?)               # or first character/empty string
                     """, string, re.VERBOSE)
    return token.group(0)

# What is returned?
#
# >>> l2m.tex_token(r'\command{without argument}')
# '\\command'
# >>> l2m.tex_token('\\nor trailing whitespace, or')
# '\\nor'
# >>> l2m.tex_token('{first simple group} or')
# '{first simple group}'
# >>> l2m.tex_token('{opening bracket of group with {nested group}} or')
# '{'
# >>> l2m.tex_token('{group with \\{escaped\\} brackets} or')
# '{group with \\{escaped\\} brackets}'
# >>> l2m.tex_token('first character, or')
# 'f'
# >>> l2m.tex_token('') # empty string
# ''
#
# test:
# >>> l2m.tex_token('{group followed by closing bracket}} from outer group')
# '{group followed by closing bracket}'

def strip_brackets(string):
    """Strip outer brackets from `string`."""
    if string.startswith('{') and string.endswith('}'):
        return string[1:-1]
    return string

# >>> l2m.strip_brackets('{a}')
# 'a'
# >>> l2m.strip_brackets('a')
# 'a'
# >>> l2m.strip_brackets('{a')
# '{a'
# >>> l2m.strip_brackets('{a}}')
# 'a}'


def parse_latex_math(string, inline=True):
    """parse_latex_math(string [,inline]) -> MathML-tree

    Returns a MathML-tree parsed from string.  inline=True is for
    inline math and inline=False is for displayed math.
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
            pass
        elif c == '\\': # start of a LaTeX macro
            name = tex_cmdname(string)
            string = string[len(name):]
            node, skip = handle_keyword(name, node, string)
            string = string[skip:]
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

# >>> l2m.parse_latex_math('\\alpha')
# math(mrow(mi('α')), xmlns='http://www.w3.org/1998/Math/MathML')
# >>> l2m.parse_latex_math(' \\sqrt{ \\alpha}')
# math(mrow(msqrt(mrow(mi('α')))), xmlns='http://www.w3.org/1998/Math/MathML')

def handle_keyword(name, node, string):
    skip = 0
    if string.startswith(' '):
        # remove leading whitespace (already normalized to " "):
        string = string[1:]
        skip = 1
    arg = tex_token(string) # argument: single letter, \cmd, or {group}

    if name == 'begin':
        if string.startswith('{matrix}'):
            entry = mtd()
            table = mtable(mtr(entry))
            node.append(table)
            node = entry
        elif string.startswith('{cases}'):
            entry = mtd()
            cases = mrow([mo('{'), mtable(mtr(entry))])
            node.append(cases)
            node = entry
        else:
            raise SyntaxError(u'Environment not supported! '
                        u'Supported environments: "matrix", "cases".')
        skip += len(arg)
    elif name == 'end':
        if string.startswith('{matrix}'):
            node = node.close().close().close()
        elif string.startswith('{cases}'):
            node = node.close().close().close().close()
        else:
            raise SyntaxError(u'Environment not supported! '
                        u'Supported environments: "matrix", "cases".')
        skip += len(arg)
    elif name in ('text', 'mbox', 'textrm'):
        text = arg.replace('{ ', '&nbsp;', 1).replace(' }', '&nbsp;', 1)
        node = node.append(mtext(strip_brackets(text)))
        skip += len(arg)
    elif name == 'mathrm': # upright identifier
        # https://www.w3.org/TR/MathML3/chapter3.html#presm.mi
        node = node.append(mi(strip_brackets(arg), mathvariant='normal'))
        # node = node.append(mo(strip_brackets(arg)))
        skip += len(arg)
    elif name == 'operatorname':
        # use <mi> (see https://www.w3.org/TR/MathML3/chapter3.html#presm.mi)
        node = node.append(mi(strip_brackets(arg), mathvariant='normal'))
        node = node.append(mo('&ApplyFunction;'))
        skip += len(arg)
    elif name == 'mathbb':
        chs = strip_brackets(arg)
        while chs:
            c = tex_token(chs)
            chs = chs[len(c):]
            try:
                node = node.append(mi(mathbb[c]))
            except KeyError:
                raise SyntaxError(u'Character "%s" not supported '
                                  u'in "\\mathbb{}"!' % c)
        skip += len(arg)
    elif name in math_alphabets.keys():
        n_c = None if string.startswith('{') else 1
        style = mstyle(nchildren=n_c, mathvariant=math_alphabets[name])
        node.append(style)
        node = style
        skip += 1
    elif name == 'sqrt':
        sqrt = msqrt()
        node.append(sqrt)
        node = sqrt
    elif name == 'frac':
        frac = mfrac()
        node.append(frac)
        node = frac
    elif name in '{}':
        node = node.append(mo(name))
    elif name == '\\':
        # End of a row:
        entry = mtd()
        row = mtr(entry)
        node.close().close().append(row)
        node = entry
    elif name in ('left', 'right'):
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
        skip += len(arg)
    elif name == 'not':
        try:
            node = node.append(mo(negatables[strip_brackets(arg)]))
        except KeyError:
            raise SyntaxError(u'Expected something to negate: "\\not ..."!')
        skip += len(arg)
    elif name == 'colon': # "normal" colon, not binary operator
        node = node.append(mo(':', lspace='0', rspace='0.28em'))
    elif name in spaces.keys():
        node = node.append(mspace(width='%s'%spaces[name]))
    elif name == 'hspace':
        node = node.append(mspace(width='%s'%strip_brackets(arg)))
        skip += len(arg)
    elif name in greek_capitals:   # Greek capitals (upright in "TeX style")
        node = node.append(mi(greek_capitals[name], CLASS='capital-greek'))
        # TODO: Use 'mathvariant="normal"'?
        # MathML sets them italic (ISO style).
        # CSS styling does not change the font style in Firefox 78.
    elif name in letters:
        node = node.append(mi(letters[name]))
    elif name in special:
        node = node.append(mo(special[name]))
    elif name in functions:
        # use <mi> (see https://www.w3.org/TR/MathML3/chapter3.html#presm.mi)
        node = node.append(mi(name))
        node = node.append(mo('&ApplyFunction;'))
    elif name in over:
        ovr = mover(mo(over[name]), reversed=True)
        node.append(ovr)
        node = ovr
    else:
        raise SyntaxError(u'Unknown LaTeX command: ' + name)

    return node, skip

# >>> l2m.handle_keyword('left', l2m.math(), '[a\right]')
# (mrow(mo('[')), 1)
# >>> l2m.handle_keyword('left', l2m.math(), '(a)')[0].xml()
# ['<mrow>', '\n  ', '<mo>', '(', '</mo>', '\n', '</mrow>']
# >>> l2m.handle_keyword('left', l2m.math(), '. a)') # emtpy \left
# (mrow(), 1)
# >>> l2m.handle_keyword('left', l2m.math(), r'\uparrow. a)') # cmd
# (mrow(mo('↑')), 8)
# >>> l2m.handle_keyword('not', l2m.math(), r'\equiv a)') # cmd
# (math(mo('≢')), 6)
# >>> l2m.handle_keyword('text', l2m.math(), r'{ for } i>0)') # group
# (math(mtext('&nbsp;for&nbsp;')), 7)
# >>> l2m.handle_keyword('text', l2m.math(), r'{B}T') # group
# (math(mtext('B')), 3)
# >>> l2m.handle_keyword('text', l2m.math(), r'{number of apples}}') # group
# (math(mtext('number of apples')), 18)
# >>> l2m.handle_keyword('text', l2m.math(), r'i \sin(x)') # single char
# (math(mtext('i')), 1)
# >>> l2m.handle_keyword('operatorname', l2m.math(), r'{abs}(x)')
# (math(mi('abs', mathvariant='normal'), mo('&ApplyFunction;')), 5)
# >>> l2m.handle_keyword('hspace', l2m.math(), r'{1ex} (x)')
# (math(mspace(width='1ex')), 5)
# >>> l2m.handle_keyword('not', l2m.math(), r'{=} x')
# (math(mo('≠')), 3)

def tex2mathml(tex_math, inline=True):
    """Return string with MathML code corresponding to `tex_math`.

    `inline`=True is for inline math and `inline`=False for displayed math.
    """

    mathml_tree = parse_latex_math(tex_math, inline=inline)
    return ''.join(mathml_tree.xml())
