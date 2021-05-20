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
             'tan',    'tanh',
             'injlim',  'varinjlim', 'varlimsup',
             'projlim', 'varliminf', 'varprojlim']


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
         }

mathscr = {
           'A': u'\U0001D49C',
           'B': u'\u212C',     # bernoulli function
           'C': u'\U0001D49E',
           'D': u'\U0001D49F',
           'E': u'\u2130',
           'F': u'\u2131',
           'G': u'\U0001D4A2',
           'H': u'\u210B',     # hamiltonian
           'I': u'\u2110',
           'J': u'\U0001D4A5',
           'K': u'\U0001D4A6',
           'L': u'\u2112',     # lagrangian
           'M': u'\u2133',     # physics m-matrix
           'N': u'\U0001D4A9',
           'O': u'\U0001D4AA',
           'P': u'\U0001D4AB',
           'Q': u'\U0001D4AC',
           'R': u'\u211B',
           'S': u'\U0001D4AE',
           'T': u'\U0001D4AF',
           'U': u'\U0001D4B0',
           'V': u'\U0001D4B1',
           'W': u'\U0001D4B2',
           'X': u'\U0001D4B3',
           'Y': u'\U0001D4B4',
           'Z': u'\U0001D4B5',
           'a': u'\U0001D4B6',
           'b': u'\U0001D4B7',
           'c': u'\U0001D4B8',
           'd': u'\U0001D4B9',
           'e': u'\u212F',
           'f': u'\U0001D4BB',
           'g': u'\u210A',
           'h': u'\U0001D4BD',
           'i': u'\U0001D4BE',
           'j': u'\U0001D4BF',
           'k': u'\U0001D4C0',
           'l': u'\U0001D4C1',
           'm': u'\U0001D4C2',
           'n': u'\U0001D4C3',
           'o': u'\u2134',     # order of
           'p': u'\U0001D4C5',
           'q': u'\U0001D4C6',
           'r': u'\U0001D4C7',
           's': u'\U0001D4C8',
           't': u'\U0001D4C9',
           'u': u'\U0001D4CA',
           'v': u'\U0001D4CB',
           'w': u'\U0001D4CC',
           'x': u'\U0001D4CD',
           'y': u'\U0001D4CE',
           'z': u'\U0001D4CF',
          }

# >>> print(''.join(l2m.mathscr.values()))
# 𝒜ℬ𝒞𝒟ℰℱ𝒢ℋℐ𝒥𝒦ℒℳ𝒩𝒪𝒫𝒬ℛ𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵𝒶𝒷𝒸𝒹ℯ𝒻ℊ𝒽𝒾𝒿𝓀𝓁𝓂𝓃ℴ𝓅𝓆𝓇𝓈𝓉𝓊𝓋𝓌𝓍𝓎𝓏

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
    token = re.match(r"""({([^{]|\\{)*}       # {group} without nested groups
                          |\\([a-zA-Z]+|.)    # or \cmdname
                          |.?)                # or first character or empty
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
        n = len(string)
        c = string[0]
        skip = 1  # number of characters consumed
            
        if c == ' ':
            pass
        elif c == '\\':
            if n > 1:
                c2 = string[1]
            else:
                c2 = ''
            if c2 in '{}':
                node = node.append(mo(c2))
                skip = 2
            elif c2 == ' ':
                node = node.append(mspace())
                skip = 2
            elif c2 == ',': # TODO: small space
                node = node.append(mspace())
                skip = 2
            elif c2.isalpha():
                # We have a LaTeX-name:
                i = 2
                while i < n and string[i].isalpha():
                    i += 1
                name = string[1:i]
                node, skip = handle_keyword(name, node, string[i:])
                skip += i
            elif c2 == '\\':
                # End of a row:
                entry = mtd()
                row = mtr(entry)
                node.close().close().append(row)
                node = entry
                skip = 2
            else:
                raise SyntaxError(u'Syntax error: "%s%s"' % (c, c2))
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
        string = string[skip:]
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
        
    if name == 'begin':
        if string.startswith('{matrix}'):
            skip += 8
            entry = mtd()
            table = mtable(mtr(entry))
            node.append(table)
            node = entry
        elif string.startswith('{cases}'):
            skip += 7
            entry = mtd()
            cases = mrow([mo('{'), mtable(mtr(entry))])
            node.append(cases)
            node = entry
        else:
            raise SyntaxError(u'Environment not supported! '
                        u'Supported environments: "matrix", "cases".')
    elif name == 'end':
        if string.startswith('{matrix}'):
            skip += 8
            node = node.close().close().close()
        elif string.startswith('{cases}'):
            skip += 7
            node = node.close().close().close().close()
        else:
            raise SyntaxError(u'Environment not supported! '
                        u'Supported environments: "matrix", "cases".')
    elif name in ('text', 'mbox', 'mathrm'):
        i = string.find('}')
        if string[0] != '{' or i == -1:
            raise SyntaxError(u'Expected "\\%s{...}"!'%name)
        node = node.append(mtext(string[1:i]))
        skip += i + 1
    elif name == 'sqrt':
        sqrt = msqrt()
        node.append(sqrt)
        node = sqrt
    elif name == 'frac':
        frac = mfrac()
        node.append(frac)
        node = frac
    elif name in ('left', 'right'):
        arg = tex_token(string)
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
        arg = tex_token(string)
        try:
            node = node.append(mo(negatables[arg.rstrip()]))
        except KeyError:
            raise SyntaxError(u'Expected something to negate: "\\not ..."!')
        skip += len(arg)
    elif name == 'mathbf':
        if string.startswith('{'):
            nchildren = None
        else:
            nchildren = 1
        style = mstyle(nchildren=nchildren, mathvariant='bold')
        node.append(style)
        node = style
        skip += 1
    elif name == 'mathbb':
        i = string.find('}')
        if string[0] != '{' or i == -1:
            raise SyntaxError(u'Expected "\\mathbb{...}"!')
        try:
            bbchars = ''.join(mathbb[c] for c in string[1:i])
        except KeyError:
            raise SyntaxError(u'Character "%s" not supportd in "\\mathbb{}"!'
                             %[c for c in string[1:i] if c not in mathbb][0])
        node = node.append(mi(bbchars))
        skip += i + 1
    elif name in ('mathscr', 'mathcal'):
        i = string.find('}')
        if string[0] != '{' or i == -1:
            raise SyntaxError(u'Expected "\\%s{...}"!'%name)
        try:
            scrchars = ''.join(mathscr[c] for c in string[1:i])
        except KeyError:
            raise SyntaxError(u'Character "%s" not supportd in "\\%s{}"!'
                    %([c for c in string[1:i] if c not in mathscr][0], name))
        node = node.append(mi(scrchars))
        skip += i + 1
    elif name == 'colon': # "normal" colon, not binary operator
        node = node.append(mo(':')) # TODO: add ``lspace="0pt"``
    elif name in greek_capitals:   # Greek capitals (upright in "TeX style")
        node = node.append(mi(greek_capitals[name], mathvariant='normal'))
        # TODO: "ISO style" sets them italic. Could we use a class argument?
        # Unfortunately CSS styling does not change the font style in Firefox 78
    elif name in letters:
        node = node.append(mi(letters[name]))
    elif name in special:
        node = node.append(mo(special[name]))
    elif name in functions:
        node = node.append(mo(name))
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
# >>> l2m.handle_keyword('text', l2m.math(), r'{for} i \in S)') # cmd
# (math(mtext('for')), 5)


def tex2mathml(tex_math, inline=True):
    """Return string with MathML code corresponding to `tex_math`.

    `inline`=True is for inline math and `inline`=False for displayed math.
    """

    mathml_tree = parse_latex_math(tex_math, inline=inline)
    return ''.join(mathml_tree.xml())
