#!/usr/bin/env python

"""
A minimal front end to the Docutils Publisher, producing HTML + MathML.
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.parsers.rst.roles import register_canonical_role
from docutils.nodes import Inline, Text, TextElement
from docutils.writers.html4css1 import HTMLTranslator
from docutils.parsers.rst.directives import _directives
from docutils.core import publish_cmdline, default_description


# Define LaTeX math node:
class latex_math(Inline, Text):
    tagname = '#latex-math'
    def __init__(self, rawsource, latex):
        Text.__init__(self, latex, rawsource)


# Register role:
def latex_math_role(role, rawtext, text, lineno, inliner,
                    options={}, content=[]):
    i = rawtext.find('`')
##    node = nodes.latex_math(rawtext, text.replace('\x00','\\'))
    node = latex_math(rawtext, rawtext[i+1:-1])
    return [node], []
register_canonical_role('latex-math', latex_math_role)


# Register directive:
def latex_math_directive(name, arguments, options, content, lineno,
                         content_offset, block_text, state, state_machine):
    node = latex_math(block_text, ''.join(content))
    return [node]
latex_math_directive.arguments = None
latex_math_directive.options = {}
latex_math_directive.content = 1
_directives['latex-math'] = latex_math_directive


# Add visit/depart methods to HTML-Translator:
def visit_latex_math(self, node):
    text = node.astext()
    inline = isinstance(node.parent, TextElement)
    mml = mathml(text, inline)
    self.body.append(mml)
    if not self.has_mathml_dtd:
        doctype = ('<!DOCTYPE html'
                   ' PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN"'
                   ' "http://www.w3.org/Math/DTD/mathml2/'
                   'xhtml-math11-f.dtd">\n')
        if self.settings.xml_declaration:
            self.head_prefix[1] = doctype
        else:
            self.head_prefix[0] = doctype
        self.has_mathml_dtd = True
def depart_latex_math(self, node):
    pass
HTMLTranslator.visit_latex_math = visit_latex_math
HTMLTranslator.depart_latex_math = depart_latex_math
HTMLTranslator.has_mathml_dtd = False


# LaTeX to MathML translation stuff:
class math:
    """Base class for MathML elements."""
    
    nchildren = 1000000
    """Required number of children"""
    
    def __init__(self, children=None):
        """math([children]) -> MathML element

        children can be one child or a list of children."""
        
        self.children = []
        if children is not None:
            if type(children) is list:
                for child in children:
                    self.append(child)
            else:
                # Only one child:
                self.append(children)

    def __repr__(self):
        if hasattr(self, 'children'):
            return self.__class__.__name__ + '(%s)' % \
                   ','.join([repr(child) for child in self.children])
        else:
            return self.__class__.__name__

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
        return ['<%s>' % self.__class__.__name__]

    def xml_end(self):
        return ['</%s>' % self.__class__.__name__]
    
    def xml_body(self):
        xml = []
        for child in self.children:
            xml.extend(child.xml())
        return xml

class mrow(math): pass
class mtable(math): pass
class mtr(mrow): pass
class mtd(mrow): pass

class mx(math):
    """Base class for mo, mi, and mn"""
    
    nchildren = 0
    def __init__(self, data):
        self.data = data

    def xml_body(self):
        return [self.data]

class mo(mx):
    translation = {'<': '&lt;', '>': '&gt;'}
    def xml_body(self):
        return [self.translation.get(self.data, self.data)]
        
class mi(mx): pass
class mn(mx): pass

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
##            self.children[1:3] = self.children[2:0:-1]
            self.children[1:3] = [self.children[2], self.children[1]]
            self.reversed = False
        return math.xml(self)
        
class mfenced(math):
    translation = {'\\{': '{', '\\langle': u'\u2329',
                   '\\}': '}', '\\rangle': u'\u232A',
                   '.': ''}
    def __init__(self, par):
        self.openpar = par
        math.__init__(self)

    def xml_start(self):
        open = self.translation.get(self.openpar, self.openpar)
        close = self.translation.get(self.closepar, self.closepar)
        return ['<mfenced open="%s" close="%s">' % (open, close)]

class mspace(math):
    nchildren = 0

class mstyle(math):
    def __init__(self, children=None, nchildren=None, **kwargs):
        if nchildren is not None:
            self.nchildren = nchildren
        math.__init__(self, children)
        self.attrs = kwargs
        
    def xml_start(self):
        return ['<mstyle '] + ['%s="%s"' % item
                               for item in self.attrs.items()] + ['>']

class mover(math):
    nchildren = 2
    reversed = True
    def xml(self):
        if self.reversed:
            self.children.reverse()
            self.reversed = False
        return math.xml(self)

class mtext(math):
    nchildren = 0
    def __init__(self, text):
        self.text = text

    def xml_body(self):
        return [self.text]


escaped = {'{': 'lbrace',
           '}': 'rbrace'}

over = {'tilde': '~',
        'hat': '^',
        'bar': '_'}

special = {'sum': u'\u03A3',
           'int': u'\u222B',
           'prod': u'\u03A0',
           'leftarraw': u'\u00D7',
           'rightarrow': u'\u00D7',
           'pm': u'\u00D7',
           'mp': u'\u00D7',
           'cdot': u'\u00D7',
           'leq': u'\u00D7',
           'geq': u'\u00D7',
           'cdots': u'\u00D7',
           'infty': u'\u00D7',
           'hbar': u'\u00D7',
           'uparrow': u'\u00D7',
           'downarrow': u'\u00D7',
           'nabla': u'\u2207',
           'times': u'\u00D7',
           # Greek letters:
           'alpha': u'\u03B1', 'beta': u'\u03B2', 'gamma': u'\u03B3',
           'delta': u'\u03B4', 'epsilon': u'\u03F5', 'varepsilon': u'\u03B5',
           'zeta': u'\u03B6',
           'eta': u'\u03B7',
           'theta': u'\u03B8',
           'vartheta': u'\u03D1',
           'iota': u'\u03B9',
           'kappa': u'\u03BA',
           'varkappa': u'\u03F0',
           'lambda': u'\u03BB',
           'mu': u'\u03BC',
           'nu': u'\u03BD',
           'xi': u'\u03BE',
           'pi': u'\u03C0',
           'varpi': u'\u03D6',
           'rho': u'\u03C1',
           'varrho': u'\u03F1',
           'sigma': u'\u03C3',
           'varsigma': u'\u03C2',
           'tau': u'\u03C4',
           'upsilon': u'\u03C5',
           'phi': u'\u03D5',
           'varphi': u'\u03C6', 
           'chi': u'\u03C7',
           'psi': u'\u03C8', 
           'omega': u'\u03C9', 
           'Gamma': u'\u0393',
           'Delta': u'\u0394',
           'Theta': u'\u0398',
           'Lambda': u'\u039B',
           'Xi': u'\u039E',
           'Pi': u'\u03A0',
           'Sigma': u'\u03A3',
           'Upsilon': u'\u03D2',
           'Phi': u'\u03A6',
           'Psi': u'\u03A8',
           'Omega': u'\u03A9',
           # Braces:
           'lbrace': u'\u007B',
           'rbrace': u'\u007D',
           'langle': u'\u2329',
           'rangle': u'\u232A'}


functions = ['arccos', 'arcsin', 'arctan', 'arg', 'cos',  'cosh',
             'cot',    'coth',   'csc',    'deg', 'det',  'dim',
             'exp',    'gcd',    'hom',    'inf', 'ker',  'lg',
             'lim',    'liminf', 'limsup', 'ln',  'log',  'max',
             'min',    'Pr',     'sec',    'sin', 'sinh', 'sup',
             'tan',    'tanh',
             'injlim',  'varinjlim', 'varlimsup',
             'projlim', 'varliminf', 'varprojlim']

def parse_latex_math(string, inline=True):
    """parse_latex_math(string [,inline]) -> MathML-tree

    Returns a MathML-tree parsed from string.  inline=True is for
    inline math and inline=False is for displayed math.

    tree is the whole tree and node is the current element."""
    
    # Normalize white-space:
    string = ' '.join(string.split())

    if inline:
        node = mrow()
        tree = node
    else:
        node = mtd()
        tree = mstyle(mtable(mtr(node)), displaystyle='true')

    while len(string) > 0:
        n = len(string)
        c = string[0]
        skip = 1  # number of characters consumed
        if n > 1:
            c2 = string[1]
        else:
            c2 = ''
##        print n, string, c, c2, node.__class__.__name__
        if c == ' ':
            pass
        elif c == '\\':
            if c2 in escaped:
                node = node.append(mo(escaped[c2]))
                skip = 2
            elif c2 == ' ':
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
                raise SyntaxError
        elif c.isalpha():
            node = node.append(mi(c))
        elif c.isdigit():
            node = node.append(mn(c))
        elif c in '+-/()[]|=<>,.':
            node = node.append(mo(c))
        elif c == '_':
            child = node.delete_child()
            if isinstance(child, msup):
                sub = msubsup(child.children[0:2], reversed=True)
            else:
                sub = msub(child)
            node.append(sub)
            node = sub
        elif c == '^':
            child = node.delete_child()
            if isinstance(child, msub):
                sup = msubsup(child.children[0:2])
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
            raise SyntaxError
        string = string[skip:]
    return tree


mathbb = {'A': u'\U0001D538',
          'B': u'\U0001D539',
          'C': u'\u2102',
          'D': u'\U0001D53B',
          'E': u'\U0001D53C', 
          'F': u'\U0001D53D',
          'G': u'\U0001D53E',
          'H': u'\u210D',
          'I': u'\U0001D540',
          'J': u'\U0001D541',
          'K': u'\U0001D542',
          'L': u'\U0001D543',
          'M': u'\U0001D544',
          'N': u'\u2115',
          'O': u'\U0001D546',
          'P': u'\u2119',
          'Q': u'\u211A',
          'R': u'\u211D',
          'S': u'\U0001D54A',
          'T': u'\U0001D54B',
          'U': u'\U0001D54C',
          'V': u'\U0001D54D',
          'W': u'\U0001D54E',
          'X': u'\U0001D54F',
          'Y': u'\U0001D550',
          'Z': u'\u2124'}

def handle_keyword(name, node, string):
    skip = 0
    if name == 'begin':
        assert string.startswith('{matrix}')
        skip = 8
        entry = mtd()
        table = mtable(mtr(entry))
        node.append(table)
        node = entry
    elif name == 'end':
        assert string.startswith('{matrix}')
        skip = 8
        node = node.close().close().close()
    elif name == 'text':
        assert string[0] == '{'
        i = string.index('}')
        node = node.append(mtext(string[1:i]))
        skip = i + 1
    elif name == 'sqrt':
        sqrt = msqrt()
        node.append(sqrt)
        node = sqrt
    elif name == 'frac':
        frac = mfrac()
        node.append(frac)
        node = frac
    elif name == 'left':
        if string[0] == ' ':
            string = string[1:]
            skip = 1
        for par in ['(', '[', '|', '\\{', '\\langle', '.']:
            if string.startswith(par):
                break
        else:
            raise SyntaxError
        fenced = mfenced(par)
        node.append(fenced)
        node = fenced
        skip += len(par)
    elif name == 'right':
        if string[0] == ' ':
            string = string[1:]
            skip = 1
        for par in [')', ']', '|', '\\}', '\\rangle', '.']:
            if string.startswith(par):
                break
        else:
            raise SyntaxError
        node.closepar = par
        node = node.close()
        skip += len(par)
    elif name == 'mathbf':
        style = mstyle(nchildren=1, fontweight='bold')
        node.append(style)
        node = style
    elif name == 'mathbb':
        assert string[0] == '{' and string[1].isupper() and string[2] == '}'
        node = node.append(mi(mathbb[string[1]]))
        skip = 3
    else:
        code = special.get(name)
        if code is not None:
            node = node.append(mi(code))
        else:
            if name in functions:
                node = node.append(mo(name))
            else:
                chr = over.get(name)
                if chr is not None:
                    ovr = mover(mo(chr))
                    node.append(ovr)
                    node = ovr
                else:
                    raise SyntaxError

    return node, skip


def mathml(string, inline):
    tree = parse_latex_math(string, inline)
##    print repr(tree)
    string = string.replace("&", "&amp;")
    string = string.replace("<", "&lt;")
    string = string.replace(">", "&gt;")
    xml = ''.join(tree.xml())
    xml = """<math xmlns="http://www.w3.org/1998/Math/MathML">
    <semantics>
    %s
    <annotation encoding="LaTeX">%s</annotation>
    </semantics>
    </math>
    """ % (xml, string)
    if inline:
        return xml
    else:
        return xml + '<br/>\n'


## if __name__ == '__main__':
##     import sys
##     tree = parse_latex_math(sys.argv[1], 1)
##     print repr(tree)
##     print tree.xml()
##     print ''.join(tree.xml()).encode('ascii', 'ignore')


description = ('Generates (X)HTML documents from standalone reStructuredText '
               'sources.  ' + default_description)

publish_cmdline(writer_name='html', description=description)
