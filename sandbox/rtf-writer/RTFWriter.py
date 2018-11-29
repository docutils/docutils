# -*- coding: utf-8 -*-
""" This is a docutils RTF Writer

   We just need a few features so it shouldn't be too difficult

   This was made possible by:

      http://search.cpan.org/~sburke/RTF-Writer/lib/RTF/Cookbook.pod

   from:

      Sean M. Burke, sburke@cpan.org
      
   Author:    Benoît Allard
   Contact:   benoit@aeteurope.nl
   Copyright: This module has been placed in the public domain.
"""

from docutils import writers, nodes

class Writer(writers.Writer):

    supported = ('rtf',)
    output = None

    def __init__(self):
        writers.Writer.__init__(self)
        self._translator_class = RTFTranslator

    def translate(self):
        visitor = self._translator_class(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.astext()

def toansi(text):
    """ Encode special characters """
    trans = {'{': r'\{',
             '}': r'\}',
             '\\': r'\\',
             }
    out = ''
    for char in text:
        if char in trans:
            out += trans[char]
        elif ord(char) < 127:
            out += char
        else:
            out += r"\'%x" % ord(char)
    return out

FONT_NAME = 'Tahoma'

# Unit is half point
FONT_SIZE = {
    -1: 18, # text
     0: 24, # title
     1: 28, # first heading
     2: 24, # second heading
}

PAR_INDENT = 480

def indent(fn):
    def wrapper(self, *args, **kwargs):
        self.par_level += 1
        return fn(self, *args, **kwargs)
    return wrapper


def dedent(fn):
    def wrapper(self, *args, **kwargs):
        self.par_level -= 1
        return fn(self, *args, **kwargs)
    return wrapper

class RTFTranslator(nodes.NodeVisitor):
    
    def __init__(self, document):
        nodes.NodeVisitor.__init__(self, document)
        self.body = []
        self.section_level = 0
        self.par_level = -1
        self.bullets = [] # stack of the currents bullets
        self.bullet = None # next one to add to the next paragraph

    def astext(self):
        return '\n'.join(self.body)

    def visit_document(self, node):
        self.body.append(r'{\rtf1\ansi\deff0{\fonttbl{\f0 %s;}}' % FONT_NAME)
        self.body.append(r'\deflang1033\widowctrl\fs%d' % FONT_SIZE[-1])
    
    def depart_document(self, node):
        self.body.append('}')

    def visit_title(self, node):
        if isinstance(node.parent, nodes.document):
            """ doc title """
            self.body.append(r'{\pard\par\qc\f0\fs%d\b' % FONT_SIZE[0])
        elif isinstance(node.parent, nodes.section):
            level = self.section_level
            self.body.append(r'{\pard\par\f0\fs%d\b' % FONT_SIZE[level])

    def depart_title(self, node):
        self.body.append(r'\par}')
            
    def visit_Text(self, node):
        if self.bullet is not None:
            self.body.append(self.bullet+'\~')
            self.bullet = None
        self.body.append(toansi(node.astext()))

    def depart_Text(self, node):
        pass

    def visit_strong(self, node):
        self.body.append(r'{\b')

    def depart_strong(self, node):
        self.body.append('}')

    def visit_section(self, node):
        self.section_level += 1
        
    def depart_section(self, node):
        self.section_level -= 1

    @indent
    def visit_paragraph(self, node):
        self.body.append(r'{\pard\par\f0\qj\li%d' % (PAR_INDENT * self.par_level))
        if self.bullet is not None:
            self.body.append(r'\fi-%d' % (PAR_INDENT / 2))

    @dedent
    def depart_paragraph(self, node):
        self.body.append('\par}')

    @indent
    def visit_bullet_list(self, node):
        self.bullets.append(node['bullet'])

    @dedent
    def depart_bullet_list(self, node):
        self.bullets.pop()

    def visit_list_item(self, node):
        self.bullet = self.bullets[-1]
        
    def depart_list_item(self, node):
        pass

    @indent
    def visit_block_quote(self, node):
        pass

    @dedent
    def depart_block_quote(self, node):
        pass

    def visit_reference(self, node):
        if 'refuri' in node:
            self.body.append(r'{\field{\*\fldinst{HYPERLINK "%s"}}{\fldrslt{\ul' % node['refuri'])
        else:
            # to balance parenthesis
            self.body.append(r'{{{')

    def depart_reference(self, node):
        self.body.append('}}}')

    def visit_docinfo(self, node):
        self.body.append(r'{\info')

    def depart_docinfo(self, node):
        self.body.append('}')

    def visit_author(self, node):
        self.body.append(r'{\author')

    def depart_author(self, node):
        self.body.append(r'}')

if __name__ == "__main__":
    """ To test the writer """
    from docutils.core import publish_string
    f_in = open('test.rtf', 'rb')
    rtf = publish_string(f_in.read(), writer=Writer())
    f_in.close()

    print rtf

    f_out = open('a.out', 'wb')
    f_out.write(rtf)
    f_out.close()
