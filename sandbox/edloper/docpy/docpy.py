#!/usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
:todo: role-labeled inline text
:todo: generate output (subclass LaTeXTransformer)
:todo later: macros (susbtitution refs)
"""
from docutils import nodes
from docutils.parsers.rst import directives
from docutils.parsers.rst.directives import admonitions
from docutils.parsers.rst import states
from docutils.writers import latex2e
import re, tokenize

######################################################################
# New nodes
######################################################################

class funcdesc(nodes.Admonition, nodes.Element): pass
class classdesc(nodes.Admonition, nodes.Element): pass
class methoddesc(nodes.Admonition, nodes.Element): pass
class datadesc(nodes.Admonition, nodes.Element): pass
class desc_name(nodes.Part, nodes.Inline, nodes.TextElement): pass

# We might use pynodes instead.
class func_signature(nodes.Part, nodes.Inline, nodes.TextElement): pass
class func_name(nodes.Part, nodes.Inline, nodes.TextElement): pass
class func_parameterlist(nodes.Part, nodes.Inline, nodes.TextElement): pass
class func_parameter(nodes.Part, nodes.Inline, nodes.TextElement): pass
class func_optional(nodes.Part, nodes.Inline, nodes.TextElement): pass

# These are the inline things.
class docpy_function(nodes.Inline, nodes.TextElement): pass
class docpy_manpage(nodes.Inline, nodes.TextElement): pass
class docpy_regexp(nodes.Inline, nodes.TextElement): pass
class docpy_file(nodes.Inline, nodes.TextElement): pass
class docpy_label(nodes.Inline, nodes.TextElement): pass
class docpy_class(nodes.Inline, nodes.TextElement): pass
class docpy_method(nodes.Inline, nodes.TextElement): pass
class docpy_cfunction(nodes.Inline, nodes.TextElement): pass
class docpy_refmodule(nodes.Inline, nodes.TextElement): pass
class docpy_module(nodes.Inline, nodes.TextElement): pass
class docpy_var(nodes.Inline, nodes.TextElement): pass
inline_docpy_elements = { # Maps roles to entitites
    'function':docpy_function,
    'manpage':docpy_manpage,
    'regexp':docpy_regexp,
    'file':docpy_file,
    'label': docpy_label,
    'class': docpy_class,
    'method': docpy_method,
    'cfunction': docpy_cfunction,
    'refmodule': docpy_refmodule,
    'module': docpy_module,
    'var': docpy_var,
    }

for (role, element) in inline_docpy_elements.items():
    states.register_inliner_role(role, element)


######################################################################
# Directives
######################################################################

# Transform a python signature into RST.                  
def parse_signature(s):
    """
    A straw-man implementation. (Might be sufficient)
    """
    s = s.strip()
    m = re.match(r'^(\w+)\s*\((.*)\)$', s)
    if m is None: raise ValueError(`s`)
    name, arglist = m.groups()

    sig = func_signature(s,'')

    sig.append(func_name(name,name))
    sig.append(func_parameterlist())

    stack = [sig[-1]]
    for token in re.split(r'(\*{0,2}\w+|[\[\],])', arglist):
        #print `token`, stack
        if token == '[':
            opt = func_optional()
            stack[-1].append(opt)
            stack.append(opt)
        elif token == ']':
            stack.pop()
        elif token == ',':
            pass
        elif re.match(r'^\s*$', token):
            pass
        elif re.match(r'^\*{0,2}\w+$', token):
            stack[-1].append(func_parameter(token,token))
        else:
            raise ValueError(s)
    if len(stack) != 1: raise ValueError(s)
    return sig

def funcdesc_directive(name, arguments, options, content, lineno,
                           content_offset, block_text, state, state_machine):
    rv = admonitions.make_admonition(funcdesc, name, [], options, content,
                                     lineno, content_offset, block_text,
                                     state, state_machine)
    rv[0].insert(0, parse_signature(arguments[0]))
    return rv
funcdesc_directive.content = 1
funcdesc_directive.arguments = (1,0,1) # 1 required arg with spaces.

def methoddesc_directive(name, arguments, options, content, lineno,
                           content_offset, block_text, state, state_machine):
    rv = admonitions.make_admonition(methoddesc, name, [], options, content,
                                     lineno, content_offset, block_text,
                                     state, state_machine)
    rv[0].insert(0, parse_signature(arguments[0]))
    return rv
methoddesc_directive.content = 1
methoddesc_directive.arguments = (1,0,1) # 1 required arg with spaces.

def classdesc_directive(name, arguments, options, content, lineno,
                           content_offset, block_text, state, state_machine):
    rv = admonitions.make_admonition(classdesc, name, [], options, content,
                                     lineno, content_offset, block_text,
                                     state, state_machine)
    rv[0].insert(0, parse_signature(arguments[0]))
    return rv
classdesc_directive.content = 1
classdesc_directive.arguments = (1,0,1) # 1 required arg with spaces.

def datadesc_directive(name, arguments, options, content, lineno,
                           content_offset, block_text, state, state_machine):
    rv = admonitions.make_admonition(datadesc, name, [], options, content,
                                     lineno, content_offset, block_text,
                                     state, state_machine)
    rv[0].insert(0, desc_name(arguments[0], arguments[0]))
    return rv
datadesc_directive.content = 1
datadesc_directive.arguments = (1,0,1) # 1 required arg with spaces.

# Register the directives
directives.register_directive('funcdesc', funcdesc_directive)
directives.register_directive('methoddesc', methoddesc_directive)
directives.register_directive('classdesc', classdesc_directive)
directives.register_directive('datadesc', datadesc_directive)

######################################################################
# Writer
######################################################################

class DocpyWriter(latex2e.Writer):
    def translate(self):
        visitor = DocpyTranslator(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.astext()
        self.head_prefix = visitor.head_prefix
        self.head = visitor.head
        self.body_prefix = visitor.body_prefix
        self.body = visitor.body
        self.body_suffix = visitor.body_suffix

class DocpyTranslator(latex2e.LaTeXTranslator):
    """
    Incompatibilities:
      - latex docs uses \subsection, we generate \subsection*
        (e.g., \subsection{asyncore Exampe ...})
      - we generate header & footer info that we don't need
      - we don't handle RFC822 stuff & generate a top-level \section
      - in a function signature, we escape underscores but latex docs
        don't.  e.g., funcdesc (line 46 of asyncore).
      - in role-labeled text, we escape underscore but latex docs
        don't.
      - table rendering is completely different.
    """

    def __init__(self, *args):
        latex2e.LaTeXTranslator.__init__(self, *args)
        self.section_level = 1
        self.first_paramter_visited = 0
        
    
    #------------------------------------------------------------
    # Directives
    #------------------------------------------------------------
    def visit_funcdesc(self, node):
        self.body.append('\n'+r'\begin{funcdesc}')
    def depart_funcdesc(self, node):
        self.body.append(r'\end{funcdesc}'+'\n')

    def visit_methoddesc(self, node):
        self.body.append('\n'+r'\begin{methoddesc}')
    def depart_methoddesc(self, node):
        self.body.append(r'\end{methoddesc}'+'\n')

    def visit_classdesc(self, node):
        self.body.append('\n'+r'\begin{classdesc}')
    def depart_classdesc(self, node):
        self.body.append(r'\end{classdesc}'+'\n')

    def visit_datadesc(self, node):
        self.body.append('\n'+r'\begin{datadesc}')
    def depart_datadesc(self, node):
        self.body.append(r'\end{datadesc}'+'\n')

    def visit_desc_name(self, node):
        self.body.append('{')
    def depart_desc_name(self, node):
        self.body.append('}')
    
    def visit_func_name(self, node):
        self.body.append('{')
    def depart_func_name(self, node):
        self.body.append('}')
    
    def visit_func_signature(self, node): pass
    def depart_func_signature(self, node): pass
    
    def visit_func_parameterlist(self, node):
        self.body.append('{')
        self.first_parameter_visited = 0
    def depart_func_parameterlist(self, node):
        self.body.append('}')

    def visit_func_parameter(self, node):
        if self.first_parameter_visited:
            self.body.append(', ')
        self.first_parameter_visited = 1
    def depart_func_parameter(self, node): pass

    def visit_func_optional(self, node):
        self.body.append(r'\optional{')
        if self.first_paramter_visited:
            self.body.append(',')
    def depart_func_optional(self, node):
        self.body.append('}')
        

    #------------------------------------------------------------
    # Inline Roles
    #------------------------------------------------------------
    def visit_docpy_function(self, node):
        self.body.append(r'\function{')
    def depart_docpy_function(self, node):
        self.body.append(r'}')

    def visit_docpy_manpage(self, node):
        self.body.append(r'\manpage{')
    def depart_docpy_manpage(self, node):
        self.body.append(r'}')

    def visit_docpy_regexp(self, node):
        self.body.append(r'\regexp{')
    def depart_docpy_regexp(self, node):
        self.body.append(r'}')

    def visit_docpy_file(self, node):
        self.body.append(r'\file{')
    def depart_docpy_file(self, node):
        self.body.append(r'}')
        
    def visit_docpy_label(self, node):
        self.body.append(r'\label{')
    def depart_docpy_label(self, node):
        self.body.append(r'}')
        
    def visit_docpy_class(self, node):
        self.body.append(r'\class{')
    def depart_docpy_class(self, node):
        self.body.append(r'}')
        
    def visit_docpy_method(self, node):
        self.body.append(r'\method{')
    def depart_docpy_method(self, node):
        self.body.append(r'}')
        
    def visit_docpy_cfunction(self, node):
        self.body.append(r'\cfunction{')
    def depart_docpy_cfunction(self, node):
        self.body.append(r'}')
        
    def visit_docpy_refmodule(self, node):
        self.body.append(r'\refmodule{')
    def depart_docpy_refmodule(self, node):
        self.body.append(r'}')
        
    def visit_docpy_module(self, node):
        self.body.append(r'\module{')
    def depart_docpy_module(self, node):
        self.body.append(r'}')
        
    def visit_docpy_var(self, node):
        self.body.append(r'\var{')
    def depart_docpy_var(self, node):
        self.body.append(r'}')
        
    #------------------------------------------------------------
    # Etc.
    #------------------------------------------------------------
    def visit_literal(self, node):
        self.literal = 1
        self.body.append('\\code{')

######################################################################
# Front-end code
######################################################################
if __name__ == '__main__':
    import locale
    try:
        locale.setlocale(locale.LC_ALL, '')
    except:
        pass

    from docutils.core import publish_cmdline, default_description
    description = default_description
    publish_cmdline(writer=DocpyWriter(), description=description)
    #publish_cmdline(writer_name='pseudoxml', description=description)

