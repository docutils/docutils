#! /usr/bin/env python

"""
:Author: Dethe Elza
:Contact: delza@enfoldingsystems.com
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Directives for recursively including files
"""

__docformat__ = 'reStructuredText'


from docutils import nodes
from docutils.parsers.rst.directives.util import cheapDirective, CheapException, openAny
from docutils.statemachine import StateMachineWS
import sys
from docutils.parsers.rst.directives import parse_directive, DirectiveParseError

def exists(arg):
  return 1

include_option_spec = {}
raw_option_spec = {'file': str, 'url': str,}

def include(match, type_name, data, state, state_machine, options):
    '''
    Include a reST file as part of the content of this reST file
    '''
    try:
        arguments, options, content, blank_finish = parse_directive(match, type_name, data, 
            state, state_machine, options, option_spec=include_option_spec)
    except DirectiveParseError, (error, unparsed):
        return [error], unparsed
    f = file(arguments[0])
    text = f.readlines()
    temp_node = nodes.section(type_name, **options)
    state.nested_parse(text, 0, node=temp_node, match_titles=1)
    return temp_node.children, blank_finish
      
def raw(match, type_name, data, state, state_machine, options):
    '''
    Pass through content unchanged
    
    Content is included in output based on type argument
    
    Content may be included inline (content section of directive) or
    imported from a file or url.
    '''
    try:
        arguments, options, content, blank_finish = parse_directive(match, type_name, data, 
            state, state_machine, options, option_spec=raw_option_spec)
    except DirectiveParseError, (error, unparsed):
        return [error], unparsed
    format = arguments[0]
    if options.has_key('file'):
      f = file(options['file'])
      text = f.readlines()
    elif options.has_key('url'):
      f = urlopen(options['url'])
      text = f.readlines()
    else:
      text = content
    options['format'] = format
    raw_node = nodes.raw(arguments, '\n'.join(text), **options)
    return [raw_node], blank_finish
    
def replace(match, type_name, data, state, state_machine, options):
    try:
        arguments, options, content, blank_finish = parse_directive(match, type_name,
        data, state, state_machine, options, option_spec={})
    except DirectiveParseError, (error, unparsed):
        return [error], unparsed
    text_node = nodes.interpreted(arguments, '\n'.join(arguments))
    return [text_node], blank_finish

