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
from docutils.parsers.rst.directives import parse_directive

def exists(arg):
  return 1

include_option_spec = {}

def include(match, type_name, data, state, state_machine, option_pre):
    '''
    Test new directive parsing
    '''
    print >> sys.sdtout, this.__name__
    result = parse_directive(match, type_name, state, state_machine, option_pre, {'file': str, 'url': str})
    for l in result:
      print >> sys.stdout >> '   ', l
      
raw = include
    

def old_include(match, type_name, data, state, state_machine, option_pre):
    '''
    Include will try to open it's non-attrbibute arg as a file.  Failing that,
    it will try to open it as a URL.  Finally it will use the argument as a
    string.  By default, it will process the data it receives as reST, but in
    the presence of the raw option it will pass data through untouched.  If
    the raw option is present we also require the format option to guide
    the writer later, otherwise the format value is not used.
    '''
    try:
        datablock, blocktext, blank_finish = cheapDirective(match, type_name, 
	  data, state, state_machine, option_pre, include_option_spec)
    except CheapException, (error, unparsed):
        return [error], unparsed
    reference = ''.join([line.strip() for line in datablock])
    # grab data from file/uri/string
    f = openAny(reference)
    text = f.readlines()
      
    temp_node = nodes.section(blocktext, **option_pre)
    state.nested_parse(text, 0, node=temp_node, match_titles=1)
    return temp_node.children, blank_finish

raw_option_spec = {'file': str, 'url': str}

def old_raw(match, type_name, data, state, state_machine, option_pre):
    # initialize default variables
    option_pre['include'] = 0
    option_pre['format'] = ''
    try:
        datablock, blocktext, blank_finish = cheapDirective(match, type_name, 
	  data, state, state_machine, option_pre, raw_option_spec)
    except CheapException, (error, unparsed):
        return [error], unparsed
      
    if option_pre['include']:
        # grab data from file/uri/string
        f = openAny(option_pre['include'].strip())
        datablock = f.readlines()
    raw_node = nodes.raw(blocktext, '\n'.join(datablock), **option_pre)
    return [raw_node], blank_finish

def replace(match, type_name, data, state, state_machine, option_pre):
    try:
        datablock, blocktext, blank_finish = cheapDirective(match, type_name,
        data, state, state_machine, option_pre, {})
    except CheapException, (error, unparsed):
        return [error], unparsed
    text_node = nodes.interpreted(blocktext, '\n'.join(datablock))
    return [text_node], blank_finish
    
    
if __name__ == '__main__':
  print include
  print raw
