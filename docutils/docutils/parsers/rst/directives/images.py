#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Directives for figures and simple images.
"""

__docformat__ = 'reStructuredText'


import sys
from docutils.parsers.rst import states
from docutils import nodes, utils

def unchanged(arg):
    return arg                          # unchanged!

image_attribute_spec = {'alt': unchanged,
                        'height': int,
                        'width': int,
                        'scale': int}

def image(match, type_name, data, state, state_machine, attributes):
    lineno = state_machine.abs_line_number()
    line_offset = state_machine.line_offset
    datablock, indent, offset, blank_finish = \
          state_machine.get_first_known_indented(match.end(), until_blank=1)
    blocktext = '\n'.join(state_machine.input_lines[
          line_offset : line_offset + len(datablock) + 1])
    for i in range(len(datablock)):
        if datablock[i][:1] == ':':
            attlines = datablock[i:]
            datablock = datablock[:i]
            break
    else:
        attlines = []
    if not datablock:
        error = state_machine.reporter.error(
              'Missing image URI argument at line %s.' % lineno, '',
              nodes.literal_block(blocktext, blocktext))
        return [error], blank_finish
    attoffset = line_offset + i
    reference = ''.join([line.strip() for line in datablock])
    if reference.find(' ') != -1:
        error = state_machine.reporter.error(
              'Image URI at line %s contains whitespace.' % lineno, '',
              nodes.literal_block(blocktext, blocktext))
        return [error], blank_finish
    if attlines:
        success, data, blank_finish = state.parse_extension_attributes(
              image_attribute_spec, attlines, blank_finish)
        if success:                     # data is a dict of attributes
            attributes.update(data)
        else:                           # data is an error string
            error = state_machine.reporter.error(
                  'Error in "%s" directive attributes at line %s:\n%s.'
                  % (match.group(1), lineno, data), '',
                  nodes.literal_block(blocktext, blocktext))
            return [error], blank_finish
    attributes['uri'] = reference
    imagenode = nodes.image(blocktext, **attributes)
    return [imagenode], blank_finish

def figure(match, type_name, data, state, state_machine, attributes):
    line_offset = state_machine.line_offset
    (imagenode,), blank_finish = image(match, type_name, data, state,
                                      state_machine, attributes)
    indented, indent, offset, blank_finish \
          = state_machine.get_first_known_indented(sys.maxint)
    blocktext = '\n'.join(state_machine.input_lines[line_offset:
                                                  state_machine.line_offset+1])
    if isinstance(imagenode, nodes.system_message):
        if indented:
            imagenode[-1] = nodes.literal_block(blocktext, blocktext)
        return [imagenode], blank_finish
    figurenode = nodes.figure('', imagenode)
    if indented:
        node = nodes.Element()          # anonymous container for parsing
        state.nested_parse(indented, line_offset, node)
        firstnode = node[0]
        if isinstance(firstnode, nodes.paragraph):
            caption = nodes.caption(firstnode.rawsource, '',
                                    *firstnode.children)
            figurenode += caption
        elif not (isinstance(firstnode, nodes.comment) and len(firstnode) == 0):
            error = state_machine.reporter.error(
                  'Figure caption must be a paragraph or empty comment.', '',
                  nodes.literal_block(blocktext, blocktext))
            return [figurenode, error], blank_finish
        if len(node) > 1:
            figurenode += nodes.legend('', *node[1:])
    return [figurenode], blank_finish
