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

def image(match, typename, data, state, statemachine, attributes):
    lineno = statemachine.abslineno()
    lineoffset = statemachine.lineoffset
    datablock, indent, offset, blankfinish = \
          statemachine.getfirstknownindented(match.end(), uptoblank=1)
    blocktext = '\n'.join(statemachine.inputlines[
          lineoffset : lineoffset + len(datablock) + 1])
    for i in range(len(datablock)):
        if datablock[i][:1] == ':':
            attlines = datablock[i:]
            datablock = datablock[:i]
            break
    else:
        attlines = []
    if not datablock:
        error = statemachine.memo.reporter.error(
              'Missing image URI argument at line %s.' % lineno, '',
              nodes.literal_block(blocktext, blocktext))
        return [error], blankfinish
    attoffset = lineoffset + i
    reference = ''.join([line.strip() for line in datablock])
    if reference.find(' ') != -1:
        error = statemachine.memo.reporter.error(
              'Image URI at line %s contains whitespace.' % lineno, '',
              nodes.literal_block(blocktext, blocktext))
        return [error], blankfinish
    if attlines:
        success, data, blankfinish = state.parse_extension_attributes(
              image_attribute_spec, attlines, blankfinish)
        if success:                     # data is a dict of attributes
            attributes.update(data)
        else:                           # data is an error string
            error = statemachine.memo.reporter.error(
                  'Error in "%s" directive attributes at line %s:\n%s.'
                  % (match.group(1), lineno, data), '',
                  nodes.literal_block(blocktext, blocktext))
            return [error], blankfinish
    attributes['uri'] = reference
    imagenode = nodes.image(blocktext, **attributes)
    return [imagenode], blankfinish

def figure(match, typename, data, state, statemachine, attributes):
    lineoffset = statemachine.lineoffset
    (imagenode,), blankfinish = image(match, typename, data, state,
                                      statemachine, attributes)
    indented, indent, offset, blankfinish \
          = statemachine.getfirstknownindented(sys.maxint)
    blocktext = '\n'.join(statemachine.inputlines[lineoffset:
                                                  statemachine.lineoffset+1])
    if isinstance(imagenode, nodes.system_message):
        if indented:
            imagenode[-1] = nodes.literal_block(blocktext, blocktext)
        return [imagenode], blankfinish
    figurenode = nodes.figure('', imagenode)
    if indented:
        node = nodes.Element()          # anonymous container for parsing
        state.nestedparse(indented, lineoffset, node)
        firstnode = node[0]
        if isinstance(firstnode, nodes.paragraph):
            caption = nodes.caption(firstnode.rawsource, '',
                                    *firstnode.children)
            figurenode += caption
        elif not (isinstance(firstnode, nodes.comment) and len(firstnode) == 0):
            error = statemachine.memo.reporter.error(
                  'Figure caption must be a paragraph or empty comment.', '',
                  nodes.literal_block(blocktext, blocktext))
            return [figurenode, error], blankfinish
        if len(node) > 1:
            figurenode += nodes.legend('', *node[1:])
    return [figurenode], blankfinish
