#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Document component directives.
"""

__docformat__ = 'reStructuredText'


from docutils import nodes
import docutils.transforms.components


contents_attribute_spec = {'depth': int,
                           'local': (lambda x: x)}

def contents(match, typename, data, state, statemachine, attributes):
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
        i = 0
    titletext = ' '.join([line.strip() for line in datablock])
    if titletext:
        textnodes, messages = state.inline_text(titletext, lineno)
        title = nodes.title(titletext, '', *textnodes)
    else:
        messages = []
        title = None
    pending = nodes.pending(docutils.transforms.components.Contents,
                            'last_reader', {'title': title}, blocktext)
    if attlines:
        success, data, blankfinish = state.parse_extension_attributes(
              contents_attribute_spec, attlines, blankfinish)
        if success:                     # data is a dict of attributes
            pending.details.update(data)
        else:                           # data is an error string
            error = statemachine.memo.reporter.error(
                  'Error in "%s" directive attributes at line %s:\n%s.'
                  % (match.group(1), lineno, data), '',
                  nodes.literal_block(blocktext, blocktext))
            return [error] + messages, blankfinish
    statemachine.memo.document.note_pending(pending)
    return [pending] + messages, blankfinish
