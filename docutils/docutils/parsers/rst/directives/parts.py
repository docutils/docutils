#! /usr/bin/env python

"""
:Author: David Goodger, Dmitry Jemerov
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Directives for document parts.
"""

__docformat__ = 'reStructuredText'

from docutils import nodes
from docutils.transforms import parts


def unchanged(arg):
    return arg                          # unchanged!

def backlinks(arg):
    try:
        value = arg.lower().strip()
    except AttributeError:
        raise TypeError('must supply an argument; choose from "top", '
                        '"entry", or "none"')
    if value == 'none':
        return None
    elif value == 'top' or arg == 'entry':
        return value
    else:
        raise ValueError(
            '"%s" unknown; choose from "top", "entry", or "none"' % arg)

contents_attribute_spec = {'depth': int,
                           'local': unchanged,
                           'backlinks': backlinks,
                           'qa': unchanged}

def contents(match, type_name, data, state, state_machine, attributes):
    """Table of contents."""
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
        i = 0
    titletext = ' '.join([line.strip() for line in datablock])
    if titletext:
        textnodes, messages = state.inline_text(titletext, lineno)
        title = nodes.title(titletext, '', *textnodes)
    else:
        messages = []
        title = None
    pending = nodes.pending(parts.Contents, 'first writer', {'title': title},
                            blocktext)
    if attlines:
        success, data, blank_finish = state.parse_extension_attributes(
              contents_attribute_spec, attlines, blank_finish)
        if success:                     # data is a dict of attributes
            pending.details.update(data)
        else:                           # data is an error string
            error = state_machine.reporter.error(
                  'Error in "%s" directive attributes at line %s:\n%s.'
                  % (match.group(1), lineno, data), '',
                  nodes.literal_block(blocktext, blocktext))
            return [error] + messages, blank_finish
    state_machine.document.note_pending(pending)
    return [pending] + messages, blank_finish

sectnum_attribute_spec = {'depth': int}

def sectnum(match, type_name, data, state, state_machine, attributes):
    """Automatic section numbering."""
    lineno = state_machine.abs_line_number()
    line_offset = state_machine.line_offset
    datablock, indent, offset, blank_finish = \
          state_machine.get_first_known_indented(match.end(), until_blank=1)
    pending = nodes.pending(parts.SectNum, 'last reader', {})
    success, data, blank_finish = state.parse_extension_attributes(
          sectnum_attribute_spec, datablock, blank_finish)
    if success:                     # data is a dict of attributes
        pending.details.update(data)
    else:                           # data is an error string
        blocktext = '\n'.join(state_machine.input_lines[
            line_offset : line_offset + len(datablock) + 1])
        error = state_machine.reporter.error(
              'Error in "%s" directive attributes at line %s:\n%s.'
              % (match.group(1), lineno, data), '',
              nodes.literal_block(blocktext, blocktext))
        return [error], blank_finish
    state_machine.document.note_pending(pending)
    return [pending], blank_finish
