#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Directives for additional body elements.
"""

__docformat__ = 'reStructuredText'


import sys
from docutils import nodes


def topic(match, type_name, data, state, state_machine, attributes):
    lineno = state_machine.abs_line_number()
    initial_offset = state_machine.line_offset
    indented, indent, line_offset, blank_finish \
          = state_machine.get_first_known_indented(match.end())
    blocktext = '\n'.join(state_machine.input_lines[
        initial_offset : line_offset + len(indented)])
    if not state_machine.match_titles:
        error = state_machine.reporter.error(
              'Topics may not be nested within body elements (line %s).'
              % lineno, '', nodes.literal_block(blocktext, blocktext))
        return [error], blank_finish
    if not indented:
        return [], blank_finish
    title_text = indented.pop(0)
    textnodes, messages = state.inline_text(title_text, lineno)
    title = nodes.title(title_text, '', *textnodes)
    if indented:
        if indented[0].strip():
            warning = state_machine.reporter.warning(
                'The second line of a topic block must be blank (line %s).'
                % (lineno + 1 + line_offset - initial_offset), '')
            messages.append(warning)
        text = '\n'.join(indented)
    else:
        text = ''
    topic_node = nodes.topic(text, title, *messages)
    if text:
        state.nested_parse(indented, line_offset, topic_node)
    return [topic_node], blank_finish


def parsed_literal(match, type_name, data, state, state_machine, attributes,
                   node_class=nodes.literal_block):
    lineno = state_machine.abs_line_number()
    initial_offset = state_machine.line_offset
    indented, indent, line_offset, blank_finish \
          = state_machine.get_first_known_indented(match.end())
    while indented and not indented[-1].strip():
        indented.pop()
    if not indented:
        return [], blank_finish
    text = '\n'.join(indented)
    textnodes, messages = state.inline_text(text, lineno)
    node = node_class(text, '', *textnodes)
    return [node] + messages, blank_finish


def line_block(match, type_name, data, state, state_machine, attributes):
    return parsed_literal(match, type_name, data, state, state_machine,
                          attributes, node_class=nodes.line_block)
