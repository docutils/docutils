#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Directives for typically HTML-specific constructs.
"""

__docformat__ = 'reStructuredText'


from docutils import nodes, utils
from docutils.parsers.rst import states
from docutils.transforms import components


def meta(match, type_name, data, state, state_machine, attributes):
    line_offset = state_machine.line_offset
    block, indent, offset, blank_finish = \
          state_machine.get_first_known_indented(match.end(), until_blank=1)
    node = nodes.Element()
    if block:
        new_line_offset, blank_finish = state.nested_list_parse(
              block, offset, node, initial_state='MetaBody',
              blank_finish=blank_finish, state_machine_kwargs=metaSMkwargs)
        if (new_line_offset - offset) != len(block): # incomplete parse of block?
            blocktext = '\n'.join(state_machine.input_lines[
                  line_offset : state_machine.line_offset+1])
            msg = state_machine.reporter.error(
                  'Invalid meta directive at line %s.'
                  % state_machine.abs_line_number(), '',
                  nodes.literal_block(blocktext, blocktext))
            node += msg
    else:
        msg = state_machine.reporter.error('Empty meta directive at line %s.'
                                          % state_machine.abs_line_number())
        node += msg
    return node.getchildren(), blank_finish

def imagemap(match, type_name, data, state, state_machine, attributes):
    return [], 0


class MetaBody(states.SpecializedBody):

    class meta(nodes.Special, nodes.PreBibliographic, nodes.Element):
        """HTML-specific "meta" element."""
        pass

    def field_marker(self, match, context, next_state):
        """Meta element."""
        node, blank_finish = self.parsemeta(match)
        self.parent += node
        return [], next_state, []

    def parsemeta(self, match):
        name, args = self.parse_field_marker(match)
        indented, indent, line_offset, blank_finish = \
              self.state_machine.get_first_known_indented(match.end())
        node = self.meta()
        pending = nodes.pending(components.Filter, 'first writer',
                                {'writer': 'html', 'nodes': [node]})
        node['content'] = ' '.join(indented)
        if not indented:
            line = self.state_machine.line
            msg = self.reporter.info(
                  'No content for meta tag "%s" at line %s.'
                  % (name, self.state_machine.abs_line_number()),
                  '', nodes.literal_block(line, line))
            return msg, blank_finish
        try:
            attname, val = utils.extract_name_value(name)[0]
            node[attname.lower()] = val
        except utils.NameValueError:
            node['name'] = name
        for arg in args:
            try:
                attname, val = utils.extract_name_value(arg)[0]
                node[attname.lower()] = val
            except utils.NameValueError, detail:
                line = self.state_machine.line
                msg = self.reporter.error(
                      'Error parsing meta tag attribute "%s" at line %s: %s.'
                      % (arg, self.state_machine.abs_line_number(), detail),
                      '', nodes.literal_block(line, line))
                return msg, blank_finish
        self.document.note_pending(pending)
        return pending, blank_finish


metaSMkwargs = {'state_classes': (MetaBody,)}
