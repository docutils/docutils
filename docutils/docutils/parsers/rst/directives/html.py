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


def meta(match, typename, data, state, statemachine, attributes):
    lineoffset = statemachine.lineoffset
    block, indent, offset, blankfinish = \
          statemachine.getfirstknownindented(match.end(), uptoblank=1)
    node = nodes.Element()
    if block:
        newlineoffset, blankfinish = state.nestedlistparse(
              block, offset, node, initialstate='MetaBody',
              blankfinish=blankfinish, statemachinekwargs=metaSMkwargs)
        if (newlineoffset - offset) != len(block): # incomplete parse of block?
            blocktext = '\n'.join(statemachine.inputlines[
                  lineoffset : statemachine.lineoffset+1])
            msg = statemachine.memo.reporter.error(
                  'Invalid meta directive at line %s.'
                  % statemachine.abslineno(), '',
                  nodes.literal_block(blocktext, blocktext))
            node += msg
    else:
        msg = statemachine.memo.reporter.error(
              'Empty meta directive at line %s.' % statemachine.abslineno())
        node += msg
    return node.getchildren(), blankfinish

def imagemap(match, typename, data, state, statemachine, attributes):
    return [], 0


class MetaBody(states.SpecializedBody):

    class meta(nodes.Special, nodes.PreBibliographic, nodes.Element):
        """HTML-specific "meta" element."""
        pass

    def field_marker(self, match, context, nextstate):
        """Meta element."""
        node, blankfinish = self.parsemeta(match)
        self.statemachine.node += node
        return [], nextstate, []

    def parsemeta(self, match):
        name, args = self.parse_field_marker(match)
        indented, indent, lineoffset, blankfinish = \
              self.statemachine.getfirstknownindented(match.end())
        node = self.meta()
        node['content'] = ' '.join(indented)
        if not indented:
            line = self.statemachine.line
            msg = self.statemachine.memo.reporter.info(
                  'No content for meta tag "%s".' % name, '',
                  nodes.literal_block(line, line))
            self.statemachine.node += msg
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
                line = self.statemachine.line
                msg = self.statemachine.memo.reporter.error(
                      'Error parsing meta tag attribute "%s": %s'
                      % (arg, detail), '', nodes.literal_block(line, line))
                self.statemachine.node += msg
        return node, blankfinish


metaSMkwargs = {'stateclasses': (MetaBody,)}
