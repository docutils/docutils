#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Admonition directives.
"""

__docformat__ = 'reStructuredText'


from docutils.parsers.rst import states
from docutils import nodes


def admonition(node_class, match, type_name, data, state, state_machine,
               option_presets):
    indented, indent, line_offset, blank_finish \
          = state_machine.get_first_known_indented(match.end())
    text = '\n'.join(indented)
    admonition_node = node_class(text)
    if text:
        state.nested_parse(indented, line_offset, admonition_node)
    return [admonition_node], blank_finish

def attention(*args, **kwargs):
    return admonition(nodes.attention, *args, **kwargs)

def caution(*args, **kwargs):
    return admonition(nodes.caution, *args, **kwargs)

def danger(*args, **kwargs):
    return admonition(nodes.danger, *args, **kwargs)

def error(*args, **kwargs):
    return admonition(nodes.error, *args, **kwargs)

def important(*args, **kwargs):
    return admonition(nodes.important, *args, **kwargs)

def note(*args, **kwargs):
    return admonition(nodes.note, *args, **kwargs)

def tip(*args, **kwargs):
    return admonition(nodes.tip, *args, **kwargs)

def hint(*args, **kwargs):
    return admonition(nodes.hint, *args, **kwargs)

def warning(*args, **kwargs):
    return admonition(nodes.warning, *args, **kwargs)
