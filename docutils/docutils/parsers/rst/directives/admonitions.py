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


def admonition(nodeclass, match, typename, data, state, statemachine,
               attributes):
    indented, indent, lineoffset, blankfinish \
          = statemachine.getfirstknownindented(match.end())
    text = '\n'.join(indented)
    admonitionnode = nodeclass(text)
    if text:
        state.nestedparse(indented, lineoffset, admonitionnode)
    return [admonitionnode], blankfinish

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
