#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Miscellaneous directives.
"""

__docformat__ = 'reStructuredText'


from docutils import nodes


def raw(match, typename, data, state, statemachine, attributes):
    return [], 1

def directive_test_function(match, typename, data, state, statemachine,
                            attributes):
    try:
        statemachine.nextline()
        indented, indent, offset, blankfinish = statemachine.getindented()
        text = '\n'.join(indented)
    except IndexError:
        text = ''
        blankfinish = 1
    if text:
        info = statemachine.memo.reporter.info(
              'Directive processed. Type="%s", data="%s", directive block:'
              % (typename, data), '', nodes.literal_block(text, text))
    else:
        info = statemachine.memo.reporter.info(
              'Directive processed. Type="%s", data="%s", directive block: None'
              % (typename, data))
    return [info], blankfinish
