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


def raw(match, type_name, data, state, state_machine, option_presets):
    return [], 1

def directive_test_function(match, type_name, data, state, state_machine,
                            option_presets):
    lineno = state_machine.abs_line_number()
    try:
        state_machine.next_line()
        indented, indent, offset, blank_finish = state_machine.get_indented()
        text = '\n'.join(indented)
    except IndexError:
        text = ''
        blank_finish = 1
    if text:
        info = state_machine.reporter.info(
              'Directive processed. Type="%s", data="%s", directive block:'
              % (type_name, data), '', nodes.literal_block(text, text),
              line=lineno)
    else:
        info = state_machine.reporter.info(
              'Directive processed. Type="%s", data="%s", directive block: '
              'None' % (type_name, data), line=lineno)
    return [info], blank_finish
