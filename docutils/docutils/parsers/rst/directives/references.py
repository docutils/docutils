#! /usr/bin/env python

"""
:Author: David Goodger, Dmitry Jemerov
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Directives for references and targets.
"""

__docformat__ = 'reStructuredText'

from docutils import nodes
from docutils.transforms import references


def target_notes(match, type_name, data, state, state_machine,
                 option_presets):
    """Target footnote generation."""
    pending = nodes.pending(references.TargetNotes, 'first reader', {})
    state_machine.document.note_pending(pending)
    nodelist = [pending]
    if data:
        warning = state_machine.reporter.warning(
              'The "%s" directive takes no data; "%s" ignored.'
              % (match.group(1), data), line=state_machine.abs_line_number())
        nodelist.append(warning)
    return nodelist, state_machine.is_next_line_blank()
