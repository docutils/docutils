# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
A do-nothing Writer.

`self.output` will change from ``None`` to the empty string
in Docutils 0.22.
"""

from docutils import writers


class Writer(writers.UnfilteredWriter):

    supported = ('null',)
    """Formats this writer supports."""

    config_section = 'null writer'
    config_section_dependencies = ('writers',)

    def translate(self):
        # output = None   # TODO in 0.22
        pass
