#!/usr/bin/env python

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
A minimal front end to the Docutils Publisher, producing Docutils XML.
"""

import sys
try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description, \
    Publisher, default_usage
from docutils import io
from docutils.writers.odtwriter import Writer


description = ('Generates Docutils-native XML from standalone '
               'reStructuredText sources.  ' + default_description)


class BinaryFileOutput(io.FileOutput):
    """
    A version of docutils.io.FileOutput which writes to a binary file.
    """
    def open(self):
        try:
            self.destination = open(self.destination_path, 'wb')
        except IOError, error:
            if not self.handle_io_errors:
                raise
            print >>sys.stderr, '%s: %s' % (error.__class__.__name__,
                                            error)
            print >>sys.stderr, ('Unable to open destination file for writing '
                                 '(%r).  Exiting.' % self.destination_path)
            sys.exit(1)
        self.opened = 1


def publish_cmdline_to_binary(reader=None, reader_name='standalone',
                    parser=None, parser_name='restructuredtext',
                    writer=None, writer_name='pseudoxml',
                    settings=None, settings_spec=None,
                    settings_overrides=None, config_section=None,
                    enable_exit_status=1, argv=None,
                    usage=default_usage, description=default_description,
                    destination=None, destination_class=BinaryFileOutput
                    ):
    """
    Set up & run a `Publisher` for command-line-based file I/O (input and
    output file paths taken automatically from the command line).  Return the
    encoded string output also.

    This is just like publish_cmdline, except that it uses
    io.BinaryFileOutput instead of io.FileOutput.

    Parameters: see `publish_programmatically` for the remainder.

    - `argv`: Command-line argument list to use instead of ``sys.argv[1:]``.
    - `usage`: Usage string, output if there's a problem parsing the command
      line.
    - `description`: Program description, output for the "--help" option
      (along with command-line option descriptions).
    """
    pub = Publisher(reader, parser, writer, settings=settings,
        destination_class=destination_class)
    pub.set_components(reader_name, parser_name, writer_name)
    output = pub.publish(
        argv, usage, description, settings_spec, settings_overrides,
        config_section=config_section, enable_exit_status=enable_exit_status)
    return output


writer = Writer()
output = publish_cmdline_to_binary(writer=writer, description=description)
    
