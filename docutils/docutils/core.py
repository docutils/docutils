#! /usr/bin/env python

"""
:Authors: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Calling the `publish` convenience function (or instantiating a
`Publisher` object) with component names will result in default
behavior.  For custom behavior (setting component options), create
custom component objects first, and pass *them* to
`publish`/`Publisher`.
"""

__docformat__ = 'reStructuredText'

import sys
from docutils import Component
from docutils import readers, parsers, writers, io
from docutils.frontend import OptionParser


class Publisher:

    """
    A facade encapsulating the high-level logic of a Docutils system.
    """

    def __init__(self, reader=None, parser=None, writer=None,
                 source=None, source_class=io.FileIO,
                 destination=None, destination_class=io.FileIO,
                 options=None):
        """
        Initial setup.  If any of `reader`, `parser`, or `writer` are not
        specified, the corresponding ``set_...`` method should be called with
        a component name (`set_reader` sets the parser as well).
        """

        self.reader = reader
        """A `readers.Reader` instance."""

        self.parser = parser
        """A `parsers.Parser` instance."""

        self.writer = writer
        """A `writers.Writer` instance."""

        self.source = source
        """The source of input data, an `io.IO` instance."""

        self.source_class = source_class
        """The class for dynamically created source objects."""

        self.destination = destination
        """The destination for docutils output, an `io.IO` instance."""

        self.destination_class = destination_class
        """The class for dynamically created destination objects."""

        self.options = options
        """An object containing Docutils settings as instance attributes.
        Set by `self.process_command_line()` or `self.set_options()`."""

    def set_reader(self, reader_name, parser, parser_name):
        """Set `self.reader` by name."""
        reader_class = readers.get_reader_class(reader_name)
        self.reader = reader_class(parser, parser_name)
        self.parser = self.reader.parser

    def set_writer(self, writer_name):
        """Set `self.writer` by name."""
        writer_class = writers.get_writer_class(writer_name)
        self.writer = writer_class()

    def set_options(self, option_spec=None, **defaults):
        """
        Set default option values (keyword arguments).

        Set components first (`self.set_reader` & `self.set_writer`).
        Explicitly setting options disables command line option processing
        from `self.publish()`.
        """
        option_parser = OptionParser(
            components=(option_spec, self.reader, self.parser, self.writer),
            defaults=defaults)
        self.options = option_parser.get_default_values()

    def process_command_line(self, argv=None, usage=None, description=None,
                             option_spec=None):
        """
        Pass an empty list to `argv` to avoid reading `sys.argv` (the
        default).

        Set components first (`self.set_reader` & `self.set_writer`).
        """
        option_parser = OptionParser(
            components=(option_spec, self.reader, self.parser, self.writer),
            usage=usage, description=description)
        if argv is None:
            argv = sys.argv[1:]
        self.options, source, destination = option_parser.parse_args(argv)
        self.source = self.source_class(self.options, source=source)
        self.destination = self.destination_class(self.options,
                                                  destination=destination)

    def publish(self, argv=None, usage=None, description=None,
                option_spec=None):
        """
        Process command line options and arguments (if `self.options` not
        already set), run `self.reader` and then `self.writer`.  Return
        `self.writer`'s output.
        """
        if self.options is None:
            self.process_command_line(argv, usage, description, option_spec)
        document = self.reader.read(self.source, self.parser, self.options)
        output = self.writer.write(document, self.destination)
        if self.options.dump_internal_document_attributes:
            from pprint import pformat
            print >>sys.stderr, pformat(document.__dict__)
        return output


def publish(reader=None, reader_name='standalone',
            parser=None, parser_name='restructuredtext',
            writer=None, writer_name='pseudoxml',
            argv=None, usage=None, description=None, option_spec=None):
    """
    A convenience function for file I/O front-ends; set up & run a
    `Publisher`.
    """
    pub = Publisher(reader, parser, writer)
    if reader is None:
        pub.set_reader(reader_name, parser, parser_name)
    if writer is None:
        pub.set_writer(writer_name)
    pub.publish(argv, usage, description, option_spec)
