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

import readers, parsers, writers, utils


class Publisher:

    """
    A facade encapsulating the high-level logic of a Docutils system.
    """

    reporter = None
    """A `utils.Reporter` instance used for all document processing."""

    def __init__(self, reader=None, parser=None, writer=None, reporter=None,
                 language_code='en', warning_level=2, error_level=4,
                 warning_stream=None, debug=0):
        """
        Initial setup.  If any of `reader`, `parser`, or `writer` are
        not specified, the corresponding 'set*' method should be
        called.
        """
        self.reader = reader
        self.parser = parser
        self.writer = writer
        if not reporter:
            reporter = utils.Reporter(warning_level, error_level,
                                      warning_stream, debug)
        self.reporter = reporter
        self.language_code = language_code

    def set_reader(self, reader_name, parser, parser_name,
                   language_code=None):
        """Set `self.reader` by name."""
        reader_class = readers.get_reader_class(reader_name)
        self.reader = reader_class(self.reporter, parser, parser_name,
                                   language_code or self.language_code)

    def set_parser(self, parser_name):
        """Set `self.parser` by name."""
        parser_class = parsers.get_parser_class(parser_name)
        self.parser = parser_class()

    def set_writer(self, writer_name):
        """Set `self.writer` by name."""
        writer_class = writers.get_writer_class(writer_name)
        self.writer = writer_class()

    def publish(self, source, destination):
        """
        Run `source` through `self.reader`, then through `self.writer` to
        `destination`.
        """
        document = self.reader.read(source, self.parser)
        self.writer.write(document, destination)


def publish(source=None, destination=None,
            reader=None, reader_name='standalone',
            parser=None, parser_name='restructuredtext',
            writer=None, writer_name='pseudoxml',
            reporter=None, language_code='en',
            warning_level=2, error_level=4, warning_stream=None, debug=0):
    """A convenience function; set up & run a `Publisher`."""
    pub = Publisher(reader, parser, writer, reporter, language_code,
                    warning_level, error_level, warning_stream, debug)
    if reader is None:
        pub.set_reader(reader_name, parser, parser_name)
    if writer is None:
        pub.set_writer(writer_name)
    pub.publish(source, destination)
    
