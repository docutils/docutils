#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

I/O classes provide a uniform API for low-level input and output.  Subclasses
will exist for a variety of input/output mechanisms.
"""

__docformat__ = 'reStructuredText'

import sys
import locale


class IO:

    """
    Base class for abstract input/output wrappers.
    """

    source = None
    destination = None

    def __init__(self, options, source=None, destination=None):
        """
        :Parameters:
            - `options`: a `docutils.optik.Values` object.
            - `source`: identifies the source of input data.
            - `destination`: identifies the destination for output data.
        """
        self.options = options

    def __repr__(self):
        return '%s: source=%r, destination=%r' % (self.__class__, self.source,
                                                  self.destination)

    def read(self, reader):
        raise NotImplementedError

    def write(self, data):
        raise NotImplementedError

    def decode(self, data):
        """
        Decode a string, `data`, heuristically.
        Raise UnicodeError if unsuccessful.
        """
        encodings = [self.options.input_encoding,
                     locale.getlocale()[1],
                     'utf-8',
                     locale.getdefaultlocale()[1],]
        # is locale.getdefaultlocale() platform-specific?
        for enc in encodings:
            if not enc:
                continue
            try:
                decoded = unicode(data, enc)
                return decoded
            except UnicodeError:
                pass
        raise UnicodeError(
            'Unable to decode input data.  Tried the following encodings: %s.'
            % ', '.join([repr(enc) for enc in encodings if enc]))


class FileIO(IO):

    """
    IO for single, simple files.
    """

    def __init__(self, options, source=None, destination=None):
        """
        :Parameters:
            - `source`: one of (a) a file-like object, which is read directly;
              (b) a path to a file, which is opened and then read; or (c)
              `None`, which implies `sys.stdin`.
            - `destination`: one of (a) a file-like object, which is written
              directly; (b) a path to a file, which is opened and then
              written; or (c) `None`, which implies `sys.stdout`.
        """
        IO.__init__(self, options)
        if hasattr(source, 'read'):
            self.source = source
        elif source is None:
            self.source = sys.stdin
        else:
            self.source = open(source)
        if hasattr(destination, 'write'):
            self.destination = destination
        elif destination is None:
            self.destination = sys.stdout
        else:
            self.destination = open(destination, 'w')

    def read(self, reader):
        """
        Read and decode a single file and return the data.
        """
        data = self.source.read()
        return self.decode(data)

    def write(self, data):
        """
        Encode and write `data` to a single file.
        """
        output = data.encode(self.options.output_encoding)
        self.destination.write(output)


class StringIO(IO):

    """
    Direct string IO.
    """

    def __init__(self, options, source=None, destination=None):
        """
        :Parameters:
            - `source`: a string containing input data.
            - `destination`: not used.
        """
        IO.__init__(self, options)
        self.source = source

    def read(self, reader):
        """
        Decode and return the source string.
        """
        return self.decode(self.source)

    def write(self, data):
        """
        Encode and return `data`.
        """
        self.destination = data.encode(self.options.output_encoding)
        return self.destination
