#! /usr/bin/env python

# Copyright (C) 2013 Stefan Merten

# odf2docutils.py is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License,
# or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

"""
Classes useful for common I/O use cases for XML.
"""

__docformat__ = 'reStructuredText' # Formatted to be rendered by epydoc

###############################################################################
###############################################################################
# Import

import zipfile

import docutils.io

###############################################################################
###############################################################################
# Constants

###############################################################################
###############################################################################
# Variables

###############################################################################
###############################################################################
# Functions

###############################################################################
###############################################################################
# Classes

class ZipFileInput(docutils.io.FileInput):
    """
    Generic input class for ZIP files. Useful for XML because some formats
    bundle a number of XML files in a ZIP file.
    """

    contentPath = None
    """
    :type: str

    Path to the file inside the ZIP file to return for reading.

    Override in subclasses.
    """

    def __init__(self, source=None, source_path=None, 
                 encoding=None, error_handler='strict', 
                 autoclose=True, handle_io_errors=None, mode=None):
        """
        :Parameters:

          encoding : str | None
            Encoding of `contentPath` or None to return content undecoded.
        """
        docutils.io.FileInput.__init__(self, source, source_path,
                                       encoding=encoding,
                                       error_handler='strict',
                                       autoclose=autoclose,
                                       handle_io_errors=handle_io_errors,
                                       mode='rb')

    def read(self):
        if not zipfile.is_zipfile(self.source):
            raise ValueError("%r is not a valid ZIP input file"
                             % ( self.source_path, ))
        inZip = zipfile.ZipFile(self.source)
        data = inZip.read(self.contentPath)
        if self.encoding:
            return self.decode(data)
        else:
            return data

    def readlines(self):
        """
        Usually makes no sense for XML based input. Subclasses may override
        this, however.
        """

        raise ValueError("Can not read lines from input")
