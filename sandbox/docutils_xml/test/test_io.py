# -*- coding: utf-8 -*-

# Copyright (C) 2013 Stefan Merten

# This file is free software; you can redistribute it and/or modify
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
Test I/O support classes for XML.
"""

import unittest

import DocutilsTestSupport

from docutils_xml.io import ZipFileInput

###############################################################################

class HelloZipFileInput(ZipFileInput):

    content = u'Hello world!\n'

    contentUnicode = u'Hello wörld!\n'

    contentPath = "hello.txt"

###############################################################################

class InputTests(unittest.TestCase):

    def test_simple(self):
        input = HelloZipFileInput(source_path="data/hello.zip")
        self.assertEqual(input.read(), HelloZipFileInput.content)

    def test_nonZip(self):
        input = HelloZipFileInput(source='')
        with self.assertRaises(ValueError):
            input.read()

    def test_emptyZip(self):
        input = HelloZipFileInput(source_path="data/empty.zip")
        with self.assertRaises(KeyError):
            input.read()

    def test_undecodedZip(self):
        input = HelloZipFileInput(source_path="data/utf8.zip")
        self.assertEqual(input.read(),
                         HelloZipFileInput.contentUnicode.encode("utf-8"))

    def test_utf8Zip(self):
        input = HelloZipFileInput(source_path="data/utf8.zip", encoding="utf-8")
        self.assertEqual(input.read(), HelloZipFileInput.contentUnicode)

    def test_latin1Zip(self):
        input = HelloZipFileInput(source_path="data/latin1.zip",
                                  encoding="iso-8859-1")
        self.assertEqual(input.read(), HelloZipFileInput.contentUnicode)
