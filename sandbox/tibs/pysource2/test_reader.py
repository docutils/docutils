#! /usr/bin/env python
"""test_reader.py

Unit tests for the Python source Reader

Initially, this is a standalone test, but ultimately it may be merged into the
mechanisms used for the Docutils self-tests.

:Author:    Tibs
:Contact:   tibs@tibsnjoan.co.uk
:Revision:  $Revision$
:Date:      $Date$
:Copyright: This module has been placed in the public domain.
"""

__docformat__ = 'reStructuredText'

import unittest

from package import parse_package
from transform import make_document
from reader import Reader
from docutils.core import publish_string
from docutils.readers.python.moduleparser import parse_module

class PackageTest(unittest.TestCase):

    def testReader(self):
        """Test the reader works as expected
        """
        reader = Reader()

        source="# A Python comment"
        source_path="test.py"

        # Hmm - extra debugging info...
        publish_string = publish_string_with_traceback

        actual_result = publish_string(reader=reader,reader_name="python",
                                       parser_name="restructuredtext",
                                       writer_name="pseudoxml",
                                       source=source, source_path=source_path)

        wanted_result = """\
<document source="Module test">
    <section class="module" id="module-test" name="module test">
        <title>
            Module test\n"""


        if wanted_result != actual_result:
            print "+++++++++++++++++++++++++ WANT"
            print wanted_result
            print "+++++++++++++++++++++++++ GOT"
            print actual_result
            print "+++++++++++++++++++++++++"

        self.assertEqual(actual_result,wanted_result)

    def testTool(self):
        """Trying to think what to do for packages"""
        # The Reader interface is designed to work with single test entities,
        # either a string or the content of a text file (i.e., a single thing
        # that can be accessed via some sort of "read" method).
        # This doesn't work for packages, where one has multiple files.
        # Thus I suspect that the Reader interface is not appropriate for
        # what I want to do (at least, not without doing it unnecessary
        # violence and making it a lot more complicated).
        # So I need to do things "by hand"...

        source="# A Python comment"
        source_path="test.py"

        # Since a body of text is a Module, not a Package, we'll go straight
        # to it
        nodes = parse_module(source,source_path)

        # That then needs converting to a docutils tree
        document = make_document(nodes)

        # And *that* wants converting to the appropriate output format

        from docutils.writers.pseudoxml import Writer
        writer = Writer()
        writer.document = document
        writer.translate()
        actual_result = writer.output

        wanted_result = """\
<document source="Module test">
    <section class="module" id="module-test" name="module test">
        <title>
            Module test\n"""


        if wanted_result != actual_result:
            print "+++++++++++++++++++++++++ WANT"
            print wanted_result
            print "+++++++++++++++++++++++++ GOT"
            print actual_result
            print "+++++++++++++++++++++++++"

        self.assertEqual(actual_result,wanted_result)


def publish_string_with_traceback(reader=None,reader_name=None,
                                  parser_name=None,writer_name=None,
                                  source=None,source_path=None):
    """A modified version of publish_string, so I can request traceback.
    """
    from docutils.core import Publisher
    from docutils import io
    pub = Publisher(reader=reader,
                    source_class=io.StringInput,
                    destination_class=io.StringOutput)
    pub.set_components(reader_name="python",
                       parser_name="restructuredtext",
                       writer_name="pseudoxml")

    pub.process_command_line(argv=["--traceback"])

    pub.set_source(source=source, source_path=source_path)
    return pub.publish(enable_exit=False)


if __name__ == "__main__":
    unittest.main()
