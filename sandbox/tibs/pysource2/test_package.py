#! /usr/bin/env python
"""test_package.py

Unit tests for parsing packages for pysource.

Initially, this is a standalone test, but ultimately it may be merged into the
mechanisms used for the Docutils self-tests.

:Author:    Tibs
:Contact:   tibs@tibsnjoan.co.uk
:Revision:  $Revision$
:Date:      $Date$
:Copyright: This module has been placed in the public domain.
"""

import unittest

from package import parse_package, NotAPackageException
from transform import make_document

class PackageTest(unittest.TestCase):

    def testNoSuchDirectory(self):
        """Not a package - no such directory.
        """

        self.assertRaises(OSError,
                          parse_package,
                          "no_such_directory")

    def testNotADirectory(self):
        """Not a package - file is not a directory.
        """

        self.assertRaises(OSError,
                          parse_package,
                          "not_a_directory")

    def testNotAPackage(self):
        """Not a package - directory is empty.
        """

        self.assertRaises(NotAPackageException,
                          parse_package,
                          "not_a_package")

    def testPackage(self):
        """A package containing subpackage(s)

        The directory is called "trivial_package" for historical reasons.
        """

        wanted_result = """\
<Package filename="trivial_package">
    <Module filename="__init__.py">
        <Docstring>
            A simple docstring.
    <Module filename="file1.py">
        <Docstring>
            This is the first example file. It *does* use reStructuredText.
        <Attribute lineno="5" name="__docformat__">
            <Expression lineno="5">
                "reST"
        <Import lineno="7">
            os
        <Class lineno="9" name="Fred">
            <Docstring lineno="9">
                An example class - it announces each instance as it is created.
            <Method lineno="13" name="__init__">
                <ParameterList lineno="13">
                    <Parameter lineno="13" name="self">
    <Module filename="file2.py">
        <Docstring>
            This module is *not* using reStructuredText for its docstrings.
    <NotPython filename="not_python">
    <Package filename="sub_package">
        <Module filename="__init__.py">\n"""

        actual_result = str(parse_package("trivial_package"))

        if wanted_result != actual_result:
            print "+++++++++++++++++++++++++ WANT"
            print wanted_result
            print "+++++++++++++++++++++++++ GOT"
            print actual_result
            print "+++++++++++++++++++++++++"

        self.assertEqual(actual_result,wanted_result)

    def testMakeDocument(self):
        """
        Turn our Package tree into a docutils Document.
        """

        wanted_result = """\
<document source="Package trivial_package">
    <section id="package-trivial-package" name="package trivial_package">
        <title>
            Package trivial_package
        <section id="package-trivial-package-sub-package" name="package trivial_package.sub_package">
            <title>
                Package trivial_package.sub_package
"""

        tree = parse_package("trivial_package")

        document = make_document(tree)

        actual_result = document.pformat()

        if wanted_result != actual_result:
            print "+++++++++++++++++++++++++ WANT"
            print wanted_result
            print "+++++++++++++++++++++++++ GOT"
            print actual_result
            print "+++++++++++++++++++++++++"

        self.assertEqual(actual_result,wanted_result)


if __name__ == "__main__":
    unittest.main()

