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

__docformat__ = 'reStructuredText'

import unittest

from package import parse_package, NotAPackageException
from transform import make_document

# The following is to ensure that there are .pyc files in the package
# - this is important for testing, since the Python compiler gets quite
# unhappy if given a non-text file to play with (it doesn't like null bytes),
# so we need to do something about that...
import trivial_package

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

        # I've split the wanted result string up into substrings so I can
        # amend it more easily (or so I hope).
        trivial_package = """\
<document source="Package trivial_package">
    <section class="package" id="package-trivial-package" name="package trivial_package">
        <title>
            Package trivial_package\n"""

        # The "xml:space" attribute is by observation, not prediction
        module_init = """\
        <section class="module" id="module-trivial-package-init" name="module trivial_package.__init__">
            <title>
                Module trivial_package.__init__
            <literal_block class="docstring" xml:space="preserve">
                A simple docstring.\n"""

        module_file1 = """\
        <section class="module" id="module-trivial-package-file1" name="module trivial_package.file1">
            <title>
                Module trivial_package.file1
            <literal_block class="docstring" xml:space="preserve">
                This is the first example file. It *does* use reStructuredText.
            <section class="class" id="class-trivial-package-file1-fred" name="class trivial_package.file1.fred">
                <title>
                    Class trivial_package.file1.Fred
                <literal_block class="docstring" xml:space="preserve">
                    An example class - it announces each instance as it is created.\n"""

        module_file2 = """\
        <section class="module" id="module-trivial-package-file2" name="module trivial_package.file2">
            <title>
                Module trivial_package.file2
            <literal_block class="docstring" xml:space="preserve">
                This module is *not* using reStructuredText for its docstrings.\n"""

        non_python_file = """\
        <section class="file" id="file-trivial-package-not-python" name="file trivial_package.not_python">
            <title>
                File trivial_package.not_python
            <paragraph>
                File 
                <literal>
                    not_python
                 is not a Python module.\n"""

        sub_package = """\
        <section class="package" id="package-trivial-package-sub-package" name="package trivial_package.sub_package">
            <title>
                Package trivial_package.sub_package\n"""

        sub_module_init = """\
            <section class="module" id="module-trivial-package-sub-package-init" name="module trivial_package.sub_package.__init__">
                <title>
                    Module trivial_package.sub_package.__init__\n"""

        wanted_result = (trivial_package + module_init + module_file1 +
                         module_file2 + non_python_file + sub_package +
                         sub_module_init)

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
