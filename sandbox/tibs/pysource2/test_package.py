"""test_package.py

Unit tests for parsing packages for pysource.

Initially, this is a standalone test, but ultimately it may be merge into the
mechanisms used for the Docutils self-tests.

:Author:    Tibs
:Contact:   tibs@tibsnjoan.co.uk
:Revision:  $Revision$
:Date:      $Date$
:Copyright: This module has been placed in the public domain.
"""

import unittest
from package import parse_package, NotAPackageException, \
     NoSuchDirectoryException

class PackageTest(unittest.TestCase):

    def testNoSuchDirectory(self):
        """Not a package - no such directory.
        """

        self.assertRaises(NoSuchDirectoryException,
                          parse_package,
                          "no_such_directory")

    def testNotADirectory(self):
        """Not a package - file is not a directory.
        """

        self.assertRaises(NotAPackageException,
                          parse_package,
                          "not_a_directory")

    def testNotAPackage(self):
        """Not a package - directory is empty.
        """

        self.assertRaises(NotAPackageException,
                          parse_package,
                          "not_a_package")

    def testTrivialPackage(self):
        """Trivial package(s) - only empty __init__.py files.
        """

        self.assertEqual(str(parse_package("trivial_package")),
                         """\
<Package filename="trivial_package">
    <Package filename="sub_package">\n""")


if __name__ == "__main__":
    unittest.main()

