#! /usr/bin/env python3
# $Id$
# Authors: Günter Milde <milde@users.sf.net>,
#          David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Test module for the docutils' __init__.py.
"""

from __future__ import annotations

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import docutils
import docutils.utils
from docutils import VersionInfo


_RELEASE_LEVEL_ABBREVIATIONS: dict[str, str] = {
    'alpha': 'a',
    'beta': 'b',
    'candidate': 'rc',
    'final': '',
}


def version_identifier(version_info: VersionInfo) -> str:
    """
    Return a version identifier string built from `version_info`, a
    `docutils.VersionInfo` namedtuple instance or compatible tuple. If
    `version_info` is not provided, by default return a version identifier
    string based on `docutils.__version_info__` (i.e. the current Docutils
    version).
    """
    if version_info.micro:
        micro = f'.{version_info.micro}'
    else:
        # 0 is omitted:
        micro = ''
    releaselevel = _RELEASE_LEVEL_ABBREVIATIONS[version_info.releaselevel]
    if version_info.serial:
        serial = version_info.serial
    else:
        # 0 is omitted:
        serial = ''
    if version_info.release:
        dev = ''
    else:
        dev = '.dev'
    version = (f'{version_info.major}.{version_info.minor}{micro}'
               f'{releaselevel}{serial}{dev}')
    return version


class ApplicationErrorTests(unittest.TestCase):

    def test_message(self):
        err = docutils.ApplicationError('the message')
        self.assertEqual('the message', str(err))

    def test_non_ASCII_message(self):
        err = docutils.ApplicationError('\u0169')
        self.assertEqual('\u0169', str(err))


class VersionInfoTests(unittest.TestCase):

    def test_VersionInfo(self):
        # arguments may use keywords
        self.assertEqual(VersionInfo(0, 1, 2, 'beta', 3, False),
                         VersionInfo(major=0, minor=1, micro=2,
                                     releaselevel='beta', serial=3,
                                     release=False))
        # check defaults:
        self.assertEqual(VersionInfo(),
                         VersionInfo(0, 0, 0, releaselevel='final',
                                     serial=0, release=True))

    def test_VersionInfo_value_check(self):
        # releaselevel must be one of ('alpha', 'beta', 'candidate', 'final')
        with self.assertRaises(ValueError):
            VersionInfo(0, 1, 0, 'gamma')
        # releaselevel 'final' must not be used with development versions
        with self.assertRaises(ValueError):
            VersionInfo(0, 1, releaselevel='final', release=False)
        # "serial" must be 0 for final releases'
        with self.assertRaises(ValueError):
            VersionInfo(0, 1, releaselevel='final', serial=1)

    def test__version_info__(self):
        """Ensure that the current __version_info__ is valid."""
        releaselevels = ('alpha', 'beta', 'candidate', 'final')

        self.assertEqual(len(docutils.__version_info__), 6)
        self.assertEqual(type(docutils.__version_info__.major), int)
        self.assertEqual(type(docutils.__version_info__.minor), int)
        self.assertEqual(type(docutils.__version_info__.micro), int)
        self.assertTrue(
            docutils.__version_info__.releaselevel in releaselevels)
        self.assertEqual(type(docutils.__version_info__.serial), int)
        self.assertEqual(type(docutils.__version_info__.release), bool)

    def test__version__(self):
        """Test that __version__ is equivalent to __version_info__."""
        self.assertEqual(
            version_identifier(docutils.__version_info__),
            docutils.__version__, f'{docutils.__version_info__} differs')

    def test_version_info_comparing(self):
        """Test comparing of __version_info__ instances."""

        # Example development cycle:
        devcycle = ('0.1a.dev 0.1a 0.1a1.dev '
                    '0.1b.dev 0.1b 0.1b1.dev '
                    '0.1rc1.dev 0.1rc1 0.1rc2.dev '
                    '0.1 0.1.1b.dev 0.1.1b 0.1.1 '
                    '0.2b.dev 0.2b 0.2')
        # corresponding version_info values:
        versioninfos = [VersionInfo(0, 1, 0, 'alpha', 0, False),
                        VersionInfo(0, 1, 0, 'alpha', 0, True),
                        VersionInfo(0, 1, 0, 'alpha', 1, False),
                        VersionInfo(0, 1, 0, 'beta', 0, False),
                        VersionInfo(0, 1, 0, 'beta', 0, True),
                        VersionInfo(0, 1, 0, 'beta', 1, False),
                        VersionInfo(0, 1, 0, 'candidate', 1, False),
                        VersionInfo(0, 1, 0, 'candidate', 1, True),
                        VersionInfo(0, 1, 0, 'candidate', 2, False),
                        VersionInfo(0, 1, 0, 'final', 0, True),
                        VersionInfo(0, 1, 1, 'beta', 0, False),
                        VersionInfo(0, 1, 1, 'beta', 0, True),
                        VersionInfo(0, 1, 1, 'final', 0, True),
                        VersionInfo(0, 2, 0, 'beta', 0, False),
                        VersionInfo(0, 2, 0, 'beta', 0, True),
                        VersionInfo(0, 2, 0, 'final', 0, True),
                        ]
        # transform to version strings
        versions = [version_identifier(vinfo) for vinfo in versioninfos]

        # ensure version infos corresponding to the dev cycle are ascending
        self.assertEqual(versions, devcycle.split())
        self.assertEqual(versioninfos, sorted(versioninfos))

    def test_version_info_tuple_comparing(self):
        """Test comparing of __version_info__ instances to tuples."""

        # {<version string>: <version info>}
        v01b = VersionInfo(0, 1, 0, 'beta', 0, True)
        v01 = VersionInfo(0, 1, 0, 'final', 0, True)
        v02b_dev = VersionInfo(0, 2, 0, 'beta', 0, False)

        # compare to ordinary tuples:
        self.assertTrue(v01b < (0, 2))
        self.assertTrue((0, 2) > v01b)
        self.assertTrue(v01b < (0, 1))
        self.assertTrue((0, 1) > v01b)
        self.assertTrue(v01 <= (0, 1))
        self.assertTrue(v01 >= (0, 1))
        self.assertTrue((0, 1) <= v01)
        self.assertTrue((0, 1) >= v01)
        self.assertTrue(v02b_dev > (0, 1))
        self.assertTrue((0, 1) < v02b_dev)
        # Test for equality requires complete tuple, because __eg__() is
        # not called when comparing a namedtuple subclass to a tuple:
        self.assertTrue((0, 1, 0, 'final', 0, True) == v01)
        # self.assertTrue((0, 1) == v01) # fails
        # self.assertTrue(v01 == (0, 1)) # fails


if __name__ == '__main__':
    unittest.main()
