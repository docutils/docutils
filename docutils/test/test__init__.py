#! /usr/bin/env python
# -*- coding: utf-8 -*-
# $Id$
# Authors: Günter Milde <milde@users.sourceforge.net>,
#          David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Test module for the docutils' __init__.py.
"""

import unittest
import sys
import DocutilsTestSupport              # must be imported before docutils
import docutils
import docutils.utils


class ApplicationErrorTests(unittest.TestCase):

    def test_message(self):
        err = docutils.ApplicationError('the message')
        self.assertEqual(unicode(err), u'the message')

    def test_non_ASCII_message(self):
        err = docutils.ApplicationError(u'\u0169')
        self.assertEqual(unicode(err), u'\u0169')


class VersionInfoTests(unittest.TestCase):

    def test__version_info__(self):
        self.assertEqual(len(docutils.__version_info__), 6)
        self.assertEqual(type(docutils.__version_info__.major), int)
        self.assertEqual(type(docutils.__version_info__.minor), int)
        self.assertEqual(type(docutils.__version_info__.micro), int)
        self.assertEqual(type(docutils.__version_info__.releaselevel), str)
        self.assertEqual(type(docutils.__version_info__.serial), int)
        self.assertEqual(type(docutils.__version_info__.release), bool)

        releaselevels = ('alpha', 'beta', 'candidate', 'final')
        self.assertTrue(
            docutils.__version_info__.releaselevel in releaselevels)

        if (docutils.__version_info__.releaselevel == 'final'):
            # releaselevel 'final' must not be used with development versions
            # (leads to wrong version ordering of the related __version__):
            #
            #   Within a numeric release (1.0, 2.7.3), the following suffixes
            #   are permitted and MUST be ordered as shown:
            #
            #      .devN, aN, bN, rcN, <no suffix>, .postN
            #
            # https://www.python.org/dev/peps/pep-0440/#summary-of-permitted-suffixes-and-relative-ordering
            self.assertTrue(docutils.__version_info__.release)
            # pre-release serial number must be 0 for final releases:
            self.assertEqual(type(docutils.__version_info__.serial), 0)

    def test__version__(self):
        """Test that __version__ is equivalent to __version_info__."""
        self.assertEqual(
            docutils.utils.version_identifier(docutils.__version_info__),
            docutils.__version__)


if __name__ == '__main__':
    unittest.main()
