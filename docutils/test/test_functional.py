#!/usr/bin/env python

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Perform tests with the data in the functional/ directory.

Read README.txt for details on how this is done.
"""

import sys
import os
import os.path
import unittest
import difflib
import docutils
import docutils.core
import DocutilsTestSupport


# Ugly magic to determine the test directory.
testroot = os.path.dirname(DocutilsTestSupport.__file__) or '.'

datadir = 'functional'
"""The directory to store the data needed for the functional tests."""


def join_path(*args):
    return '/'.join(args) or '.'


class FunctionalTestSuite(DocutilsTestSupport.CustomTestSuite):

    """Test suite containing test cases for all config files."""

    def __init__(self):
        """Process all config files in functional/tests/."""
        DocutilsTestSupport.CustomTestSuite.__init__(self)
        os.chdir(testroot)
        self.added = 0
        os.path.walk(join_path(datadir, 'tests'), self.walker, None)
        assert self.added, 'No functional tests found.'

    def walker(self, dummy, dirname, names):
        """
        Process all config files among `names` in `dirname`.

        This is a helper function for os.path.walk.  A config file is
        a Python file (*.py) which sets several variables.
        """
        for name in names:
            if name.endswith('.py') and not name.startswith('_'):
                config_file_full_path = join_path(dirname, name)
                self.addTestCase(FunctionalTestCase, 'test', None, None,
                                 id=config_file_full_path,
                                 configfile=config_file_full_path)
                self.added += 1


class FunctionalTestCase(DocutilsTestSupport.CustomTestCase):

    """Test case for one config file."""

    def __init__(self, *args, **kwargs):
        """Set self.configfile, pass arguments to parent __init__."""
        self.configfile = kwargs['configfile']
        del kwargs['configfile']
        DocutilsTestSupport.CustomTestCase.__init__(self, *args, **kwargs)

    def shortDescription(self):
        return 'test_functional.py: ' + self.configfile

    def test(self):
        """Process self.configfile."""
        os.chdir(testroot)
        # Keyword parameters for publish_file:
        params = {}
        # Initialize 'settings_overrides' for test settings scripts,
        # and disable configuration files:
        params['settings_overrides'] = {'_disable_config': 1}
        # Read the variables set in the default config file and in
        # the current config file into params:
        execfile(join_path(datadir, 'tests', '_default.py'), params)
        execfile(self.configfile, params)
        # Check for required settings:
        assert params.has_key('test_source'),\
               "No 'test_source' supplied in " + self.configfile
        assert params.has_key('test_destination'),\
               "No 'test_destination' supplied in " + self.configfile
        # Set source_path and destination_path if not given:
        params.setdefault('source_path',
                          join_path(datadir, 'input',
                                       params['test_source']))
        # Path for actual output:
        params.setdefault('destination_path',
                          join_path(datadir, 'output',
                                       params['test_destination']))
        # Path for expected output:
        expected_path = join_path(datadir, 'expected',
                                     params['test_destination'])
        # test_source and test_destination aren't needed any more:
        del params['test_source']
        del params['test_destination']
        # Delete private stuff like params['__builtins__']:
        for key in params.keys():
            if key.startswith('_'):
                del params[key]
        # Get output (automatically written to the output/ directory
        # by publish_file):
        output = docutils.core.publish_file(**params)
        # Get the expected output *after* writing the actual output.
        self.assert_(os.access(expected_path, os.R_OK),\
                     'Cannot find expected output at\n' + expected_path)
        expected = open(expected_path).read()
        diff = ('Please compare the expected and actual output files:\n'
                'diff %s %s\n' % (expected_path, params['destination_path']))
        try:
            self.assertEquals(output, expected, diff)
        except AssertionError:
            if hasattr(difflib, 'unified_diff'):
                # Generate diff if unified_diff available:
                diff = ''.join(
                    difflib.unified_diff(expected.splitlines(1),
                                         output.splitlines(1),
                                         expected_path,
                                         params['destination_path']))
            print >>sys.stderr, '\n%s:' % (self,)
            print >>sys.stderr, diff
            raise


def suite():
    return FunctionalTestSuite()


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
