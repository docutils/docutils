#!/usr/bin/env python

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""Perform tests with the data in the functional/ directory.

Read README.txt for details on how this is done."""

import docutils.core
import os
import os.path
import unittest
import difflib
import DocutilsTestSupport

datadir = 'functional'
"""The directory to store the data needed for the functional tests."""

class FunctionalTestSuite(DocutilsTestSupport.CustomTestSuite):

    """Test suite containing test cases for all config files."""

    def __init__(self):
        """Process all config files in functional/tests/."""
        DocutilsTestSupport.CustomTestSuite.__init__(self)
        os.path.walk(os.path.join(datadir, 'tests'), self.walker, None)
    
    def walker(self, dummy, dirname, names):
        """Process all config files among ``names`` in ``dirname``.

        This is a helper function for os.path.walk.  A config file is
        a Python file (*.py) which sets several veriables."""
        
        for i in filter(lambda x: x.endswith('.py') and not
                        x.startswith('_'), names):
            
            c = os.path.join(dirname, i)
            """Full path of configuration file."""
            
            self.addTestCase(
                FunctionalTestCase, 'test', None, None, id=c, configfile=c)
            
class FunctionalTestCase(DocutilsTestSupport.CustomTestCase):

    """Test case for one config file."""

    def __init__(self, *args, **kwargs):
        """Set self.configfile, pass arguments to parent __init__."""
        self.configfile = kwargs['configfile']
        del kwargs['configfile']
        self.shortDescription = self.getdesc
        DocutilsTestSupport.CustomTestCase.__init__(self, *args, **kwargs)

    def getdesc(self):
        """Ugly hack for Python 2.1."""
        return 'Functional test for ' + self.configfile

    def test(self):

        """Process self.configfile."""
    
        params = {'settings_overrides': {}}
        """Keyword parameters for publish_file."""
        # Note that settings_overrides has been initialized to an
        # empty dictionary.

        # Read the variables set in the default config file and in
        # the current config file into params.
        execfile(os.path.join(datadir, 'tests', '_default.py'), params)
        execfile(self.configfile, params)

        assert params.has_key('test_source'),\
               "No 'test_source' supplied in " + self.configfile
        assert params.has_key('test_destination'),\
               "No 'test_destination' supplied in " + self.configfile

        # Set source_path and destination_path if not given.
        params.setdefault(
            'source_path',
            os.path.join(datadir, 'input', params['test_source']))
        params.setdefault(
            'destination_path',
            os.path.join(datadir, 'output', params['test_destination']))
        """Path for actual output."""
        
        expected_path = os.path.join(datadir, 'expected',
                                     params['test_destination'])
        """Path for expected output."""

        # test_source and test_destination aren't needed anymore.
        del params['test_source']
        del params['test_destination']

        # Delete private stuff like params['__builtins__'].
        for key in params.keys():
            if key.startswith('_'):
                del params[key]

        # Get output.  (Automatically written to the output/ directory
        # by publish_file.)
        output = docutils.core.publish_file(**params)

        # Get the expected output *after* writing the actual output.
        self.assert_(os.access(expected_path, os.R_OK),\
                     'Cannot find expected output at\n' + expected_path)
        expected = open(expected_path).read()
        # Generate diff if unified_diff available.
        if hasattr(difflib, 'unified_diff'):
            diff = ('Functional test failed:\n\n' +
                    ''.join(difflib.unified_diff(
                expected.splitlines(1),
                output.splitlines(1),
                expected_path,
                params['destination_path'])))
        else:
            diff = ('Functional test failed. '
                    'Please compare the following files:\n%s\n%s\n' %
                    (expected_path, params['destination_path']))
        self.assertEqual(output, expected, diff)

def suite():
    return FunctionalTestSuite()

if __name__ == '__main__':
    unittest.main(defaultTest='suite')
