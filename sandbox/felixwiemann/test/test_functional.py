#!/usr/bin/env python

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""Perform the tests with the data in the functional/ directory.

Read the README.txt files for details on how this is done.

TODO:
One test per file, not one test for all files.
Use publish_file?
Add an option to copy files from output/ to expected/?
"""

import docutils.core
import os
import os.path
import unittest
import difflib

datadir = 'functional'
"""The directory to store the data needed for the functional tests."""

class TestFunctional(unittest.TestCase):
    
    def walker(self, dummy, dirname, names):
        """Process all config files among ``names`` in ``dirname``.

        This is a helper function for os.path.walk.  A config file is
        a Python file (*.py) which sets several veriables."""
        
        for i in filter(lambda x: x.endswith('.py') and not x.startswith('_'),
                        names):
            configfile = os.path.join(dirname, i)
            """Name of current config file to process."""
            
            params = {'settings_overrides': {}}
            """Keyword parameters for publish_string."""
            # Note that settings_overrides has been initialized to an
            # empty dictionary.

            # Read the variables set in the default config file and in
            # the current config file into params.
            execfile(os.path.join(datadir, 'tests', '_default.py'), params)
            execfile(configfile, params)
            
            assert params.has_key('test_source'),\
                   "No 'test_source' supplied in " + configfile
            assert params.has_key('test_destination'),\
                   "No 'test_destination' supplied in " + configfile

            # Set source_path and destination_path if not given.
            params.setdefault(
                'source_path',
                os.path.join(datadir, 'input', params['test_source']))
            params.setdefault(
                'destination_path',
                os.path.join(datadir, 'output', params['test_destination']))
            expected_path = os.path.join(datadir, 'expected',
                                         params['test_destination'])

            # test_source and test_destination aren't needed anymore.
            del params['test_source']
            del params['test_destination']

            # Read the source file into params['source']
            params['source'] = open(params['source_path']).read()

            # Delete private stuff like params['__builtins__'].
            for key in params.keys():
                if key.startswith('_'):
                    del params[key]

            # Get output and write it to the output/ directory.
            output = docutils.core.publish_string(**params)
            open(params['destination_path'], 'w').write(output)

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

    def test_functional(self):
        """Process all config files in functional/tests/."""
        
        os.path.walk(os.path.join(datadir, 'tests'), self.walker, None)

if __name__ == '__main__':
    unittest.main()
