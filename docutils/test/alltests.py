#!/usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
All modules named 'test_*.py' in the current directory, and recursively in
subdirectories (packages) called 'test_*', are loaded and test suites within
are run.
"""

import time
# Start point for actual elapsed time, including imports
# and setup outside of unittest.
start = time.time()

import sys
import os
import docutils


class Tee:

    """Write to a file and a stream (default: stdout) simultaneously."""

    def __init__(self, filename, stream=sys.__stdout__):
        self.file = open(filename, 'w')
        self.stream = stream

    def write(self, string):
        self.stream.write(string)
        self.file.write(string)

    def flush(self):
        self.stream.flush()
        self.file.flush()


def pformat(suite):
    step = 4
    suitestr = repr(suite).replace('=[<', '=[\n<').replace(', ', ',\n')
    indent = 0
    output = []
    for line in suitestr.splitlines():
        output.append(' ' * indent + line)
        if line[-1:] == '[':
            indent += step
        else:
            if line [-5:] == ']>]>,':
                indent -= step * 2
            elif line[-3:] == ']>,':
                indent -= step
    return '\n'.join(output)


# must redirect stderr *before* first import of unittest
sys.stdout = sys.stderr = Tee('alltests.out')

import package_unittest

print ('Testing Docutils %s with Python %s on %s at %s'
       % (docutils.__version__, sys.version.split()[0],
          time.strftime('%Y-%m-%d'), time.strftime('%H:%M:%S')))
sys.stdout.flush()

path, script = os.path.split(sys.argv[0])
suite = package_unittest.loadTestModules(path, 'test_', packages=1)
sys.stdout.flush()

package_unittest.main(suite)
#if package_unittest.verbosity > 1:
#    print >>sys.stderr, pformat(suite) # check the test suite
finish = time.time()

print 'Elapsed time: %.3f seconds' % (finish - start)
