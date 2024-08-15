#!/usr/bin/env python3
# :Copyright: © 2022 Günter Milde.

# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

# :Id: $Id$

"""
Test module for the command line interface.
"""

import difflib
from io import StringIO
import locale
from pathlib import Path
import os
import re
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from docutils import __main__, core

# DATA_ROOT is ./test/data/ from the docutils root
DATA_ROOT = Path(__file__).parent / 'data'


def print_mismatch(expected, output):
    diff = ''.join(difflib.unified_diff(
                       expected.splitlines(keepends=True),
                       output.splitlines(keepends=True),
                       'expected', 'output'))
    raise AssertionError('Unexpected output:\n'+diff)


class CliTests(unittest.TestCase):

    def setUp(self):
        # save state
        self.orig_argv = sys.argv
        self.orig_stdout = sys.stdout
        os.environ['DOCUTILSCONFIG'] = ''  # don't read config files
        sys.stdout = StringIO()  # re-direct sys.stdout

    def tearDown(self):
        del os.environ['DOCUTILSCONFIG']
        sys.stdout = self.orig_stdout
        sys.argv = self.orig_argv
        # restore default locale settings:
        if sys.platform != "win32":
            locale.setlocale(locale.LC_MESSAGES, 'C')
        locale.setlocale(locale.LC_TIME, 'C')

    def get_help_text(self, prog, entry_point):
        # call entry_point function and collect help text
        sys.argv = [prog, '--help']
        try:
            entry_point()
        except SystemExit:
            pass
        output = sys.stdout.getvalue()
        # replace unpredictable paths (eventually wrapped)
        output = re.sub(r'default:[^)]*[/\\][^)]*\)', 'default: [...])',
                        output, flags=re.DOTALL)
        # normalise error encoding default
        output = output.replace(
            f'{core.OptionParser.default_error_encoding}:backslashreplace',
            'utf-8:backslashreplace')
        return output

    def test_main_help(self):
        # collect help text
        output = self.get_help_text('docutils', __main__.main)

        # compare to stored version
        docutils_txt = os.path.join(DATA_ROOT, 'help/docutils.rst')
        expected = Path(docutils_txt).read_text(encoding='utf-8')
        if expected != output:
            print_mismatch(expected, output)

    def test_rst2html_help(self):
        # collect help text
        output = self.get_help_text('rst2html', core.rst2html)
        # compare to stored version
        rst2html_txt = os.path.join(DATA_ROOT, 'help/rst2html.rst')
        expected = Path(rst2html_txt).read_text(encoding='utf-8')
        if expected != output:
            print_mismatch(expected, output)

    def test_rst2latex_help(self):
        # collect help text
        output = self.get_help_text('rst2latex', core.rst2latex)
        # compare to stored version
        rst2latex_txt = os.path.join(DATA_ROOT, 'help/rst2latex.rst')
        expected = Path(rst2latex_txt).read_text(encoding='utf-8')
        if expected != output:
            print_mismatch(expected, output)


if __name__ == '__main__':
    unittest.main()
