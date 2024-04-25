#!/usr/bin/env python3
# :Copyright: © 2024 Günter Milde.
#
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause
"""
Test module for docutils/utils/math/__init__.py.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))

from docutils.utils import math

TEST_ROOT = Path(__file__).parents[2]  # ./test/ from the docutils root


class MathTests(unittest.TestCase):

    tests = [('a + b = c', 'equation*'),
             (r'x = \begin{cases} -1&x<0 \\ 1&x>0 \end{cases}', 'equation*'),
             (r'a + 2b = c \\ 3a + b = 2c', 'align*'),
             ]

    def test_pick_math_environment(self):
        for sample, result in self.tests:
            self.assertEqual(math.pick_math_environment(sample), result)

    def test_wrap_math_code(self):
        for sample, env in self.tests:
            self.assertEqual(math.wrap_math_code(sample, as_block=False),
                             f'${sample}$')
            self.assertEqual(math.wrap_math_code(sample, as_block=True),
                             f'\\begin{{{env}}}\n'
                             f'{sample}\n'
                             f'\\end{{{env}}}')


if __name__ == '__main__':
    unittest.main()
