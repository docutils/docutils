#!/usr/bin/env python3
# :Copyright: © 2024 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""Create output samples for TeX to MathML conversion.

Allows to compare the results of the various converters and Docutils' built-in
convert in web browsers.
"""

from pathlib import Path
import sys
import unittest

# Testing the conversion with external converters requires additional
# ressources (latexml, blahtexml, ttm, pandoc) and is SLOW
# (ca. 27s without testing "latexml" and 13 min with "latexml").
# Therefore, tests are skipped unless run as stand-alone module:

if __name__ != '__main__':
    raise unittest.SkipTest('run stand-alone to test external math converters')
else:
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))


from docutils.core import publish_parts, publish_file

from test.test_functional import compare_output

TEST_ROOT = Path(__file__).parents[2]
DOCS = TEST_ROOT.parent / 'docs'
FUNCTIONAL = TEST_ROOT / 'functional'
EXPECTED = FUNCTIONAL / 'expected'
INPUT = FUNCTIONAL / 'input'
OUTPUT = FUNCTIONAL / 'output'


buggy_sample = r"""
There is ":math:`\lambda\omega\tau\varsigma`" to math.

======================  ========== =======
Das ist nicht lustig.   Dafür das  hier

.. math:: So \isses
======================  ========== =======


Das ist :math:`\ein` Fehler.

.. math:: \int_{-\infty} 3 \sin x} \phy \, d\gamma

.. sidebar:: nebenbemerkung

   noch was :math:`\sqrt[2]`

.. math:: \sqrt[2]

2-zeilig mit align:

.. math::
          s_i     & = 3 \\
          s_j + 3 &

"""


math_options = [('mathml', ''),
                ('mathml', 'ttm'),
                ('mathml', 'blahtexml'),
                ('mathml', 'pandoc'),
                # ('mathml', 'latexml'),  # VERY slow
                ]


class MathMLConverterTestCase(unittest.TestCase):
    """
    Functional tests of TeX to MathML converters.

    PROVISIONAL: This class tests a provisional implementation
    which is open to change without warning.
    """
    settings = {'_disable_config': True,
                'input_encoding': 'utf-8',  # skip auto-detection
                'embed_stylesheet': False,
                'warning_stream': '',
                'report_level': 2,  # warning
                # 'report_level': 3,  # error
                # 'report_level': 4,  # severe
                }

    def test_mathematics(self):
        """Test converting "mathematics.txt" from the documentation."""

        source_path = DOCS / 'ref' / 'rst' / 'mathematics.txt'

        for math_output in math_options:
            settings = {'math_output': math_output,
                        'stylesheet_path': 'minimal.css,responsive.css',
                        **self.settings}
            out_file = f'mathematics_{"_".join(math_output).strip("_")}.html'
            out_path = OUTPUT / out_file
            expected_path = EXPECTED / out_file
            output = publish_file(source_path=str(source_path),
                                  destination_path=out_path.as_posix(),
                                  writer='html5',
                                  settings_overrides=settings)
            with self.subTest(converter=math_output[1] or 'latex2mathml()'):
                compare_output(output, out_path, expected_path)

    def test_math_experiments(self):
        """Convert experimental math sample."""

        source_path = INPUT / 'data' / 'math_experiments.txt'

        for math_output in math_options:
            settings = {'math_output': math_output, **self.settings}
            out_file = f'math_experiments_{"_".join(math_output).strip("_")}.html'  # noqa: E501
            out_path = OUTPUT / out_file
            expected_path = EXPECTED / out_file
            output = publish_file(source_path=str(source_path),
                                  destination_path=out_path.as_posix(),
                                  writer='html5',
                                  settings_overrides=settings)
            with self.subTest(converter=math_output[1] or 'latex2mathml()'):
                compare_output(output, out_path, expected_path)

    def test_buggy_math(self):
        """Test reporting conversion failure due to TeX-syntax errors."""

        for math_output in math_options:
            settings = {'math_output': math_output, **self.settings}
            out_file = f'buggy_{"_".join(math_output).strip("_")}.html'
            out_path = OUTPUT / out_file
            expected_path = EXPECTED / out_file
            preface = f'Test "math-output: {" ".join(math_output)}".\n\n'
            parts = publish_parts(preface + buggy_sample,
                                  'buggy-maths',
                                  writer='html5',
                                  settings_overrides=settings)
            Path(out_path).write_text(parts['whole'])
            with self.subTest(converter=math_output[1] or 'latex2mathml()'):
                compare_output(parts['whole'], out_path, expected_path)


if __name__ == '__main__':
    unittest.main()
