#!/usr/bin/env python3
# :Copyright: © 2024 Günter Milde.

#             Released without warranty under the terms of the
#             GNU General Public License (v. 2 or later)

# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

from pathlib import Path

from docutils.core import publish_string, publish_file

TEST_ROOT = Path(__file__).parent.parent
EXTRA = TEST_ROOT / 'extra'
FUNCTIONAL = TEST_ROOT / 'functional'
# EXPECTED = FUNCTIONAL / 'expected'
INPUT = FUNCTIONAL / 'input'
OUTPUT = EXTRA / 'output'

sample = r"""
There is ":math:`\lambda\omega\tau\varsigma`" to math.

======================  ========== =======
Das ist nicht lustig.   Dafür das  hier

.. math:: So \isses
======================  ========== =======


Das ist :math:`\ein` Fehler.

.. math:: \int_{-\infty} 3 \sinc x \phy \, d\gamma

.. sidebar:: nebenbemerkung

   noch was :math:`\sqrt[2]`

.. math:: \sqrt[2]

2-zeilig mit align:

.. math::
          s_i     & = 3 \\
          s_j + 3 &

"""

source_path = INPUT / 'data' / 'math.txt'

settings = {'_disable_config': True,
            'report_level': 2,  # warning
            # 'report_level': 3,  # error
            # 'report_level': 4,  # severe
            }

math_options = (['mathml', ''],
                ['mathml', 'ttm'],
                ['mathml', 'blahtexml'],
                ['mathml', 'pandoc'],
                # ['mathml', 'latexml'],  # VERY slow
                )

for math_output in math_options:
    settings['math_output'] = math_output
    out_path = OUTPUT / f'math_output_{"_".join(math_output).strip("_")}.html'
    html = publish_file(source_path=str(source_path),
                        destination_path=str(out_path),
                        writer_name='html5', settings_overrides=settings)

    out_path = OUTPUT / f'buggy_{"_".join(math_output).strip("_")}.html'
    html = publish_string(sample, 'mathml-test', writer_name='html5',
                          settings_overrides=settings)

    with open(out_path, "wb") as fd:
        fd.write(html)
