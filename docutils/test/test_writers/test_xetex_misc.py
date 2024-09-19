#! /usr/bin/env python3
# $Id$
# Author: Günter Milde
# Maintainer: docutils-develop@lists.sourceforge.net
# :Copyright: 2024 Günter Milde,
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""
Miscellaneous XeTeX/LuaTeX writer tests.
"""

from pathlib import Path
import sys
import unittest


if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils import core
from docutils.writers import xetex

# TEST_ROOT is ./test/ from the docutils root
TEST_ROOT = Path(__file__).parents[1]
DATA_ROOT = TEST_ROOT / 'data'

px_sample = """\
.. image:: foo.pdf
   :width: 250 px
   :height: 50pt
"""

px_body = r"""
\includegraphics[height=50bp,width=250\pdfpxdimen]{foo.pdf}
"""

px_fallback = r"""
% Provide a length variable and set default, if it is new
\providecommand*{\DUprovidelength}[2]{
  \ifthenelse{\isundefined{#1}}{\newlength{#1}\setlength{#1}{#2}}{}
}

\DUprovidelength{\pdfpxdimen}{1bp}


"""


class PublishTestCase(unittest.TestCase):
    maxDiff = None

    settings = {'_disable_config': True,
                # avoid latex writer future warnings:
                'use_latex_citations': False,
                'legacy_column_widths': False,
                }

    def test_px_workaround(self):
        """Check the workaround for length unit 'px' missing in XeTeX.
        """
        parts = core.publish_parts(px_sample,
                                   writer=xetex.Writer(),
                                   settings_overrides=self.settings)
        self.assertEqual(px_body, parts['body'])
        self.assertEqual(px_fallback, parts['fallbacks'])


if __name__ == '__main__':
    unittest.main()
