#! /usr/bin/env python3
# $Id$
# Author: Günter Milde
# Maintainer: docutils-develop@lists.sourceforge.net
# :Copyright: 2020 Günter Milde,
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""
Miscellaneous LaTeX writer tests.
"""

from pathlib import Path
import sys
import unittest


if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils import core

# TEST_ROOT is ./test/ from the docutils root
TEST_ROOT = Path(__file__).parents[1]
DATA_ROOT = TEST_ROOT / 'data'

sample_toc = """\
.. contents:: TOC

foo
---

bar
---

"""

sample_multiterm = f"""\
.. include:: {DATA_ROOT}/multiple-term-definition.xml
   :parser: xml
"""
expected_multiterm = """
\\begin{description}
\\item[{New in Docutils 0.22}] \n\
A definition list item may contain several
terms with optional classifier(s).

However, there is currently no corresponding
reStructuredText syntax.

\\item[{term 2a}] \n\
\\item[{term 2b}] \n\
definition 2

\\item[{term 3a}] (\\textbf{classifier 3a})
(\\textbf{classifier 3aa})
\\item[{term 3b}] (\\textbf{classifier 3b})
definition 3
\\end{description}
"""


class PublishTestCase(unittest.TestCase):
    maxDiff = None

    settings = {'_disable_config': True,
                # avoid latex writer future warnings:
                'use_latex_citations': False,
                'legacy_column_widths': False,
                }

    def test_publish_from_doctree(self):
        """Ignore the Docutils-generated ToC when ``use_latex_toc`` is True.

        (This did happen when publishing from a doctree.)
        """
        settings = self.settings.copy()
        settings['output_encoding'] = 'unicode'
        settings['warning_stream'] = ''  # don't warn for missing ToC details
        doctree = core.publish_doctree(sample_toc,
                                       settings_overrides=settings)
        result = core.publish_from_doctree(doctree,
                                           writer='latex',
                                           settings_overrides=settings)
        self.assertNotIn(r'\item \hyperref[foo]{foo}', result)
        self.assertIn(r'\tableofcontents', result)

    def test_publish_parts(self):
        """Check for the presence of documented parts.
        """
        parts = core.publish_parts(sample_multiterm,
                                   writer='latex',
                                   settings_overrides=self.settings)
        documented_parts = [
            'abstract',
            'body',
            'body_pre_docinfo',
            'dedication',
            'docinfo',
            'encoding',
            'errors',
            'fallbacks',
            'head_prefix',
            'latex_preamble',
            'pdfsetup',
            'requirements',
            'stylesheet',
            'subtitle',
            'title',
            'titledata',
            'version',
            'whole'
            ]
        self.assertEqual(documented_parts, sorted(parts.keys()))
        self.assertEqual(expected_multiterm, parts['body'])


class WarningsTestCase(unittest.TestCase):

    def test_future_warnings(self):
        """Warn about changing defaults."""
        # Warn only if not set (uncommenting should make test fail):
        settings = {'_disable_config': True,
                    # 'use_latex_citations': False,
                    # 'legacy_column_widths': True,
                    'output_encoding': 'unicode',
                    }
        with self.assertWarnsRegex(FutureWarning,
                                   '"legacy_column_widths" will change'):
            core.publish_string('warnings test', writer='latex',
                                settings_overrides=settings)
        with self.assertWarnsRegex(FutureWarning,
                                   '"use_latex_citations" will change'):
            core.publish_string('warnings test', writer='latex',
                                settings_overrides=settings)


if __name__ == '__main__':
    unittest.main()
