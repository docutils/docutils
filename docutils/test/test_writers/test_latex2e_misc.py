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

import unittest

from docutils import core

contents_test_input = """\
.. contents:: TOC

foo
---

bar
---

"""


class PublishTestCase(unittest.TestCase):

    settings = {'_disable_config': True,
                # avoid latex writer future warnings:
                'use_latex_citations': False,
                'legacy_column_widths': True,
                }

    def test_publish_from_doctree(self):
        """Ignore the Docutils-generated ToC when ``use_latex_toc`` is True.

        (This did happen when publishing from a doctree.)
        """
        settings = self.settings.copy()
        settings['output_encoding'] = 'unicode'
        settings['warning_stream'] = ''  # don't warn for missing ToC details
        doctree = core.publish_doctree(contents_test_input,
                                       settings_overrides=settings)
        result = core.publish_from_doctree(doctree,
                                           writer_name='latex',
                                           settings_overrides=settings)
        self.assertNotIn(r'\item \hyperref[foo]{foo}', result)
        self.assertIn(r'\tableofcontents', result)

    def test_publish_parts(self):
        """Check for the presence of documented parts.
        """
        parts = core.publish_parts(contents_test_input,
                                   writer_name='latex',
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
            core.publish_string('warnings test', writer_name='latex',
                                settings_overrides=settings)
        with self.assertWarnsRegex(FutureWarning,
                                   '"use_latex_citations" will change'):
            core.publish_string('warnings test', writer_name='latex',
                                settings_overrides=settings)


if __name__ == '__main__':
    unittest.main()
