#! /usr/bin/env python
# coding: utf-8

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
from __future__ import absolute_import

import os

if __name__ == '__main__':
    import __init__
from test_writers import DocutilsTestSupport
from docutils import core

contents_test_input = """\
.. contents:: TOC

foo
---

bar
---

"""

class TocTestCase(DocutilsTestSupport.StandardTestCase):

    def test_publish_from_doctree(self):
        """Ignore the Docutils-generated ToC, when ``use_latex_toc``
        is True. (This did happen when publishing from a doctree.)
        """
        settings_overrides={'output_encoding': 'unicode',
                            '_disable_config': True,}
        doctree = core.publish_doctree(contents_test_input,
                                       settings_overrides=settings_overrides)
        result = core.publish_from_doctree(doctree,
                                     writer_name='latex',
                                     settings_overrides=settings_overrides)
        self.assertNotIn(r'\item \hyperref[foo]{foo}', result)
        # self.assertIn(r'\tableofcontents', result)


if __name__ == '__main__':
    import unittest
    unittest.main()
