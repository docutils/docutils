#! /usr/bin/env python3
# $Id$
# Authors: Lea Wiemann, Dmitry Shachnev, Günter Milde
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""
Miscellaneous HTML writer tests.
"""

import os
from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils import core
from docutils.writers import html5_polyglot

# TEST_ROOT is ./test/ from the docutils root
TEST_ROOT = Path(__file__).parents[1]
DATA_ROOT = TEST_ROOT / 'data'


def relpath(path):
    return os.path.relpath(path, os.getcwd()).replace('\\', '/')


class EncodingTestCase(unittest.TestCase):

    def test_xmlcharrefreplace(self):
        # Test that xmlcharrefreplace is the default output encoding
        # error handler.
        settings_overrides = {
            'output_encoding': 'latin1',
            'stylesheet_path': '',
            '_disable_config': True}
        result = core.publish_string(
            'EUR = \u20ac', writer=html5_polyglot.Writer(),
            settings_overrides=settings_overrides)
        # Encoding a euro sign with latin1 doesn't work, so the
        # xmlcharrefreplace handler is used.
        self.assertIn(b'EUR = &#8364;', result)


class MovingArgsTestCase(unittest.TestCase):

    mys = {'stylesheet_path': '',
           '_disable_config': True,
           }

    def test_definition_list_item_classes(self):
        # Do not drop class arguments for the definition list item.
        # Pass them to the term node instead.
        data = """\
first term:
  fist def

  .. class:: for the second item

second term:
  second def
"""
        result = core.publish_string(data, writer=html5_polyglot.Writer(),
                                     settings_overrides=self.mys).decode()
        self.assertIn('<dt class="for the second item">second term:</dt>',
                      result)

    def test_definition_list_item_name(self):
        # Do not drop the "name" of the definition list item.
        # Pass it to to the term node instead.
        data = """\
first term:
  first def

  .. _second item:

second term:
  second def
"""
        result = core.publish_string(data, writer=html5_polyglot.Writer(),
                                     settings_overrides=self.mys).decode()
        self.assertIn('<dt id="second-item">second term:</dt>',
                      result)


ham_css = relpath(DATA_ROOT / 'ham.css')


class SettingsTestCase(unittest.TestCase):
    data = 'test'

    def test_default_stylesheet(self):
        # default style sheet, embedded
        mys = {'_disable_config': True}
        styles = core.publish_parts(self.data, writer=html5_polyglot.Writer(),
                                    settings_overrides=mys)['stylesheet']
        self.assertIn('Minimal style sheet for the HTML output of Docutils.',
                      styles)

    def test_default_stylesheet_linked(self):
        # default style sheet, linked
        mys = {'_disable_config': True,
               'embed_stylesheet': False}
        styles = core.publish_parts(self.data, writer=html5_polyglot.Writer(),
                                    settings_overrides=mys)['stylesheet']
        self.assertIn('docutils/writers/html5_polyglot/minimal.css', styles)

    def test_math_stylesheet_linked(self):
        # default + math style sheet, linked
        mys = {'_disable_config': True,
               'embed_stylesheet': False,
               'stylesheet_path': 'minimal.css, math.css'}
        styles = core.publish_parts(self.data, writer=html5_polyglot.Writer(),
                                    settings_overrides=mys)['stylesheet']
        self.assertIn('docutils/writers/html5_polyglot/minimal.css', styles)
        self.assertIn('docutils/writers/html5_polyglot/math.css', styles)

    def test_custom_stylesheet_linked(self):
        # default + custom style sheet, linked
        mys = {'_disable_config': True,
               'embed_stylesheet': False,
               'stylesheet_path': 'minimal.css, '
               'data/ham.css'}
        styles = core.publish_parts(self.data, writer=html5_polyglot.Writer(),
                                    settings_overrides=mys)['stylesheet']
        self.assertIn('docutils/writers/html5_polyglot/minimal.css', styles)
        self.assertIn('href="data/ham.css"', styles)

    def test_custom_stylesheet_dir(self):
        mys = {'_disable_config': True,
               'embed_stylesheet': False,
               'stylesheet_dirs': (
                   TEST_ROOT / '../docutils/writers/html5_polyglot/',
                   DATA_ROOT),
               'stylesheet_path': 'minimal.css, ham.css'}
        styles = core.publish_parts(self.data, writer=html5_polyglot.Writer(),
                                    settings_overrides=mys)['stylesheet']
        if (TEST_ROOT / '../docutils/writers/html5_polyglot/').is_dir():
            self.assertIn('docutils/writers/html5_polyglot/minimal.css',
                          styles)
        self.assertIn(f'href="{ham_css}"', styles)

    def test_custom_stylesheet_dir_embedded(self):
        mys = {'_disable_config': True,
               'embed_stylesheet': True,
               'stylesheet_dirs': (
                   TEST_ROOT / '../docutils/writers/html5_polyglot/',
                   DATA_ROOT),
               'stylesheet_path': 'ham.css'}
        styles = core.publish_parts(self.data, writer=html5_polyglot.Writer(),
                                    settings_overrides=mys)['stylesheet']
        self.assertIn('dl.docutils dd {\n  margin-bottom: 0.5em }', styles)

    def test_future_warnings(self):
        """Warn about deprecated setting name."""
        my_settings = {'_disable_config': True,
                       'embed_images': False,
                       'output_encoding': 'unicode',
                       }
        with self.assertWarnsRegex(FutureWarning,
                                   '"embed_images"\n  will be removed'):
            core.publish_string('warnings test',
                                writer=html5_polyglot.Writer(),
                                settings_overrides=my_settings)


minimal_css = relpath(TEST_ROOT / 'functional/input/data/minimal.css')
plain_css = relpath(TEST_ROOT / 'functional/input/data/plain.css')
math_css = relpath(TEST_ROOT / 'functional/input/data/math.css')


class MathTestCase(unittest.TestCase):

    """Provisional: This class tests the current implementation of maths
    support which is open to change in future Docutils releases. """

    mathjax_script = '<script type="text/javascript" src="%s">'
    default_mathjax_url = ('file:/usr/share/javascript/mathjax/MathJax.js'
                           '?config=TeX-AMS_CHTML')
    custom_mathjax_url = '/mathjax/MathJax.js?config=TeX-AMS-MML_HTMLorMML'
    data = ':math:`42`'

    def test_math_output_default(self):
        # default math output is MathML (since 0.22)
        mys = {'_disable_config': True}
        fragment = core.publish_parts(self.data, writer=html5_polyglot.Writer(),
                                      settings_overrides=mys)['fragment']
        self.assertIn('<mn>42</mn>', fragment)

    def test_math_output_mathjax(self):
        # Explicitly specifying math_output=MathJax, case insensitively
        # use default MathJax URL
        mys = {'_disable_config': True,
               'report_level': 3,
               'math_output': 'MathJax'}
        head = core.publish_parts(self.data, writer=html5_polyglot.Writer(),
                                  settings_overrides=mys)['head']
        self.assertIn(self.mathjax_script % self.default_mathjax_url, head)

    def test_math_output_mathjax_custom(self):
        # Customizing MathJax URL
        mys = {'_disable_config': True,
               'math_output':
               'mathjax %s' % self.custom_mathjax_url}
        head = core.publish_parts(self.data, writer=html5_polyglot.Writer(),
                                  settings_overrides=mys)['head']
        self.assertIn(self.mathjax_script % self.custom_mathjax_url, head)

    def test_math_output_html(self):
        mys = {'_disable_config': True,
               'math_output': 'HTML'}
        head = core.publish_parts(self.data, writer=html5_polyglot.Writer(),
                                  settings_overrides=mys)['head']
        # There should be no MathJax script when math_output is not MathJax
        self.assertNotIn('MathJax.js', head)

    def test_math_output_html_stylesheet(self):
        mys = {'_disable_config': True,
               'math_output': 'HTML math.css,custom/style.css',
               'stylesheet_dirs': (
                   TEST_ROOT,
                   TEST_ROOT / 'functional/input/data'),
               'embed_stylesheet': False}
        styles = core.publish_parts(self.data, writer=html5_polyglot.Writer(),
                                    settings_overrides=mys)['stylesheet']
        self.assertEqual(f"""\
<link rel="stylesheet" href="{minimal_css}" type="text/css" />
<link rel="stylesheet" href="{plain_css}" type="text/css" />
<link rel="stylesheet" href="{math_css}" type="text/css" />
<link rel="stylesheet" href="custom/style.css" type="text/css" />
""", styles)

    def test_math_output_mathjax_no_math(self):
        # There should be no math script when text does not contain math
        head = core.publish_parts('No math.', writer=html5_polyglot.Writer())['head']
        self.assertNotIn('MathJax', head)


if __name__ == '__main__':
    unittest.main()
