#! /usr/bin/env python
# coding: utf-8

# $Id$
# Authors: Lea Wiemann, Dmitry Shachnev, Günter Milde
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""
Miscellaneous HTML writer tests.
"""

from __init__ import DocutilsTestSupport
from docutils import core
from docutils._compat import b

class EncodingTestCase(DocutilsTestSupport.StandardTestCase):

    def test_xmlcharrefreplace(self):
        # Test that xmlcharrefreplace is the default output encoding
        # error handler.
        settings_overrides={
            'output_encoding': 'latin1',
            'stylesheet': '',
            '_disable_config': True,}
        result = core.publish_string(
            u'EUR = \u20ac', writer_name='html4css1',
            settings_overrides=settings_overrides)
        # Encoding a euro sign with latin1 doesn't work, so the
        # xmlcharrefreplace handler is used.
        self.assertIn(b('EUR = &#8364;'), result)

class MathTestCase(DocutilsTestSupport.StandardTestCase):
    
    """Attention: This class tests the current implementation of maths support
    which is open to change in future Docutils releases. """

    settings_overrides={'_disable_config': True,}
    mathjax_script = '<script type="text/javascript" src="%s">'
    default_mathjax_url = ('http://cdn.mathjax.org/mathjax/latest/MathJax.js'
                           '?config=TeX-AMS-MML_HTMLorMML')
    custom_mathjax_url = ('file:///usr/share/javascript/mathjax/MathJax.js'
                          '?config=TeX-AMS-MML_HTMLorMML')
    data = ':math:`42`'

    def test_math_output_default(self):
        # Currently MathJax with default URL. Likely to change to HTML!
        mysettings = self.settings_overrides
        head = core.publish_parts(self.data, writer_name='html4css1',
                                  settings_overrides=mysettings)['head']
        self.assertIn(self.mathjax_script % self.default_mathjax_url, head)
        
    def test_math_output_mathjax(self):
        # Explicitly specifying math_output=MathJax, case insensitively
        # use default MathJax URL
        mysettings = self.settings_overrides.copy()
        mysettings.update({'math_output': 'MathJax'})
        head = core.publish_parts(self.data, writer_name='html4css1',
            settings_overrides=mysettings)['head']
        self.assertIn(self.mathjax_script % self.default_mathjax_url, head)

    def test_math_output_mathjax_custom(self):
        # Customizing MathJax URL
        mysettings = self.settings_overrides.copy()
        mysettings.update({'math_output': 
                           'mathjax %s' % self.custom_mathjax_url})
        head = core.publish_parts(self.data, writer_name='html4css1',
            settings_overrides=mysettings)['head']
        self.assertIn(self.mathjax_script % self.custom_mathjax_url, head)
        
    def test_math_output_html(self):
        # There should be no MathJax script when math_output is not MathJax
        mysettings = self.settings_overrides.copy()
        mysettings.update({'math_output': 'HTML'})
        head = core.publish_parts(self.data, writer_name='html4css1',
            settings_overrides=mysettings)['head']
        self.assertNotIn('MathJax.js', head)
        
    def test_math_output_mathjax_no_math(self):
        mysettings = self.settings_overrides.copy()
        mysettings.update({'math_output': 'MathJax'})
        # There should be no math script when text does not contain math
        head = core.publish_parts('No math.', writer_name='html4css1')['head']
        self.assertNotIn('MathJax', head)


if __name__ == '__main__':
    import unittest
    unittest.main()
