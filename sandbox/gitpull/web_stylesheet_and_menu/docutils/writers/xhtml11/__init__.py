# .. coding: utf8
# :Author: Günter Milde <milde@users.berlios.de>
# :Revision: $Revision$
# :Date: $Date: 2005-06-28$
# :Copyright: © 2005, 2009 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: http://www.spdx.org/licenses/BSD-2-Clause

"""
Strict HyperText Markup Language document tree Writer.

This is a variant of Docutils' standard 'html4css1' writer.

GOAL:
 * The output conforms to the XHTML version 1.1 DTD.
 * It contains no hard-coded formatting information that would prevent
   layout design by cascading style sheets.
"""

__docformat__ = 'reStructuredText'

import os
import os.path
import re

import docutils
from docutils import frontend, nodes, utils, writers, languages
from docutils.writers import html_base

class Writer(html_base.Writer):

    supported = ('html', 'xhtml', 'xhtml1',
                 'html4strict', 'xhtml1strict',
                 'xhtml11', 'xhtml1css2')
    """Formats this writer supports."""

    default_stylesheets = ['html-base.css', 'xhtml11.css']
    default_stylesheet_dirs = ['.',
        os.path.abspath(os.path.dirname(__file__)),
        os.path.abspath(os.path.join(
            os.path.dirname(os.path.dirname(__file__)), 'html_base'))
       ]

    config_section = 'xhtml11 writer'
    config_section_dependencies = ('writers', 'html writer')

    settings_spec = frontend.filter_settings_spec(
        html_base.Writer.settings_spec,
        stylesheet_path = (
          'Comma separated list of stylesheet paths. '
          'Relative paths are expanded if a matching file is found in '
          'the --stylesheet-dirs. With --link-stylesheet, '
          'the path is rewritten relative to the output HTML file. '
          'Default: "%s"' % ','.join(default_stylesheets),
          ['--stylesheet-path'],
          {'metavar': '<file[,file,...]>', 'overrides': 'stylesheet',
           'validator': frontend.validate_comma_separated_list,
           'default': default_stylesheets}),
        stylesheet_dirs = (
          'Comma-separated list of directories where stylesheets are found. '
          'Used by --stylesheet-path when expanding relative path arguments. '
          'Default: "%s"' % default_stylesheet_dirs,
          ['--stylesheet-dirs'],
          {'metavar': '<dir[,dir,...]>',
           'validator': frontend.validate_comma_separated_list,
           'default': default_stylesheet_dirs}),
        math_output = ('Math output format, one of "MathML", "HTML", '
            '"MathJax" or "LaTeX". Default: "MathML"',
            ['--math-output'],
            {'default': 'MathML'}),
        xml_declaration = ('Prepend an XML declaration. '
          'Default: True',
          ['--xml-declaration'],
          {'default': True, 'action': 'store_true',
           'validator': frontend.validate_boolean}))

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = HTMLTranslator


class HTMLTranslator(html_base.HTMLTranslator):
    """
    This writer generates XHTML 1.1
    without formatting that interferes with a CSS stylesheet.
    """
    doctype = ('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" '
               '"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">\n')
    doctype_mathml = (
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN" '
        '"http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd">\n')

    # there is no attribute "lang" in XHTML 1.1
    lang_attribute = 'xml:lang' # changed from 'lang' in XHTML 1.0
    head_prefix_template = ('<html xmlns="http://www.w3.org/1999/xhtml"'
                            ' xml:lang="%(lang)s">\n<head>\n')


    # enumerated lists
    # ----------------
    # The 'start' attribute does not conform to HTML4/XHTML1 Strict
    # (resurfaced in HTML5)

    def visit_enumerated_list(self, node):
        atts = {}
        if 'start' in node:
            atts['style'] = 'counter-reset: item %d;' % (node['start'] - 1)
        classes = node.setdefault('classes', [])
        if 'enumtype' in node:
            classes.append(node['enumtype'])
        if self.is_compactable(node):
            classes.append('simple')
        self.body.append(self.starttag(node, 'ol', **atts))


    # Meta tags: 'lang' attribute replaced by 'xml:lang' in XHTML 1.1
    # HTML5/polyglott recommends using both

    def visit_meta(self, node):
        if node.hasattr('lang'):
            node['xml:lang'] = node['lang']
            del(node['lang'])
        meta = self.emptytag(node, 'meta', **node.non_default_attributes())
        self.add_meta(meta)
