#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

PEP HTML Writer.
"""

__docformat__ = 'reStructuredText'


from docutils import nodes
from docutils.writers import html4css1


class Writer(html4css1.Writer):

    cmdline_options = (
        'PEP/HTML-Specific Options',
        None,
        (('Specify a stylesheet file.  Default is "rststyle.css".',
          ['--stylesheet'],
          {'default': 'rststyle.css', 'metavar': '<file>'}),
         ('Specify a template file.  Default is "peptemplate.html".',
          ['--template'],
          {'default': 'peptemplate.html', 'metavar': '<file>'}),
         ('Python\'s home URL.  Default is "http://www.python.org".',
          ['--python-home'],
          {'default': 'http://www.python.org', 'metavar': '<URL>'}),
         ('Home URL for this PEP.  Default is "." (current directory).',
          ['--pep-home'],
          {'default': '.', 'metavar': '<URL>'}),))

    def __init__(self):
        html4css1.Writer.__init__(self)
        self.translator_class = HTMLTranslator

    def translate(self):
        html4css1.Writer.translate(self)
        options = self.document.options
        template = open(options.template).read()
        pyhome = options.python_home
        pephome = options.pep_home
        index = self.document.first_child_matching_class(nodes.field_list)
        header = self.document[index]
        pep = header[0][1].astext()
        try:
            pepnum = '%04i' % int(pep)
        except:
            pepnum = pep
        title = self.document[1][1].astext()
        body = ''.join(self.body)
        body_suffix = ''.join(self.body_suffix)
        self.output = template % locals()


class HTMLTranslator(html4css1.HTMLTranslator):

    def depart_field_list(self, node):
        html4css1.HTMLTranslator.depart_field_list(self, node)
        if node.hasattr('class') and node['class'] == 'rfc2822':
             self.body.append('<hr />\n')
