#!/usr/bin/env python3
# -*- coding: utf8 -*-
# :Copyright: © 2020 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause
#
# Revision: $Revision$
# Date: $Date$

"""
A generic front end to the Docutils Publisher.
"""

try:
    import locale # module missing in Jython
    locale.setlocale(locale.LC_ALL, '')
except locale.Error:
    pass

import argparse
import sys

from docutils.core import publish_cmdline, default_description

description = (u'Generate documents from reStructuredText sources.'
              )
epilog = (u'Currently, the component selection cannot be specified in the '
          u'configuration file. '
          u'The availability of some options depends on the selected '
          u'components, the list below adapts to your selection.'
         )

parser = argparse.ArgumentParser(add_help=False, description=description,
                                 epilog=epilog)

parser.add_argument('--reader', choices=('standalone', 'pep'),
                    help=u'Reader name (default "standalone").',
                    default='standalone')
parser.add_argument('--parser', choices=('markdown', 'rst'),
                    help=u'Parser name (default "rst").',
                    default='rst')
parser.add_argument('--writer', 
                    # choices=('html', 'html4', 'html5', 'latex', 'xelatex',
                    #          'odt', 'xml', 'pseudoxml', 'manpage',
                    #          'pep_html', 's5_html'),
                    help=u'Writer name (default "html5").',
                    default='html5')

(args, remainder) = parser.parse_known_args()


if '-h' in sys.argv or '--help' in sys.argv:
    print(parser.format_help())
    print('')


publish_cmdline(reader_name=args.reader,
                parser_name=args.parser,
                writer_name=args.writer, 
                description=default_description,
                argv=remainder)
