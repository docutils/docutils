#!/usr/bin/env python3
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
from docutils.frontend import ConfigParser, OptionParser

config_section = 'docutils-cli application'

def config_settings_from_files():
    """Return dict with default settings for the docutils-cli front end.

    Read default values for the three docutils-cli options from the
    [docutils-cli application] section in standard configuration files.

    Command-line options are defined separately, using the "argparse" module
    to allow two-stage parsing of the command line.
    """
    settings = {'reader': 'standalone',
                'parser': 'rst',
                'writer': 'html5'
               }
    option_parser = OptionParser()
    config_parser = ConfigParser()
    config_files = option_parser.get_standard_config_files()
    config_parser.read(config_files, option_parser)
    settings.update(config_parser.get_section(config_section))
    return settings

default_settings = config_settings_from_files()

description = u'Generate documents from reStructuredText or Markdown sources.'

epilog = (u'Further optional arguments are added by the selected '
          u'components, the list below adapts to your selection.'
         )

parser = argparse.ArgumentParser(add_help=False,
                description=description, epilog=epilog,
                formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument('source', nargs='?')
parser.add_argument('destination', nargs='?')
parser.add_argument('--reader', help=u'reader name',
                    default=default_settings['reader'])
parser.add_argument('--parser', help=u'parser name',
                    default=default_settings['parser'])
parser.add_argument('--writer', help=u'writer name',
                    default=default_settings['writer'])

(args, remainder) = parser.parse_known_args()

# push back positional arguments
if args.destination:
    remainder.insert(0, args.destination)
if args.source:
    remainder.insert(0, args.source)

if '-h' in sys.argv or '--help' in sys.argv:
    print(parser.format_help())
    print('')

try:
    publish_cmdline(reader_name=args.reader,
                    parser_name=args.parser,
                    writer_name=args.writer,
                    config_section=config_section,
                    description=default_description,
                    argv=remainder)
except ImportError as error:
    print('%s.' % error)
    if '--traceback' in remainder:
        raise
    else:
        print('Use "--traceback" to show details.')
