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
except:
    pass

import argparse
import sys

import docutils
from docutils.core import Publisher, publish_cmdline

usage = '%(prog)s [options] [<source> [<destination>]]'

description = ('Convert plaintext documentation into useful formats.\n'
               'Reads from <source> (default is stdin) '
               'and writes to <destination> (default is stdout).')

epilog = ('Further optional arguments depend on the selected components. '
          'See <https://docutils.sourceforge.io/docs/user/config.html> '
          'for the full reference. The list below adapts to your selection.')


class CliSettingsSpec(docutils.SettingsSpec):
    """Additional runtime settings & command-line options for the CLI."""

    settings_spec = (
        'Docutils CLI Options',
        None,
        (# 'help text', [<option strings>], {<keyword arguments>}
         ('', ['--reader'], {'default': 'standalone'}),
         ('', ['--parser'], {'default': 'restructuredtext'}),
         ('', ['--writer'], {'default': 'html'}),
        )
    )
    config_section = 'docutils-cli application'
    config_section_dependencies = ('applications',)

# Get default components from configuration files 
# default to "html5" writer for backwards compatibility
default_settings = Publisher().get_settings(settings_spec=CliSettingsSpec,
                        writer='html5')


argparser = argparse.ArgumentParser(
                usage=usage, description=description, epilog=epilog,
                formatter_class=argparse.ArgumentDefaultsHelpFormatter,
                add_help=False,)

argparser.add_argument('--reader', help='reader name',
                       default=default_settings.reader)
argparser.add_argument('--parser', help='parser name',
                       default=default_settings.parser)
argparser.add_argument('--writer', help='writer name',
                       default=default_settings.writer)

(args, remainder) = argparser.parse_known_args()

if '-h' in sys.argv or '--help' in sys.argv:
    print(argparser.format_help())

try:
    publish_cmdline(reader_name=args.reader,
                    parser_name=args.parser,
                    writer_name=args.writer,
                    config_section=CliSettingsSpec.config_section,
                    argv=remainder,
                    usage='',
                    description='')
except ImportError as error:
    print('%s.' % error)
    if '--traceback' in remainder:
        raise
    else:
        print('Use "--traceback" to show details.')
