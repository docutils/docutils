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

import argparse
import locale
import sys

import docutils
from docutils.core import Publisher, publish_cmdline


class CliSettingsSpec(docutils.SettingsSpec):
    """Runtime settings & command-line options for the generic CLI."""

    settings_spec = (
        'Docutils CLI Options',
        None,
        (# 'help text', [<option strings>], {<keyword arguments>}
         ('Reader name (default/current: "%default").',
          ['--reader'], {'metavar': '<reader>'}),
         ('Parser name (default/current: "%default").',
          ['--parser'], {'metavar': '<parser>'}),
         ('Writer name (default/current: "%default").',
          ['--writer'], {'metavar': '<writer>'}),
        )
    )
    config_section = 'docutils-cli application'
    config_section_dependencies = ('applications',)


def main(settings_spec=CliSettingsSpec,
         reader='standalone', parser='rst', writer='html'):
    """Generic command line interface for the Docutils Publisher.
    """
    locale.setlocale(locale.LC_ALL, '')

    description = ('Convert plaintext documentation into useful formats.  '
                   'Available options depend on the selected reader, '
                   'writer, and parser.  '
                   + docutils.core.default_description)

    default_settings = Publisher().get_settings(settings_spec=settings_spec,
                                                reader=reader,
                                                parser=parser,
                                                writer=writer)

    # Pre-parse the command-line with "argparse" for component-setting options
    argparser = argparse.ArgumentParser(add_help=False)
    argparser.add_argument('--reader', default=default_settings.reader)
    argparser.add_argument('--parser', default=default_settings.parser)
    argparser.add_argument('--writer', default=default_settings.writer)
    (args, remainder) = argparser.parse_known_args()

    # TODO: require source ('-' for stdin)
    # argparser.add_argument('source')
    # argparser.add_argument('destination', nargs='?')
    #
    # print usage if there is no arg
    # (always or if sys.stdout.isatty() ?)
    # Alternatively, print a log message to stderr.
    #
    # Display output on success, but keep it brief.
    # provide a -q option to suppress all non-essential output.
    #
    # Consider chaining several args as input
    # and use --output (or redirection) for output
    # argparser.add_argument('source', nargs='+')
    #
    # -- https://clig.dev/#help

    # Update defaults (shown in help):
    CliSettingsSpec.settings_default_overrides = args.__dict__

    try:
        publish_cmdline(reader_name=args.reader,
                        parser_name=args.parser,
                        writer_name=args.writer,
                        settings_spec=settings_spec,
                        description=description,
                        argv=remainder)
    except ImportError as error:
        print('%s.' % error, file=sys.stderr)
        if '--traceback' in remainder:
            raise
        else:
            print('Use "--traceback" to show details.')


if __name__ == '__main__':
    # backwards compatibility: docutils-cli.py defaults to "html5" writer:
    main(writer='html5')
