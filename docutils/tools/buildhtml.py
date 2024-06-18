#!/usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Generate .html from all reStructuredText files in a directory.

Source files are understood to be standalone reStructuredText documents.
Files with names starting ``pep-`` are interpreted as reStructuredText PEPs.
"""

__docformat__ = 'reStructuredText'


try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except Exception:
    pass

from fnmatch import fnmatch
import os
import os.path
import sys
import warnings

import docutils
import docutils.io
from docutils import core, frontend, ApplicationError
from docutils.parsers import rst
from docutils.readers import standalone, pep
from docutils.writers import html4css1, html5_polyglot, pep_html


usage = '%prog [options] [<directory> ...]'
description = ('Generate .html from all reStructuredText files '
               'in each <directory> (default is the current directory).')


class SettingsSpec(docutils.SettingsSpec):

    """
    Runtime settings & command-line options for the "buildhtml" front end.
    """

    prune_default = ['/*/.hg', '/*/.bzr', '/*/.git', '/*/.svn',
                     '/*/.venv', '/*/__pycache__']
    sources_default = ['*.rst', '*.txt']

    # Can't be included in OptionParser below because we don't want to
    # override the base class.
    settings_spec = (
        'Build-HTML Options',
        None,
        (('Process all files matching any of the given '
          'glob-style patterns (separated by colons). '
          'This option overwrites the default or config-file values. '
          f'Default: "{":".join(sources_default)}".',
          ['--sources'],
          {'metavar': '<patterns>',
           'default': sources_default,
           'validator': frontend.validate_colon_separated_string_list}),
         ('Recursively ignore files matching any of the given '
          'glob-style patterns (separated by colons). '
          'This option may be used more than once to add more patterns.',
          ['--ignore'],
          {'metavar': '<patterns>', 'action': 'append',
           'default': [],
           'validator': frontend.validate_colon_separated_string_list}),
         ('Do not scan subdirectories for files to process.',
          ['--local'], {'dest': 'recurse', 'action': 'store_false'}),
         ('Recursively scan subdirectories for files to process.  This is '
          'the default.',
          ['--recurse'],
          {'action': 'store_true', 'default': 1,
           'validator': frontend.validate_boolean}),
         ('Do not process files in <directory> (glob-style patterns, '
          'separated by colons).  This option may be used '
          'more than once to add more patterns.  Default: "%s".'
          % ':'.join(prune_default),
          ['--prune'],
          {'metavar': '<directory>', 'action': 'append',
           'validator': frontend.validate_colon_separated_string_list,
           'default': prune_default}),
         ('Docutils writer, one of "html", "html4", "html5". '
          'Default: "html" (use Docutils\' default HTML writer).',
          ['--writer'],
          {'metavar': '<writer>',
           'choices': ['html', 'html4', 'html5'],
           # 'default': 'html' (set below)
           }),
         (frontend.SUPPRESS_HELP,  # Obsoleted by "--writer"
          ['--html-writer'],
          {'metavar': '<writer>',
           'choices': ['html', 'html4', 'html5']}),
         ('Work silently (no progress messages).  Independent of "--quiet".',
          ['--silent'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         ('Do not process files, show files that would be processed.',
          ['--dry-run'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),))

    relative_path_settings = ('prune',)
    config_section = 'buildhtml application'
    config_section_dependencies = ('applications',)


class OptionParser(frontend.OptionParser):

    """
    Command-line option processing for the ``buildhtml.py`` front end.
    """

    def check_values(self, values, args):
        super().check_values(values, args)
        values._source = None
        return values

    def check_args(self, args):
        self.values._directories = args or [os.getcwd()]
        # backwards compatibility:
        return None, None


class Struct:

    """Stores data attributes for dotted-attribute access."""

    def __init__(self, **keywordargs):
        self.__dict__.update(keywordargs)


class Builder:

    def __init__(self):
        self.publishers = {
            '': Struct(components=(pep.Reader, rst.Parser, pep_html.Writer,
                                   SettingsSpec)),
            'html4': Struct(components=(rst.Parser, standalone.Reader,
                                        html4css1.Writer, SettingsSpec),
                            reader='standalone',
                            writer='html4'),
            'html5': Struct(components=(rst.Parser, standalone.Reader,
                                        html5_polyglot.Writer, SettingsSpec),
                            reader='standalone',
                            writer='html5'),
            'PEPs': Struct(components=(rst.Parser, pep.Reader,
                                       pep_html.Writer, SettingsSpec),
                           reader='pep',
                           writer='pep_html')}
        """Publisher-specific settings.  Key '' is for the front-end script
        itself.  ``self.publishers[''].components`` must contain a superset of
        all components used by individual publishers."""

        self.setup_publishers()
        # default html writer (may change to html5 some time):
        self.publishers['html'] = self.publishers['html4']

    def setup_publishers(self):
        """
        Manage configurations for individual publishers.

        Each publisher (combination of parser, reader, and writer) may have
        its own configuration defaults, which must be kept separate from those
        of the other publishers.  Setting defaults are combined with the
        config file settings and command-line options by
        `self.get_settings()`.
        """
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            for name, publisher in self.publishers.items():
                option_parser = OptionParser(
                    components=publisher.components, read_config_files=True,
                    usage=usage, description=description)
                publisher.option_parser = option_parser
                publisher.setting_defaults = option_parser.get_default_values()
                frontend.make_paths_absolute(
                    publisher.setting_defaults.__dict__,
                    option_parser.relative_path_settings)
                publisher.config_settings = (
                    option_parser.get_standard_config_settings())
            self.settings_spec = self.publishers[''].option_parser.parse_args(
                values=frontend.Values())  # no defaults; just the cmdline opts
            self.initial_settings = self.get_settings('')

        if self.initial_settings.html_writer is not None:
            warnings.warn('The configuration setting "html_writer" '
                          'will be removed in Docutils 2.0. '
                          'Use setting "writer" instead.',
                          FutureWarning, stacklevel=5)
        if self.initial_settings.writer is None:
            self.initial_settings.writer = (self.initial_settings.html_writer
                                            or 'html')

    def get_settings(self, publisher_name, directory=None):
        """
        Return a settings object, from multiple sources.

        Copy the setting defaults, overlay the startup config file settings,
        then the local config file settings, then the command-line options.

        If `directory` is not None, it is searched for a file "docutils.conf"
        which is parsed after standard configuration files.
        Path settings in this configuration file are resolved relative
        to `directory`, not the current working directory.
        """
        publisher = self.publishers[publisher_name]
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            settings = frontend.Values(publisher.setting_defaults.__dict__)
        settings.update(publisher.config_settings, publisher.option_parser)
        if directory:
            local_config = publisher.option_parser.get_config_file_settings(
                os.path.join(directory, 'docutils.conf'))
            frontend.make_paths_absolute(
                local_config, publisher.option_parser.relative_path_settings,
                directory)
            settings.update(local_config, publisher.option_parser)
        settings.update(self.settings_spec.__dict__, publisher.option_parser)
        # remove duplicate entries from "appending" settings:
        settings.ignore = list(set(settings.ignore))
        settings.prune = list(set(settings.prune))
        return settings

    def run(self, directory=None, recurse=True):
        recurse = recurse and self.initial_settings.recurse
        if directory:
            self.directories = [directory]
        elif self.settings_spec._directories:
            self.directories = self.settings_spec._directories
        else:
            self.directories = [os.getcwd()]
        for directory in self.directories:
            directory = os.path.abspath(directory)
            for dirpath, dirnames, filenames in os.walk(directory):
                # `os.walk()` by default recurses down the tree,
                # we modify `dirnames` in-place to control the behaviour.
                if recurse:
                    dirnames.sort()
                else:
                    del dirnames[:]
                self.visit(dirpath, dirnames, filenames)

    def visit(self, dirpath, dirnames, filenames):
        settings = self.get_settings('', dirpath)
        errout = docutils.io.ErrorOutput(encoding=settings.error_encoding)
        if match_patterns(dirpath, settings.prune):
            errout.write('/// ...Skipping directory (pruned): %s\n'
                         % os.path.relpath(dirpath))
            sys.stderr.flush()
            del dirnames[:]  # modify in-place to control `os.walk()` run
            return
        if not self.initial_settings.silent:
            errout.write('/// Processing directory: %s\n'
                         % os.path.relpath(dirpath))
            sys.stderr.flush()
        for name in sorted(filenames):
            if match_patterns(name, settings.ignore):
                continue
            if match_patterns(name, settings.sources):
                self.process_txt(dirpath, name)

    def process_txt(self, directory, name):
        # TODO change name to `process_rst_source_file()`?
        if name.startswith('pep-'):
            publisher = 'PEPs'
        else:
            publisher = self.initial_settings.writer
        settings = self.get_settings(publisher, directory)
        errout = docutils.io.ErrorOutput(encoding=settings.error_encoding)
        pub_struct = self.publishers[publisher]
        settings._source = os.path.normpath(os.path.join(directory, name))
        settings._destination = os.path.splitext(settings._source)[0] + '.html'
        if not self.initial_settings.silent:
            errout.write('    ::: Processing: %s\n' % name)
            sys.stderr.flush()
        if not settings.dry_run:
            try:
                core.publish_file(source_path=settings._source,
                                  destination_path=settings._destination,
                                  reader=pub_struct.reader,
                                  parser='restructuredtext',
                                  writer=pub_struct.writer,
                                  settings=settings)
            except ApplicationError as err:
                errout.write(f'        {type(err).__name__}: {err}\n')


def match_patterns(name, patterns):
    """Return True, if `name` matches any item of the sequence `patterns`.

    Matching is done with `fnmatch.fnmatch`. It resembles shell-style
    globbing, but without special treatment of path separators and '.'
    (in contrast to the `glob module` and `pathlib.PurePath.match()`).
    For example, "``/*.py``" matches "/a/b/c.py".

    PROVISIONAL.
    TODO: use `pathlib.PurePath.match()` once this supports "**".
    """
    for pattern in patterns:
        if fnmatch(name, pattern):
            return True
    return False


if __name__ == "__main__":
    Builder().run()
