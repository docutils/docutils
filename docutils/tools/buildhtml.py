#!/usr/bin/env python

"""
Generates .html from all the .txt files in a directory.

Ordinary .txt files are understood to be standalone reStructuredText.
Files named ``pep-*.txt`` are interpreted as PEPs (either old-style or
new reStructuredText PEPs).
"""
# Once PySource is here, build .html from .py as well.

__docformat__ = 'reStructuredText'


import locale
try:
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

import sys
import os
import os.path
import copy
import docutils
from docutils import ApplicationError
from docutils import core, frontend
from docutils.parsers import rst
from docutils.readers import standalone, pep
from docutils.writers import html4css1, pep_html
import pep2html


usage = '%prog [options] [<directory> ...]'
description = ('Generates .html from all the .txt files (including PEPs) '
               'in each <directory> (default is the current directory).')


class SettingsSpec(docutils.SettingsSpec):

    """
    Runtime settings & command-line options for the front end.
    """

    # Can't be included in OptionParser below because we don't want to
    # override the base class.
    settings_spec = (
        'Build-HTML Options',
        None,
        (('Recursively scan subdirectories for files to process.  This is '
          'the default.',
          ['--recurse'], {'action': 'store_true', 'default': 1}),
         ('Do not scan subdirectories for files to process.',
          ['--local'], {'dest': 'recurse', 'action': 'store_false'}),
         ('Work silently (no progress messages).  Independent of "--quiet".',
          ['--silent'], {'action': 'store_true'}),))

    
class OptionParser(frontend.OptionParser):

    """
    Command-line option processing for the ``buildhtml.py`` front end.
    """

    def check_values(self, values, args):
        frontend.OptionParser.check_values(self, values, args)
        values._source = None
        return values

    def check_args(self, args):
        source = destination = None
        if args:
            self.values._directories = args
        else:
            self.values._directories = [os.getcwd()]
        return source, destination


class Struct:

    """Stores data attributes for dotted-attribute access."""

    def __init__(self, **keywordargs):
        self.__dict__.update(keywordargs)


class Builder:

    def __init__(self):
        self.publishers = {
            '': Struct(components=(SettingsSpec, pep.Reader, rst.Parser,
                                   pep_html.Writer)),
            '.txt': Struct(components=(rst.Parser, standalone.Reader,
                                       html4css1.Writer)),
            'PEPs': Struct(components=(rst.Parser, pep.Reader,
                                       pep_html.Writer))}
        """Publisher-specific settings.  Key '' is for the front-end script
        itself.  ``self.publishers[''].components`` must contain a superset of
        all components used by individual publishers."""

        self.setup_publishers()

    def setup_publishers(self):
        """
        Manage configurations for individual publishers.

        Each publisher (combination of parser, reader, and writer) may have
        its own configuration defaults, which must be kept separate from those
        of the other publishers.  Setting defaults are combined with the
        config file settings and command-line options by
        `self.get_settings()`.
        """
        for name, publisher in self.publishers.items():
            option_parser = OptionParser(
                components=publisher.components,
                usage=usage, description=description)
            publisher.option_parser = option_parser
            publisher.setting_defaults = option_parser.get_default_values()
            frontend.make_paths_absolute(publisher.setting_defaults.__dict__,
                                         option_parser.relative_path_settings)
        config_parser = frontend.ConfigParser()
        config_parser.read_standard_files()
        self.config_settings = config_parser.get_section('options')
        frontend.make_paths_absolute(
            self.config_settings,
            self.publishers[''].option_parser.relative_path_settings)
        self.settings_spec = self.publishers[''].option_parser.parse_args(
            values=frontend.Values())   # no defaults; just the cmdline opts
        self.initial_settings = self.get_settings('')

    def get_settings(self, publisher_name, directory=None):
        """
        Return a settings object, from multiple sources.

        Copy the setting defaults, overlay the startup config file settings,
        then the local config file settings, then the command-line options.
        Assumes the current directory has been set.
        """
        publisher = self.publishers[publisher_name]
        settings = copy.deepcopy(publisher.setting_defaults)
        settings.__dict__.update(self.config_settings)
        if directory:
            config_parser = frontend.ConfigParser()
            config_parser.read(os.path.join(directory, 'docutils.conf'))
            local_config = config_parser.get_section('options')
            frontend.make_paths_absolute(
                local_config, publisher.option_parser.relative_path_settings,
                directory)
            settings.__dict__.update(local_config)
        settings.__dict__.update(self.settings_spec.__dict__)
        return settings

    def run(self, directory=None, recurse=1):
        recurse = recurse and self.initial_settings.recurse
        if directory:
            self.directories = [directory]
        elif self.settings_spec._directories:
            self.directories = self.settings_spec._directories
        else:
            self.directories = [os.getcwd()]
        for directory in self.directories:
            os.path.walk(directory, self.visit, recurse)

    def visit(self, recurse, directory, names):
        if not self.initial_settings.silent:
            print >>sys.stderr, '/// Processing directory:', directory
            sys.stderr.flush()
        peps_found = 0
        for name in names:
            if name.endswith('.txt'):
                if name.startswith('pep-'):
                    peps_found = 1
                else:
                    self.process_txt(directory, name)
        if peps_found:
            self.process_peps(directory)
        if not recurse:
            del names[:]

    def process_txt(self, directory, name):
        settings = self.get_settings('.txt', directory)
        settings._source = os.path.normpath(os.path.join(directory, name))
        settings._destination = settings._source[:-4]+'.html'
        if not self.initial_settings.silent:
            print >>sys.stderr, '    ::: Processing .txt:', name
            sys.stderr.flush()
        try:
            core.publish_file(source_path=settings._source,
                              destination_path=settings._destination,
                              reader_name='standalone',
                              parser_name='restructuredtext',
                              writer_name='html',
                              settings=settings)
        except ApplicationError, error:
            print >>sys.stderr, ('        Error (%s): %s'
                                 % (error.__class__.__name__, error))

    def process_peps(self, directory):
        settings = self.get_settings('PEPs', directory)
        old_directory = os.getcwd()
        os.chdir(directory)
        if self.initial_settings.silent:
            argv = ['-q']
        else:
            print >>sys.stderr, '    ::: Processing PEPs:'
            sys.stderr.flush()
            argv = []
        pep2html.docutils_settings = settings
        try:
            pep2html.main(argv)
        except Exception, error:
            print >>sys.stderr, ('        Error (%s): %s'
                                 % (error.__class__.__name__, error))
        os.chdir(old_directory)


if __name__ == "__main__":
    Builder().run()
