#!/usr/bin/env python

"""
Generates .html from all the .txt files in a directory.

Ordinary .txt files are understood to be standalone reStructuredText.
Files named ``pep-*.txt`` are interpreted as PEPs (either old-style or
new reStructuredText PEPs).
"""
# Once PySource is here, build .html from .py as well.

__docformat__ = 'reStructuredText'


import sys
import os
import os.path
import copy
import docutils
from docutils import ApplicationError
from docutils import core, frontend, io
from docutils.parsers import rst
from docutils.readers import pep
from docutils.writers import pep_html
import pep2html


usage = '%prog [options] [<directory> ...]'
description = ('Generates .html from all the .txt files (including PEPs) '
               'in each <directory> (default is the current directory).')


class OptionSpec(docutils.OptionSpec):

    """
    Command-line options for the ``buildhtml.py`` front end.
    """

    # Can't be included in OptionParser below because we don't want to
    # override the base class.
    cmdline_options = (
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


class Builder:

    def run(self, directory=None, recurse=1):
        self.process_command_line()
        initial_options = self.get_options()
        recurse = recurse and initial_options.recurse
        self.setup_html_publisher()
        if directory:
            self.directories = [directory]
        elif self.cmdline_options._directories:
            self.directories = self.cmdline_options._directories
        else:
            self.directories = [os.getcwd()]
        for directory in self.directories:
            os.path.walk(directory, self.visit, recurse)

    def visit(self, recurse, directory, names):
        options = self.get_options(directory)
        if not options.silent:
            print >>sys.stderr, '/// Processing directory:', directory
            sys.stderr.flush()
        peps_found = 0
        for name in names:
            if name.endswith('.txt'):
                if name.startswith('pep-'):
                    peps_found = 1
                else:
                    self.process_txt(directory, name, options)
        if peps_found:
            self.process_peps(options, directory)
        if not recurse:
            del names[:]

    def process_txt(self, directory, name, options):
        options._source = os.path.normpath(os.path.join(directory, name))
        options._destination = options._source[:-4]+'.html'
        self.pub.options = options
        if not options.silent:
            print >>sys.stderr, '    ::: Processing .txt:', name
            sys.stderr.flush()
        self.pub.source = io.FileInput(options, source_path=options._source)
        self.pub.destination = io.FileOutput(
            options, destination_path=options._destination)
        try:
            self.pub.publish()
        except ApplicationError, error:
            print >>sys.stderr, ('        Error (%s): %s'
                                 % (error.__class__.__name__, error))

    def process_peps(self, options, directory):
        old_directory = os.getcwd()
        os.chdir(directory)
        if options.silent:
            argv = ['-q']
        else:
            print >>sys.stderr, '    ::: Processing PEPs:'
            sys.stderr.flush()
            argv = []
        pep2html.docutils_options = options
        try:
            pep2html.main(argv)
        except Exception, error:
            print >>sys.stderr, ('        Error (%s): %s'
                                 % (error.__class__.__name__, error))
        os.chdir(old_directory)

    def process_command_line(self):
        option_parser = OptionParser(
            components=(OptionSpec, pep.Reader, rst.Parser, pep_html.Writer),
            usage=usage, description=description)
        self.option_defaults = option_parser.get_default_values()
        self.relative_path_options = option_parser.relative_path_options
        frontend.make_paths_absolute(self.option_defaults.__dict__,
                                     self.relative_path_options)
        config_parser = frontend.ConfigParser()
        config_parser.read_standard_files()
        self.config_settings = config_parser.get_section('options')
        frontend.make_paths_absolute(self.config_settings,
                                     self.relative_path_options)
        self.cmdline_options = option_parser.parse_args(
            values=frontend.Values())   # no defaults

    def get_options(self, directory=None):
        """
        Return an option values object, from multiple sources.

        Copy the option defaults, overlay the startup config file settings,
        then the local config file settings, then the command-line options.
        Assumes the current directory has been set.
        """
        options = copy.deepcopy(self.option_defaults)
        options.__dict__.update(self.config_settings)
        if directory:
            config_parser = frontend.ConfigParser()
            config_parser.read(os.path.join(directory, 'docutils.conf'))
            local_config = config_parser.get_section('options')
            frontend.make_paths_absolute(
                local_config, self.relative_path_options, directory)
            options.__dict__.update(local_config)
        options.__dict__.update(self.cmdline_options.__dict__)
        return options

    def setup_html_publisher(self):
        pub = core.Publisher()
        pub.set_reader(reader_name='standalone',
                       parser_name='restructuredtext', parser=None)
        pub.set_writer(writer_name='html')
        self.pub = pub


if __name__ == "__main__":
    Builder().run()
