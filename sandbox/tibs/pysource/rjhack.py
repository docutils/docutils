#!/usr/bin/env python

# Author: Richard Jones
# Contact: richard@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
A front end to the Docutils Publisher, taking Python source and producing HTML.
"""

import os, locale
try:
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

import visit, transform

from docutils.parsers.rst import Parser
class SourceParser(Parser):
    supported = ('pysource',)
    settings_spec = (
        'PySource Parser Options',
        None,
        (('Be verbose while parsing', ['--verbose-parse'],
          {'action': 'store_true'}),
         )) + Parser.settings_spec
    def parse(self, filename, document):
        if os.path.isdir(filename):
            thing = visit.Package(document.settings, filename)
        else:
            thing = visit.Module(document.settings, filename)
        process = transform.Process(with_groups=0, document=document)
        process(thing)

from docutils.readers import Reader
class SourceReader(Reader):
    def read(self, source, parser, settings):
        self.source = source
        if not self.parser:
            self.parser = parser
        self.settings = settings
        # we want the input as the filename, not the file content
        self.source.source.close()
        self.input = self.source.source_path
        self.parse()
        return self.document

from docutils.core import publish_cmdline, default_description
description = ('Generates (X)HTML documents from Python sources.  '
               + default_description)
parser = SourceParser()
reader = SourceReader(parser, 'pysource')
publish_cmdline(reader=reader, parser=parser, writer_name='html',
                description=description)

