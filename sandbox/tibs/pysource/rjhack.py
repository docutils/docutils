#!/usr/bin/env python

# Author: Richard Jones
# Contact: richard@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
A front end to the Docutils Publisher, taking Python source and producing HTML.
"""

import locale
try:
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

import visit, transform

from docutils.parsers import Parser
class SourceParser(Parser):
    supported = ('pysource',)
    cmdline_options = (
        'PySource Parser Options',
        None,
        (('Be verbose while parsing', ['--verbose-parse'],
          {'action': 'store_true'}),
        )
    )
    def parse(self, inputstring, document):
        thing = visit.Module(inputstring,
            verbose=document.options.verbose_parse)
        process = transform.Process(with_groups=0, document=document)
        process(thing)

from docutils.readers import Reader
class SourceReader(Reader):
    def read(self, source, parser, options):
        self.source = source
        if not self.parser:
            self.parser = parser
        self.options = options
        # we want the input as the filename, not the file content
        self.source.source.close()
        self.input = self.source.source_path
        self.parse()
        self.transform()
        return self.document

from docutils.core import publish, default_description
description = ('Generates (X)HTML documents from Python sources.  '
                + default_description)
parser = SourceParser()
reader = SourceReader(parser, 'pysource')
publish(reader=reader, parser=parser, writer_name='html',
    description=description)

