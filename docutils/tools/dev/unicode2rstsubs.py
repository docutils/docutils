#! /usr/bin/env python3
# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This program has been placed in the public domain.

"""
unicode2subfiles.py -- produce character entity files (reSructuredText
substitutions) from the W3C master unicode.xml file.

This program extracts character entity and entity set information from a
unicode.xml file and produces multiple reStructuredText files (in the current
directory) containing substitutions.  Entity sets are from ISO 8879 & ISO
9573-13 (combined), MathML, and HTML4.  One or two files are produced for each
entity set; a second file with a "-wide.txt" suffix is produced if there are
wide-Unicode characters in the set.

The input file, unicode.xml, is maintained as part of the MathML 2
Recommentation XML source, and is available from
<https://www.w3.org/2003/entities/xml/>.
"""

from __future__ import annotations

import os
import re
import sys
from typing import TYPE_CHECKING
from xml.parsers.expat import ParserCreate

if TYPE_CHECKING:
    from typing import BinaryIO, NoReturn, TextIO
    from xml.parsers.expat import XMLParserType


usage_msg = """Usage: %s [unicode.xml]\n"""


def usage(prog: str, status: int = 0, msg: str | None = None) -> NoReturn:
    sys.stderr.write(usage_msg % prog)
    if msg:
        sys.stderr.write(msg + '\n')
    sys.exit(status)


def main(argv: list[str] | None = None) -> None:
    if argv is None:
        argv = sys.argv
    if len(argv) == 2:
        inpath = argv[1]
    elif len(argv) > 2:
        usage(argv[0], 2,
              'Too many arguments (%s): only 1 expected.' % (len(argv) - 1))
    else:
        inpath = 'unicode.xml'
    if not os.path.isfile(inpath):
        usage(argv[0], 1, 'No such file: "%s".' % inpath)
    infile = open(inpath, mode='rb')
    process(infile)


def process(infile: BinaryIO) -> None:
    grouper = CharacterEntitySetExtractor(infile)
    grouper.group()
    grouper.write_sets()


class CharacterEntitySetExtractor:

    """
    Extracts character entity information from unicode.xml file, groups it by
    entity set, and writes out reStructuredText substitution files.
    """

    unwanted_entity_sets = ['stix',     # unknown, buggy set
                            'predefined']

    header = """\
.. This data file has been placed in the public domain.
.. Derived from the Unicode character mappings available from
   <https://www.w3.org/2003/entities/xml/>.
   Processed by unicode2rstsubs.py, part of Docutils:
   <https://docutils.sourceforge.io>.
"""

    def __init__(self, infile: BinaryIO) -> None:
        self.infile = infile
        """Input unicode.xml file."""

        self.parser: XMLParserType = self.setup_parser()
        """XML parser."""

        self.elements: list[str] = []
        """Stack of element names.  Last is current element."""

        self.sets: dict[str, dict[str, str]] = {}
        """Mapping of charent set name to set dict."""

        self.charid: str | None = None
        """Current character's "id" attribute value."""

        self.descriptions: dict[str, str] = {}
        """Mapping of character ID to description."""

    def setup_parser(self) -> XMLParserType:
        parser = ParserCreate()
        parser.StartElementHandler = self.StartElementHandler
        parser.EndElementHandler = self.EndElementHandler
        parser.CharacterDataHandler = self.CharacterDataHandler
        return parser

    def group(self) -> None:
        self.parser.ParseFile(self.infile)

    def StartElementHandler(self, name: str, attributes) -> None:
        self.elements.append(name)
        handler = name + '_start'
        if hasattr(self, handler):
            getattr(self, handler)(name, attributes)

    def EndElementHandler(self, name: str) -> None:
        assert self.elements[-1] == name, \
               'unknown end-tag %r (%r)' % (name, self.element)
        self.elements.pop()
        handler = name + '_end'
        if hasattr(self, handler):
            getattr(self, handler)(name)

    def CharacterDataHandler(self, data) -> None:
        handler = self.elements[-1] + '_data'
        if hasattr(self, handler):
            getattr(self, handler)(data)

    def character_start(self, name: str, attributes) -> None:
        self.charid = attributes['id']

    def entity_start(self, name, attributes) -> None:
        set_ = self.entity_set_name(attributes['set'])
        if not set_:
            return
        if set_ not in self.sets:
            print('bad set: %r' % set_)
            return
        entity = attributes['id']
        assert (entity not in self.sets[set_]
                or self.sets[set_][entity] == self.charid
                ), ('sets[%r][%r] == %r (!= %r)'
                    % (set_, entity, self.sets[set_][entity], self.charid))
        self.sets[set_][entity] = self.charid

    def description_data(self, data) -> None:
        self.descriptions.setdefault(self.charid, '')
        self.descriptions[self.charid] += data

    entity_set_name_pat = re.compile(r'[0-9-]*(.+)$')
    """Pattern to strip ISO numbers off the beginning of set names."""

    def entity_set_name(self, name: str) -> str | None:
        """
        Return lowcased and standard-number-free entity set name.
        Return ``None`` for unwanted entity sets.
        """
        match = self.entity_set_name_pat.match(name)
        name = match.group(1).lower()
        if name in self.unwanted_entity_sets:
            return None
        self.sets.setdefault(name, {})
        return name

    def write_sets(self) -> None:
        sets = sorted(self.sets.keys())
        for set_name in sets:
            self.write_set(set_name)

    def write_set(self, set_name: str, wide: bool = False) -> None:
        if wide:
            outname = set_name + '-wide.txt'
        else:
            outname = set_name + '.txt'
        outfile = open(outname, 'w', encoding='ascii')
        print('writing file "%s"' % outname)
        outfile.write(self.header + '\n')
        set_ = self.sets[set_name]
        entities = sorted((e.lower(), e) for e in set_.keys())
        longest = 0
        for _, entity_name in entities:
            longest = max(longest, len(entity_name))
        has_wide = False
        for _, entity_name in entities:
            has_wide = self.write_entity(
                set_, set_name, entity_name, outfile, longest, wide,
            ) or has_wide
        if has_wide and not wide:
            self.write_set(set_name, wide=True)

    def write_entity(
        self,
        set_: dict[str, str],
        set_name: str,
        entity_name: str,
        outfile: TextIO,
        longest: int,
        wide: bool = False,
    ) -> bool:
        charid = set_[entity_name]
        if not wide:
            for code in charid[1:].split('-'):
                if int(code, 16) > 0xFFFF:
                    return True         # wide-Unicode character
        codes = ' '.join('U+%s' % code for code in charid[1:].split('-'))
        outfile.write('.. %-*s unicode:: %s .. %s\n'
                      % (longest + 2, '|' + entity_name + '|',
                         codes, self.descriptions[charid]))
        return False


if __name__ == '__main__':
    sys.exit(main())
