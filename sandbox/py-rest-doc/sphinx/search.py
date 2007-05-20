# -*- coding: utf-8 -*-
"""
    sphinx.search
    ~~~~~~~~~~~~~

    Create a search index for offline search.

    :copyright: 2007 by Armin Ronacher.
    :license: Python license.
"""
import re

from docutils.nodes import Text, NodeVisitor
from .stemmer import PorterStemmer
from .json import dump_json


word_re = re.compile(r'\w+(?u)')


class Stemmer(PorterStemmer):
    """
    All those porter stemmer implementations look hideous.
    make at least the stem method nicer.
    """

    def stem(self, word):
        return PorterStemmer.stem(self, word, 0, len(word) - 1)


class WordCollector(NodeVisitor):

    def __init__(self, document):
        NodeVisitor.__init__(self, document)
        self.found_words = []

    def dispatch_visit(self, node):
        if node.__class__ is Text:
            self.found_words.extend(word_re.findall(node.astext()))


class IndexBuilder(object):

    def __init__(self):
        self._filenames = {}
        self._mapping = {}
        self._titles = {}
        self._categories = {}
        self._stemmer = Stemmer()

    def dump(self, stream):
        stream.write(dump_json([
            [k for k, v in sorted(self._filenames.items(),
                                  key=lambda x: x[1])],
            dict(item for item in sorted(self._categories.items(),
                                         key=lambda x: x[0])),
            [v for k, v in sorted(self._titles.items(),
                                  key=lambda x: x[0])],
            dict(item for item in sorted(self._mapping.items(),
                                         key=lambda x: x[0])),
        ]))

    def feed(self, filename, category, title, doctree):
        file_id = self._filenames.setdefault(filename, len(self._filenames))
        self._titles[file_id] = title
        visitor = WordCollector(doctree)
        doctree.walk(visitor)
        self._categories.setdefault(category, set()).add(file_id)
        for word in word_re.findall(title) + visitor.found_words:
            self._mapping.setdefault(self._stemmer.stem(word.lower()),
                                     set()).add(file_id)
