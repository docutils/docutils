import sys
import os

from docutils import nodes
from docutils.parsers.rst import directives
from docutils.parsers.rst.languages import en

import sys
from warnings import warn
import re

import docutils
from docutils import nodes, utils, writers, languages

from docutils.parsers import rst

en.roles['chapter'] = 'chapter'
en.roles['figure'] = 'figure'
en.roles['item'] = 'item'
en.roles['list'] = 'list'

class Inliner(rst.states.Inliner):
    _interpreted_roles = rst.states.Inliner._interpreted_roles.copy()
    _interpreted_roles.update({
        'chapter': 'chapter_role',
        'figure': 'figure_role',
        'item': 'item_role',
        'list': 'list_role',
        })

    def chapter_role(self, role, rawtext, text, lineno):
        return [nodes.emphasis(rawtext, text)], []

    def figure_role(self, role, rawtext, text, lineno):
        return [nodes.emphasis(rawtext, text)], []

    def item_role(self, role, rawtext, text, lineno):
        return [nodes.emphasis(rawtext, text)], []

    def list_role(self, role, rawtext, text, lineno):
        return [nodes.emphasis(rawtext, text)], []

class Parser(rst.Parser):
    def __init__(self, *args, **kwargs):
        rst.Parser.__init__(self, *args, **kwargs)
