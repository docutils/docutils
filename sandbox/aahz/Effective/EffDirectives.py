import sys
import os
import re

from docutils import nodes
from docutils.parsers.rst import directives
from docutils.parsers.rst.languages import en

import EffTransformer

registry = directives._directive_registry
registry['chapter-list'] = ('EffDirectives', 'chapter_list')
registry['item-list'] = ('EffDirectives', 'item_list')
registry['index'] = ('EffDirectives', 'index_directive')
registry['include-code'] = ('EffDirectives', 'include_code')
registry['include-output'] = ('EffDirectives', 'include_output')
registry['comment'] = ('EffDirectives', 'comment')

en.directives['chapter-list'] = 'chapter-list'
en.directives['item-list'] = 'item-list'
en.directives['index'] = 'index'
en.directives['include-code'] = 'include-code'
en.directives['include-output'] = 'include-output'
en.directives['comment'] = 'comment'



def chapter_list(name, arguments, options, content, lineno,
        content_offset, block_text, state, state_machine):
    document = state_machine.document
    chapters = {}
    chapter_num = 1
    for chapter in content:
        chapter = chapter.strip()
        chapters[chapter] = chapter_num
        chapter_num += 1
    document.chapters = chapters
    return []

chapter_list.content = 1



def item_list(name, arguments, options, content, lineno,
        content_offset, block_text, state, state_machine):
    document = state_machine.document
    items = {}
    document.items = items
    return []

item_list.content = 1



class index_entry(nodes.Inline, nodes.Invisible, nodes.Element): pass
class index_entry_level(nodes.Inline, nodes.TextElement): pass

re_index_levels = re.compile(r';\w*')

def index_directive(name, arguments, options, content, lineno,
        content_offset, block_text, state, state_machine):
    #pending = nodes.pending(EffTransformer.Index, {'entries': content})
    #state_machine.document.note_pending(pending)
    #return [pending]
    # XXX
    entries = []
    message_set = []
    for entry in content:
        levels = re_index_levels.split(entry)
        tmp = []
        for level in levels:
            textnodes, messages = state.inline_text(level, lineno)
            entry_level = index_entry_level(level, '', *textnodes)
            tmp.append(entry_level)
            message_set.extend(messages)
        index = index_entry(entry, *tmp)
        entries.append(index)
    pending = nodes.pending(EffTransformer.Index, {'entries': entries})
    state_machine.document.note_pending(pending)
    return [pending] + message_set

index_directive.content = 1



MAX_WIDTH = 65

def _checkwidth(fname, code):
    code = code.split('\n')
    i = 1
    for line in code:
        if len(line) > MAX_WIDTH:
            print "Line %s in %s exceeds %s chars" % (i, fname, MAX_WIDTH)
            print "    ", line
        i += 1

def include_code(name, arguments, options, content, lineno,
        content_offset, block_text, state, state_machine):
    #obj = state_machine.document.attributes
    #print >> sys.stderr, obj
    #print >> sys.stderr, dir(obj)
    document = state_machine.document
    dirname = _getDocDir(document)
    fname = os.path.join(dirname, arguments[0])
    code = open(fname).read()
    _checkwidth(fname, code)
    node = nodes.literal_block(code, code)
    return [node]

include_code.arguments = (0, 1, 0)



def include_output(name, arguments, options, content, lineno,
        content_offset, block_text, state, state_machine):
    document = state_machine.document
    dirname = _getDocDir(document)
    fname = os.path.join(dirname, arguments[0])
    cmd = os.environ['PYTHON'] + ' ' + fname
    f_input, f_output = os.popen4(cmd)
    output = f_output.read()
    f_output.close()
    return [nodes.literal_block(output, output)]

include_output.arguments = (0, 1, 0)



def comment(name, arguments, options, content, lineno,
        content_offset, block_text, state, state_machine):
    return []

comment.content = 1



def _getDocDir(document):
    source = document.current_source
    if source is None:
        return os.getcwd()
    else:
        dirname = os.path.dirname(os.path.abspath(source))
        if dirname is None:
            return os.getcwd()
        else:
            return dirname
