"""Custom directives"""

__author__ = "Patrick K. O'Brien <pobrien@orbtech.com>"
__cvsid__ = "$Id$"
__revision__ = "$Revision$"[11:-2]

import sys
import os

from docutils import nodes
from docutils.parsers.rst import directives
from docutils.parsers.rst.languages import en

registry = directives._directive_registry
registry['index'] = ('OOdirectives', 'index_directive')
registry['include-code'] = ('OOdirectives', 'include_code')
registry['include-output'] = ('OOdirectives', 'include_output')

en.directives['index'] = 'index'
en.directives['include-code'] = 'include-code'
en.directives['include-output'] = 'include-output'


#class index_entry(nodes.General, nodes.Element): pass
class index_entry(nodes.Inline, nodes.TextElement): pass


def index_directive(name, arguments, options, content, lineno,
        content_offset, block_text, state, state_machine):
    #nodes = []
    #for entry in content:
    #    nodes.append(index_entry(entry, entry))
    #return nodes
    entries = '\n'.join(content)
    return [index_entry(entries,entries)]

index_directive.content = 1


def include_code(name, arguments, options, content, lineno,
        content_offset, block_text, state, state_machine):
    #obj = state_machine.document.attributes
    #print >> sys.stderr, obj
    #print >> sys.stderr, dir(obj)
    document = state_machine.document
    dirname = getDocDir(document)
    fname = os.path.join(dirname, arguments[0])
    code = open(fname).read()
    return [nodes.literal_block(code, code)]

include_code.arguments = (0, 1, 0)


def include_output(name, arguments, options, content, lineno,
        content_offset, block_text, state, state_machine):
    document = state_machine.document
    dirname = getDocDir(document)
    fname = os.path.join(dirname, arguments[0])
    cmd = os.environ['PYTHON'] + ' ' + fname
    f_input, f_output = os.popen4(cmd)
    output = f_output.read()
    f_output.close()
    return [nodes.literal_block(output, output)]

include_output.arguments = (0, 1, 0)


def getDocDir(document):
    source = document.current_source
    if source is None:
        return os.getcwd()
    else:
        dirname = os.path.dirname(os.path.abspath(source))
        if dirname is None:
            return os.getcwd()
        else:
            return dirname
