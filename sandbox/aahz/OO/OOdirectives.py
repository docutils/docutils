import sys

from docutils import nodes
from docutils.parsers.rst import directives

registry = directives._directive_registry
registry['index'] = ('OOdirectives', 'index_directive')
registry['include-code'] = ('OOdirectives', 'include_code')
registry['include-output'] = ('OOdirectives', 'include_output')



class index_entry(nodes.General, nodes.Element): pass
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
    print >> sys.stderr, state_machine.document.options._source
    return []
    document = state_machine.document
    source = document.options._source
    if source is not None:
        if has_attr(

include_code.arguments = (0, 1, 0)



def include_output(name, arguments, options, content, lineno,
        content_offset, block_text, state, state_machine):
    return []

include_output.arguments = (0, 1, 0)
