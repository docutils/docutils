from docutils import nodes
from docutils import transforms 

#class index_entry(nodes.Inline, nodes.TextElement): pass

class Index(transforms.Transform):
    default_priority = 250

    def apply(self):
        pending = self.startnode
        curr_index = pending.parent.index(pending)
        if curr_index == 0:
            error = self.document.reporter.error(
                'Index directive must follow a text node',
                nodes.literal_block(pending.rawsource, pending.rawsource),
                line=pending.line)
            pending.parent.replace(pending, error)
            return
        entries = []
        for entry in pending.details['entries']:
            entries.append(entry)
        sibling = pending.parent[curr_index-1]
        sibling.children[0:0] = entries
        pending.parent.remove(pending)
