"""
Move footnotes to the end of the document.
"""

from docutils import transforms
from docutils import nodes

class EndNotes(transforms.Transform):

    default_priority = 950
    
    def apply(self):
        all_footnotes = []
        assert isinstance(self.document, nodes.document)
        for footnote in self.document.traverse(nodes.footnote):
            all_footnotes.append(footnote)
            footnote.parent.remove(footnote)
        self.document += all_footnotes
