from docutils.writers import html4css1
from docutils import nodes

class Writer(html4css1.Writer):
    W_init = html4css1.Writer.__init__
    def __init__(self):
        self.W_init()
        self.translator_class = MyHTMLTranslator

def find_visible_child(node, starting_index, direction):
    if direction == "forwards":
        step = 1
        end = len(node)
    else:
        step = -1
        end = -1
    for i in range(starting_index, end, step):
        if not isinstance(node[i], nodes.Invisible):
            return node[i]
    else:
        return None

class MyHTMLTranslator(html4css1.HTMLTranslator):
    def visit_title(self, node):
        self.body.append(self.starttag(node, 'b', '',
                                       STYLE="font-variant: small-caps"))
        self.context.append('</b>\n')
    def visit_document(self, node):
        pass
    def depart_document(self, node):
        pass
    def visit_paragraph(self, node):
        if isinstance(node.parent, nodes.section):
            index = node.parent.index(node)
            prev = find_visible_child(node.parent, index - 1, "backwards")
            if isinstance(prev, nodes.title):
                self.body.append(self.starttag(node, 'blockquote', '\n'))
            else:
                self.body.append('<p>\n')
        else:
            html4css1.HTMLTranslator.visit_paragraph(self, node)

    def depart_paragraph(self, node):
        if isinstance(node.parent, nodes.section):
            index = node.parent.index(node)
            next = find_visible_child(node.parent, index + 1, "forwards")
            if not next:
                self.body.append('\n</blockquote>\n')
            else:
                self.body.append("\n");
        else:
            # this tends to include "</p>"s which advogato then
            # discards.  I don't care that much...
            html4css1.HTMLTranslator.depart_paragraph(self, node)
        
    def visit_section(self, node):
        pass
    def depart_section(self, node):
        pass
