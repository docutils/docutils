from docutils.writers import html4css1
from docutils import nodes

class Writer(html4css1.Writer):
    W_init = html4css1.Writer.__init__
    def __init__(self):
        self.W_init()
        self.translator_class = MyHTMLTranslator

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
        index = node.parent.index(node) - 1
        while index >= 0:
            if not isinstance(node.parent[index], nodes.Invisible):
                if isinstance(node.parent[index], nodes.paragraph):
                    self.body.append('\n<p>\n')
                else:
                    self.body.append(self.starttag(node, 'blockquote', '\n'))
                break
            index -= 1
        else:
            self.body.append(self.starttag(node, 'blockquote', '\n'))
    def depart_paragraph(self, node):
        index = node.parent.index(node) + 1
        while index < len(node.parent):
            if not isinstance(node.parent[index], nodes.Invisible):
                if not isinstance(node.parent[index], nodes.paragraph):
                    self.body.append('\n</blockquote>\n')
                break
            index += 1
        else:
            self.body.append('\n</blockquote>\n')
    def visit_section(self, node):
        pass
    def depart_section(self, node):
        pass
