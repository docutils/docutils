
import epydoc.markup.epytext as epytext
import xml.dom.minidom as minidom
import docutils.nodes as nodes
import docutils.parsers

class Parser(docutils.parsers.Parser):
    """The epytext parser."""

    supported = ('epytext')

    settings_spec = (
        'epytext Parser Options',
        None,
        ())

    def __init__(self):
        pass

    def parse(self, inputstring, document):
        errors = []
        epytext_tree = epytext.parse(inputstring)
        
        self.setup_parse(inputstring, document)
        for child in self._parse(epytext_tree):
            self.document.append(child)
        self.finish_parse()
        
    def _parse(self, tree):
        if isinstance(tree, minidom.Document):
            return self._parse(tree.childNodes[0])
        if isinstance(tree, minidom.Text):
            return nodes.Text(tree.data)

        # Get children.
        children = [self._parse(c) for c in tree.childNodes]

        if tree.tagName == 'epytext':
            return children
        if tree.tagName == 'para':
            return nodes.paragraph('','', *children)
        if tree.tagName == 'section':
            return nodes.section('', *children)
        if tree.tagName == 'heading':
            return nodes.title('','', *children)
        if tree.tagName == 'fieldlist':
            return nodes.field_list('', *children)
        if tree.tagName == 'field':
            return nodes.field('', *self._parse_field(tree, children))
        if tree.tagName == 'literalblock':
            return nodes.literal_block('','', *children)
        if tree.tagName == 'doctestblock':
            return nodes.doctest_block('','', *children)
        if tree.tagName == 'ulist':
            return nodes.bullet_list('', *children)
        if tree.tagName == 'olist':
            return nodes.enumerated_list('', *children)
        if tree.tagName == 'li':
            return nodes.list_item('', *children)
        if tree.tagName == 'link':
            # [XX] discards link target.
            name, target = children
            return nodes.title_reference('','', name)
        if tree.tagName == 'uri':
            name, target = children
            return nodes.reference('','', name, refuid=target.astext())
        if tree.tagName == 'code':
            return nodes.literal('','', *children)
        if tree.tagName == 'math':
            return nodes.emphasis('','', *children)
        if tree.tagName == 'italic':
            return nodes.emphasis('','', *children)
        if tree.tagName == 'bold':
            return nodes.strong('','', *children)
        if tree.tagName == 'indexed': 
            # [XX] doesn't mark the fact that it's indexedd
            return nodes.emphasis('','', *children)
        if tree.tagName == 'symbol':
            # use substitutions.  
            # [XX] this needs to be fixed!
            return nodes.Text(children[0])
        elif tree.tagName in ('tag', 'arg', 'name', 'target'):
            return children[0]
        else:
            raise ValueError, ('unknown %s' % tree.tagName)

    def _parse_field(self, tree, children):
        numargs = 0
        while tree.childNodes[numargs+1].tagName == 'arg': numargs += 1
        tag = children[0]
        args = children[1:1+numargs]
        body = children[1+numargs:]
        name = nodes.field_name('','', *children[:1+numargs])
        body = nodes.field_body('', *children[1+numargs:])
        return (name, body)
