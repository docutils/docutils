import cStringIO, cgi, sys, urllib
import docutils.utils
from docutils.parsers.restructuredtext import Parser

class DumbPythonPointFormatter:
    def __init__(self):
        self.out = cStringIO.StringIO()
        self.w = self.out.write
        self.section = 0
        self.closers = []
        self.slidenum = 1

    def format(self, node):
        '''Format a node
        '''
        for entry in node:
            self.formatOneTag(entry)

    def formatOneTag(self, tag):
        if tag.tagname == '#text':
            meth = self.format__text
        else:
            if not hasattr(self, 'format_'+tag.tagname):
                print >>sys.stderr, '**skipping %s'%tag.tagname
                return
            else:
                meth = getattr(self, 'format_'+tag.tagname)
        meth(tag)

    def open_slide(self, node):
        if node.attributes.has_key('dupname'):
            name = node.attributes['dupname']
        else:
            name = node.attributes['name']
        self.w('<slide id="Slide%03d" title="%s">\n'%(self.slidenum, name))
        self.slidenum += 1

    def open_frame(self):
        self.w('<frame x="160" y="72" width="600" height="432" leftmargin="36" rightmargin="0">\n')

    #
    # Root Element
    #
    # ((title, subtitle?)?, docinfo?, %structure.model;)
    #
    def format_document(self, document):
        ''' ((title, subtitle?)?, docinfo?, %structure.model;)

            
        '''
        self.document = document

        # make sure the structure is what we're expecting
        self.w('<presentation>\n')

        # TODO: get this into the stx
        self.w('<stylesheet module="modern" function="getParagraphStyles"/>\n')

        self.w('<section name="Main">\n')

        # TODO: get this into the stx
        self.w('<fixedimage filename="logo.gif" x="0" y="0" width="134" height="70"/>\n')

        # now for the body
        for entry in document:
            assert entry.tagname == 'section'
            self.formatOneTag(entry)

        self.w('</section>\n')
        self.w('</presentation>\n')

        return self.out.getvalue()

    def format_title(self, node):
        self.w('<para style="Heading2">')
        if node.children: self.format(node)
        self.w('</para>\n')

    def format_section(self, node):
        self.open_slide(node)
        self.open_frame()
        if node.children: self.format(node)
        self.w('</frame>\n')
        self.w('</slide>\n')

    def format_paragraph(self, node):
        ''' %text.model;
        '''
        # TODO: there are situations where the <p> </p> are unnecessary
        self.w('<para>')
        if node.children: self.format(node)
        self.w('</para>\n')

    # Simple lists
    def format_bullet_list(self, node):
        if node.children: self.format(node)

    def format_enumerated_list(self, node):
        if node.children: self.format(node)

    def format_list_item(self, node):
        self.w('<para style="Bullet">')
        if node.children: self.format(node[0])
        self.w('</para>\n')

    # Definition List
    def format_definition_list(self, node):
        if node.children: self.format(node)

    def format_definition_list_item(self, node):
        '''  (term, classifier?, definition)
        '''
        if node.children: self.format(node)

    def format_term(self, node):
        ''' %text.model;
        '''
        self.w('<para><b>')
        if node.children:self.format(node[0])
        self.w('</b>')

    def format_classifier(self, node):
        ''' %text.model;
        '''
        # TODO: handle the classifier better
        self.w('<i>')
        if node.children: self.format(node[0])
        self.w('</i>')

    def format_definition(self, node):
        ''' (%body.elements;)+
        '''
        self.w('</para>\n<para style="Definition">')
        for child in node.children:
            self.w(child[0].data)
        self.w('</para>\n')

    # Literal Block
    def format_literal_block(self, node):
        self.w('<pre>')
        if node.children: self.format(node)
        self.w('</pre>\n')

    # Block Quote
    def format_block_quote(self, node):
        self.w('<para style="Indent">')
        if node.children: self.format(node)
        self.w('</para>\n')

    def format_image(self, node):
        ''' EMPTY
            uri       CDATA     #REQUIRED
            alt       CDATA     #IMPLIED
            height    NMTOKEN   #IMPLIED
            width     NMTOKEN   #IMPLIED
            scale     NMTOKEN   #IMPLIED
        '''
        attrs = node.attributes
        l = ['src="%(uri)s"'%attrs]
        # TODO: scale
        self.w('<image filename="%s">'%node.attributes['uri'])

    #
    # Tables:
    #  NOT IN DOM YET
    #
    def format_table(self, node):
        '''
            +------------------------+------------+----------+----------+
            | Header row, column 1   | Header 2   | Header 3 | Header 4 |
            | (header rows optional) |            |          |          |
            +========================+============+==========+==========+
            | body row 1, column 1   | column 2   | column 3 | column 4 |
            +------------------------+------------+----------+----------+
            | body row 2             | Cells may span columns.          |
            +------------------------+------------+---------------------+
            | body row 3             | Cells may  | - Table cells       |
            +------------------------+ span rows. | - contain           |
            | body row 4             |            | - body elements.    |
            +------------------------+------------+---------------------+
        '''
        self.w('<table border=1>\n')
        if node.children: self.format(node)
        self.w('</table>\n')

    def format_tgroup(self, node):
        # we get the number of columns, if that's important
        if node.children: self.format(node)

    def format_colspec(self, node):
        # we get colwidth, but don't need it
        pass

    def format_thead(self, node):
        for row in node.children:
            self.w('<tr>')
            for cell in row.children:
                s = ''
                attrs = cell.attributes
                if attrs.has_key('morecols'):
                    s = s + ' colspan=%d'%(attrs['morecols']+1)
                if attrs.has_key('morerows'):
                    s = s + ' rowspan=%d'%(attrs['morerows']+1)
                self.w('<th valign="top" align="left"%s>'%s)
                if cell.children: self.format(cell)
                self.w('</th>\n')
            self.w('</tr>\n')

    def format_tbody(self, node):
        for row in node.children:
            self.w('<tr>')
            for cell in row.children:
                s = ''
                attrs = cell.attributes
                if attrs.has_key('morecols'):
                    s = s + ' colspan=%d'%(attrs['morecols']+1)
                if attrs.has_key('morerows'):
                    s = s + ' rowspan=%d'%(attrs['morerows']+1)
                self.w('<td valign="top" align="left"%s>'%s)
                if cell.children: self.format(cell)
                self.w('</td>\n')
            self.w('</tr>\n')

    #
    # Inline Elements
    #
    # Inline elements occur within the text contents of body elements. Some
    # nesting of inline elements is allowed by these definitions, with the
    # following caveats:
    # - An inline element may not contain a nested element of the same type
    #   (e.g. <strong> may not contain another <strong>).
    # - Nested inline elements may or may not be supported by individual
    #   applications using this DTD.
    # - The inline elements <footnote_reference>, <literal>, and <image> do
    #   not support nesting.
    #
    #  What that means is that all of these take (%text.model;) except:
    #   literal (#PCDATA)
    #   footnote_reference (#PCDATA)
    #
    # text.model:
    # (#PCDATA | %inline.elements;)*
    #
    def format_emphasis(self, node):
        ''' (%text.model;)
        '''
        self.w('<i>')
        if node.children: self.format(node)
        self.w('</i>')

    def format_strong(self, node):
        ''' (%text.model;)
        '''
        self.w('<b>')
        if node.children: self.format(node)
        self.w('</b>')

    def format_interpreted(self, node):
        ''' (%text.model;)
            type      CDATA     #IMPLIED
        '''
        pass #raise NotImplementedError, node

    def format_literal(self, node):
        ''' (#PCDATA)
        '''
        self.w('<tt>')
        for literal in node.children:
            self.w(cgi.escape(literal.data))
        self.w('</tt>')

    def format_reference(self, node):
        ''' (%text.model;)
            %reference.atts;
            %anonymous.att;
        '''
        attrs = node.attributes
        doc = self.document
        ok = 1
        print node
        if attrs.has_key('refuri'):
            self.w('<a href="%s">'%attrs['refuri'])
        elif doc.explicit_targets.has_key(attrs['refname']):
            # an external reference has been defined
            ref = doc.explicit_targets[attrs['refname']]
            if ref.attributes.has_key('refuri'):
                self.w('<a href="%s">'%ref.attributes['refuri'])
            else:
                self.w('<a href="#%s">'%attrs['refname'])
        elif doc.implicit_targets.has_key(attrs['refname']):
            # internal reference
            name = attrs['refname']
            self.w('<a href="#%s">'%urllib.quote(name))
        else:
            ok = 0
            self.w('<span class="formatter_error">target "%s" '
                'undefined</span>'%attrs['refname'])
        if node.children: self.format(node)
        if ok:
            self.w('</a>')

    def format_footnote_reference(self, node):
        ''' (#PCDATA)
            %reference.atts;
            %auto.att;
        '''
        raise NotImplementedError, node

    def format_substitution_reference(self, node):
        ''' (%text.model;)
            %refname.att;
        '''
        #raise NotImplementedError, node
        pass

    def format_problematic(self, node):
        ''' (%text.model;)
        '''
        raise NotImplementedError, node

    def format_system_message(self, node):
        ''' just print it to stderr
        '''
        print >>sys.stderr, '%s: %s'%(node.attributes['type'], node[0][0].data)

    #
    # Finally, #text
    #
    def format__text(self, node):
        self.w(cgi.escape(node.data))

def main(filename, debug=0):
    parser = Parser()
    input = open(filename).read()
    document = dps.utils.newdocument()
    parser.parse(input, document)
    if debug == 1:
        print document.pformat()
    else:
        formatter = DumbPythonPointFormatter()
        print formatter.format_document(document)

if __name__ == '__main__':
    if len(sys.argv) > 2:
        main(sys.argv[1], debug=1)
    else:
        main(sys.argv[1])

