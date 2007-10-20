# Authors: Jay Kint
# Contact: bilbo@hobbit-hole.org
# Revision: $Revision$
# Date: $Date$
# Copyright: This module is copyright 2007 by Jay Kint, licensed under the BSD License.

"""
WordML writer for Docutils.  WordML is the XML format for MS Word documents.

The output conforms to basic WordML and can be read by MS Word to be output in any of the formats it supports
(i.e. PDF).

The way it works is that it uses a template file *(default template.xml)* and looks for the <w:rest /> node
for where to insert the generated WordML.  This allows you to save a preformatted template with the formatting
you want (e.g., styles, fonts, paper size).

Known bugs:

- Mixing bulleted lists and enumerated lists doesn't work quite the same way that MS Word does it.  It works,
  but the output isn't the same as if you'd typed it in Word.

- You can't mix paper types.  That stands to reason, but it's still a limitation.

- Titles and subtitles are only allowed up to 3 levels for now.

Of course, there's nothing to prevent you from loading up the generated file and
fixing these things yourself in MS Word.
  
"""

__docformat__ = 'reStructuredText'

import sys
import os
import docutils
from docutils import frontend, writers, nodes, utils, languages
import xml.parsers.expat
from xml.etree.ElementTree import TreeBuilder, dump, tostring
import copy
try:
    import Image
except ImportError:
    Image = None

class Writer(writers.Writer):

    supported = ('wordml',)
    """Formats this writer supports."""

    default_template = 'template.xml'

    default_template_path = utils.relative_path(
        os.path.join(os.getcwd(), 'dummy'),
        os.path.join(os.path.dirname(__file__), default_template))

    settings_spec = (
        '"Docutils WordML" Writer Options',
        None,
        (('Specify the template file (UTF-8 encoded).  Default is "%s".'
          % default_template_path,
          ['--template'],
          {'default': default_template_path, 'metavar': '<file>'}),))

    settings_defaults = {'output-encoding-error-handler': 'xmlcharrefreplace'}

    config_section = 'docutils_wordml writer'
    config_section_dependencies = ('writers',)

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = WordMLTranslator

    def translate(self):
        self.visitor = visitor = self.translator_class(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.astext()


class TextNoEncoding(nodes.Text):

    """A text node that is meant to take its contents literally and not
    have it encoded."""

    def __init__( self, data, rawsource=''):
        nodes.Text.__init__( self, data, rawsource )


class TextWithStyle( nodes.Text ):

    """A text node that has an attached style."""

    def __init__( self, data, style, rawsource='' ):
        nodes.Text.__init__( self, data, rawsource )
        self.style = style

# contain an xml tag within a region (with styles if needed)
class XMLRegion( nodes.General, nodes.Element ): pass


class WordMLTranslator(nodes.NodeVisitor):

    """
    This WordML writer handles most of the features for now, You can get the gist of it rather easily.  It's not nearly
    as complicated as the HTML writer, surprisingly.
    """

    title_styles = [ "Heading1", "Heading2", "Heading3" ]
    xml_spaces = '                                                                                '

    # these are meant to be default lists, one for bullet and one for enumerated.
    # other list types should be added here later.

    def __init__(self, document):
        nodes.NodeVisitor.__init__(self, document)
        self.document = document
        self.doc = ''
        self.body = []    # the body of the document
        self.in_docinfo = None
        self.in_sidebar = None
        self.settings = settings = document.settings
        self.section_level = 0
        self.title_level = 0
        self.text_properties = {}   # the list of properties for a run of test
        self.text_prefix = []       # text or commands inserted before next text run
        self.paragraph_properties = {}
        self.id = 1000
        self.in_footnote = 0
        self.no_text = 0            # flag to suppress any text output
        self.literal_text = 0       # flag to output newlines in text
        self.skip_encode = 0        # flag to skip the encoding of text to xml friendly output
        self.has_text_output = 0    # flag if any text output since last reset
        self.figure_count = 0
        self.in_figure = 0
        self.indentation = 0
        # list related variables
        self.lists = []
        self.list_level = -1
        self.list_count = 0
        self.list_properties = []
        self.list_defs = []
        self.template_list_defs = []
        self.current_listdef = []
        self.in_listdef = 0
        self.show_list_properties = 0
        self.extract_listdefs()
        self.substitutions = {}
        # table related variables
        self.in_table = 0
        self.colspecs = []
        self.row = 0
        self.col = 0
        self.spans = {}  # for multispan columns and rows
        self.total_col_width = 0
        # xml output variables
        self.doc_tree = TreeBuilder()
        self.doc_tree.start( 'w:document', {} )
        self.xml_spacing = 0


    def gen_id(self):
        self.id += 1
        return self.id

    
    def template_start_element( self, name, attr ):
        if name == 'w:rest':
            self.doc_tree.end( 'w:document' )
            tree = self.doc_tree.close()
            doc_string = tostring( tree )
            p = xml.parsers.expat.ParserCreate();
            p.StartElementHandler = self.body_start_element
            p.EndElementHandler = self.body_end_element
            p.CharacterDataHandler = self.body_data
            p.Parse( doc_string )
            # for b in self.body:
            #    self.doc += b
        elif name == 'w:rstlists':
            for l in self.lists:
                self.doc += l
        elif name == 'w:rstlistdefs':
            for l in self.list_defs:
                self.doc += l
        else:
            self.doc += "<" + name + " "
            for k, v in attr.iteritems():
                self.doc += k + '="' + v + '" '
            self.doc += ">"


    def template_end_element( self, name ):
        if name == 'w:rest' or name == 'w:rstlists' or name == 'w:rstlistdefs':
            pass
        else:
            self.doc += '</' + name + '>'


    def template_char_data( self, data ):
        self.doc += data


    # routines for extracting the listdef elements from the template
    def start_listdef_extract( self, name, attr ):
        if name == 'w:listDef':
            self.in_listdef = 1
            self.current_listdef = []
        if name == 'w:lsid':
            return
        if self.in_listdef == 1:
            self.current_listdef.append( [ name, attr ] )


    def end_listdef_extract( self, name ):
        self.current_listdef.append( [ "/" + name ] )
        if name == 'w:listDef':
            self.template_list_defs.append( self.current_listdef )
            self.in_listdef = 0
        if name == 'w:lsid':
            return

    def list_def_data( self, data ):
        pass
    

    def extract_listdefs( self ):
        p = xml.parsers.expat.ParserCreate();
        p.StartElementHandler = self.start_listdef_extract
        p.EndElementHandler = self.end_listdef_extract
        p.CharacterDataHandler = self.list_def_data
        template = file( self.document.settings.template )
        p.ParseFile( template )


    def listdef_to_xml( self, list_number, level, start, nfc ):
        """Modify a listdef to include an alternate numbering scheme and new starting number.
           Then convert it to XML and return the XML string."""
        xml = ''
        lvl = -1000
        for x in self.template_list_defs[ list_number ]:
            # change the listDefId to match the new list
            if x[0] == 'w:listDef':
                x[1]['w:listDefId'] = str( self.list_count + 1 )
            # get the level if it has changed
            if x[0] == 'w:lvl':
                lvl = int( x[1]['w:ilvl'] )
            # skip an existing nfc node
            if ( x[0] == 'w:nfc' or x[0] == '/w:nfc' ) and level == lvl:
                continue
            xml += '<' + x[0] + ' '
            if len( x ) == 2:
                for k, v in x[1].iteritems():
                    if x[0] == 'w:start' and k == 'w:val' and lvl == level:
                        xml += k + '="' + str( start ) + '" '
                    else:
                        xml += k + '="' + v + '" '
            xml += '>\n'
            # add in our nfc node right after the start node
            if x[0] == '/w:start' and level == lvl:
                xml += '<w:nfc w:val="' + str( nfc ) + '" />\n'
        return xml


    def body_start_element( self, name, attr ):
        if name == 'w:document':
            return
        element = self.xml_spaces[ 0 : self.xml_spacing ] + "<" + name + " "
        for k, v in attr.iteritems():
            element += k + '="' + v + '" '
        element = element[:-1]
        element += ">"
        if name != 'w:t':
            element += '\n'
        self.xml_spacing += 2
        self.doc += element


    def body_end_element( self, name ):
        if name == 'w:document':
            return
        self.xml_spacing -= 2
        element = ''
        if name != 'w:t':
            element += self.xml_spaces[ 0 : self.xml_spacing ]
        element += '</' + name + '>\n'
        self.doc += element

    def body_data( self, data ):
        self.doc += data


    def check_for_span( self, col, row ):
        check_span = '{' + str( col ) + ',' + str( row ) + '}'
        if self.spans.has_key( check_span ):
            self.doc_tree.start( 'w:tc', {} )
            self.doc_tree.start( 'w:tcPr', {} )
            self.doc_tree.start( 'w:tcW', { 'w' : str( self.calc_col_pct( self.col )), 'w:type' : 'pct' })
            self.doc_tree.start( self.spans[ check_span ][0], self.spans[ check_span ][1] )
            self.doc_tree.end( self.spans[ check_span ][0] )
            self.doc_tree.end( 'w:tcW' )
            self.doc_tree.end( 'w:tcPr' )
            self.doc_tree.start( 'w:p', {} )
            self.doc_tree.end( 'w:p' )
            self.doc_tree.end( 'w:tc' )
            self.body.append( '<w:tc>\n  <w:tcPr>\n    <w:tcW w="' + str( self.calc_col_pct( col )) + '" w:type="pct" />\n    <' + self.spans[ check_span ][0] + ' />\n  </w:tcPr>\n  <w:p />\n</w:tc>\n' )
            return True
        return False


    def astext( self ):
        p = xml.parsers.expat.ParserCreate();
        p.StartElementHandler = self.template_start_element
        p.EndElementHandler = self.template_end_element
        p.CharacterDataHandler = self.template_char_data
        template = file( self.document.settings.template )
        p.ParseFile( template )
        return self.doc
    

    def encode(self, text):
        """Encode special characters in `text` & return."""
        text = text.replace("&", "&amp;")
        text = text.replace("<", "&lt;")
        text = text.replace('"', "&quot;")
        text = text.replace(">", "&gt;")
        if self.literal_text == 1:
            text = text.replace( "\n", "</w:t><w:br /><w:t>" )
        else:
            text = text.replace("\n", " " )
        return text


    def visit_Text(self, node):
        # if we have turned off text input, then just return
        if self.no_text:
            return
        # skip encode allows us to inject custom wordml as a text node without
        self.doc_tree.start( 'w:r', {} )
        self.body.append( "<w:r>" )
        if len( self.text_properties ) > 0:
            self.doc_tree.start( 'w:rPr', {} )
            self.body.append( "<w:rPr>\n" )
            for v in self.text_properties.values():
                if type( v ) == type( () ):
                    element = '<' + v[0] + ' '
                    for k,a in v[1].iteritems():
                        element += k + '="' + a + '" '
                    element += '/>'
                    self.doc_tree.start( v[0], v[1] )
                    self.doc_tree.end( v[0] )
                    self.body.append( element )
                else:
                    self.body.append( v )
            self.doc_tree.end( 'w:rPr' )
            self.body.append( "</w:rPr>\n" )
        self.doc_tree.start( 'w:t', {} )
        self.body.append( "<w:t>" )
        text = node.astext()
        encoded = self.encode(text)
        self.doc_tree.data( encoded )
        self.body.append(encoded)
        self.has_text_output = 1


    def depart_Text(self, node):
        # if we have turned off text input, then just return
        if self.no_text:
            return
        self.doc_tree.end( 'w:t' )
        self.doc_tree.end( 'w:r' )
        self.body.append( "</w:t></w:r>\n" )

 
    def visit_TextNoEncoding( self, node ):
        if self.no_text:
            return
        self.doc_tree.data( node.astext() )
        self.body.append( node.astext() )


    def depart_TextNoEncoding( self, node ):
        pass


    def visit_TextWithStyle( self, node ):
        self.text_properties[ node.style[0] ] = node.style[1]
        self.visit_Text( node )


    def depart_TextWithStyle( self, node ):
        del self.text_properties[ node.style[0] ]
        self.depart_Text( node )


    def visit_abbreviation(self, node):
        pass


    def depart_abbreviation(self, node):
        pass


    def visit_acronym(self, node):
        pass


    def depart_acronym(self, node):
        pass


    def visit_address(self, node):
        pass


    def depart_address(self, node):
        pass


    def visit_admonition(self, node, name=''):
        pass


    def depart_admonition(self, node=None):
        pass


    def visit_attention(self, node):
        pass


    def depart_attention(self, node):
        pass


    def visit_attribution(self, node):
        pass


    def depart_attribution(self, node):
        pass


    def visit_author(self, node):
        self.paragraph_properties[ 'author' ] = ('w:pStyle', { 'w:val' : 'AuthorName' })
        # self.paragraph_properties[ 'author' ] = '<w:pStyle w:val="AuthorName" />'
        self.visit_paragraph( node )


    def depart_author(self, node):
        del self.paragraph_properties[ 'author' ]
        self.depart_paragraph( node )


    def visit_authors(self, node):
        pass


    def depart_authors(self, node):
        pass


    def visit_block_quote(self, node):
        self.indentation += 720


    def depart_block_quote(self, node):
        self.indentation -= 720


    def visit_bullet_list(self, node):
        self.list_level += 1
        self.list_count += 1
        self.lists.append( '<w:list w:ilfo="' + str( self.list_count ) + '">\n  <w:ilst w:val="1">\n  </w:ilst>\n</w:list>\n' )
        self.list_properties.append( '<w:listPr>\n<w:ilvl w:val="' + str( self.list_level ) + '" />\n<w:ilfo w:val="' + str( self.list_count ) + '" />\n</w:listPr>\n' )


    def depart_bullet_list(self, node):
        self.list_properties.pop()
        self.list_level -= 1


    def visit_caption(self, node):
        self.figure_count += 1
        self.text_properties[ 'caption' ] = ('w:rStyle', { 'w:val' : 'Caption' })
        # self.text_properties[ 'caption' ] = '<w:rStyle w:val="Caption" />'
        node.children.insert( 0, nodes.Text( 'Figure ' + str( self.figure_count ) + ' ' ));


    def depart_caption(self, node):
        del self.text_properties[ 'caption' ]


    def visit_caution(self, node):
        pass


    def depart_caution(self, node):
        pass


    def visit_citation(self, node):
        if not self.in_footnote:
            self.no_text += 1


    def depart_citation(self, node):
        if not self.in_footnote:
            self.no_text -= 1


    def visit_citation_reference(self, node):
        citation = self.document.ids[ node['refid'] ]
        citation_reference_text = ''
        if not isinstance( citation, nodes.citation ):
            raise TypeError( 'not a citation node mapped to id' )
        self.doc_tree.start( 'w:r', {} )
        self.doc_tree.start( 'w:rPr', {} )
        self.doc_tree.start( 'w:rStyle', { 'w:val' : 'CitationReference' })
        self.doc_tree.end( 'w:rStyle' )
        self.doc_tree.end( 'w:rPr' )
        self.doc_tree.start( 'w:endnote', { 'w:suppressRef' : 'on' } )
        self.body.append( '<w:r>\n<w:rPr>\n<w:rStyle w:val="CitationReference"/>\n</w:rPr>\n<w:endnote w:suppressRef="on">\n' )
        self.in_footnote = 1
        former_paragraph_properties = self.paragraph_properties.copy()
        self.paragraph_properties = {}
        self.paragraph_properties[ 'citation' ] = ( 'w:pStyle', { 'w:val' : 'EndnoteText' })
        # self.paragraph_properties[ 'citation' ] = '<w:pStyle w:val="EndnoteText"/>'
        labels = citation.traverse( condition=nodes.label )
        for n in labels:
            citation_reference_text += n.astext()
            citation.children.remove( n )
        p = citation.traverse( condition=nodes.paragraph )
        # t_head = TextNoEncoding( '<w:r>\n<w:rPr>\n<w:rStyle w:val="CitationReference" />\n</w:rPr>\n<w:t>' )
        # t_tail = TextNoEncoding( '</w:t>\n</w:r>\n')
        # p[0].children.insert( 0, t_tail )
        # p[0].children.insert( 0, TextNoEncoding( '[' + citation_reference_text + '] ' ))
        # p[0].children.insert( 0, nodes.Text( '[' + citation_reference_text + '] ' ))
        t = TextWithStyle( '[' + citation_reference_text + '] ', ( 'citation', ( 'w:rStyle', { 'w:val' : 'CitationReference' })))
        p[0].children.insert( 0, t )
        # p[0].children.insert( 0, t_head )
        citation.walkabout( self )
        p[0].children.remove( t )
        self.doc_tree.end( 'w:endnote' )
        self.doc_tree.start( 'w:t', {} )
        self.doc_tree.data( '[' + citation_reference_text + ']' )
        self.doc_tree.end( 'w:t' )
        self.doc_tree.end( 'w:r' )
        self.body.append( '</w:endnote>\n' )
        self.body.append( '<w:t>' )
        self.body.append( '[' + citation_reference_text + ']' )
        self.body.append( '</w:t>\n</w:r>\n' )
        del self.paragraph_properties[ 'citation' ]
        self.in_footnote = 0
        self.no_text += 1
        self.paragraph_properties = former_paragraph_properties


    def depart_citation_reference(self, node):
        self.no_text -= 1
        pass


    def visit_classifier(self, node):
        pass


    def depart_classifier(self, node):
        pass


    def visit_colspec(self, node):
        self.colspecs.append( node )
        self.total_col_width += node['colwidth']


    def depart_colspec(self, node):
        pass


    def visit_comment(self, node):
        self.no_text += 1


    def depart_comment( self, node ):
        self.no_text -= 1


    def visit_compound(self, node):
        pass


    def depart_compound(self, node):
        pass


    def visit_contact(self, node):
        self.paragraph_properties[ 'contact' ] = ( 'w:pStyle', { 'w:val' : 'AuthorContact' })
        # self.paragraph_properties[ 'contact' ] = '<w:pStyle w:val="AuthorContact" />'
        self.visit_paragraph( node )


    def depart_contact(self, node):
        del self.paragraph_properties[ 'contact' ]
        self.depart_paragraph( node )


    def visit_container(self, node):
        pass


    def depart_container(self, node):
        pass


    def visit_copyright(self, node):
        self.paragraph_properties[ 'copyright' ] = ( 'w:pStyle', { 'w:val' : 'BibliographMatter' })
        # self.paragraph_properties[ 'copyright' ] = '<w:pStyle w:val="BibliographMatter" />'
        self.visit_paragraph( node )


    def depart_copyright(self, node):
        del self.paragraph_properties[ 'copyright' ]
        self.depart_paragraph( node )


    def visit_danger(self, node):
        pass


    def depart_danger(self, node):
        pass


    def visit_date(self, node):
        self.paragraph_properties[ 'date' ] = ( 'w:pStyle', { 'w:val' : 'BibliographMatter' })
        # self.paragraph_properties[ 'date' ] = '<w:pStyle w:val="BibliographMatter" />'
        self.visit_paragraph( node )


    def depart_date(self, node):
        del self.paragraph_properties[ 'date' ]
        self.depart_paragraph( node )


    def visit_decoration(self, node):
        pass


    def depart_decoration(self, node):
        pass


    def visit_definition(self, node):
        self.indentation += 720
        self.paragraph_properties[ 'definition' ] = ( 'w:pStyle', { 'w:val' : 'Definition' })
        # self.paragraph_properties[ 'definition' ] = '<w:pStyle w:val="Definition" />'


    def depart_definition(self, node):
        self.indentation -= 720
        del self.paragraph_properties[ 'definition' ]


    def visit_definition_list(self, node):
        pass
    

    def depart_definition_list(self, node):
        pass


    def visit_definition_list_item(self, node):
        pass


    def depart_definition_list_item(self, node):
        pass


    def visit_description(self, node):
        pass


    def depart_description(self, node):
        pass


    def visit_docinfo(self, node):
        pass


    def depart_docinfo(self, node):
        pass


    def visit_docinfo_item(self, node, name, meta=1):
        pass


    def depart_docinfo_item(self):
        pass


    def visit_doctest_block(self, node):
        pass


    def depart_doctest_block(self, node):
        pass


    def visit_document(self, node):
        pass


    def depart_document(self, node):
        pass


    def visit_emphasis(self, node):
        self.text_properties[ '*' ] = ('w:i', {})
        # self.text_properties[ '*' ] = '<w:i/>'


    def depart_emphasis(self, node):
        del self.text_properties[ '*' ]


    def calc_col_pct( self, col ):
        width = int( self.colspecs[ col ][ 'colwidth' ] * 100.0 / self.total_col_width + 0.5 )
        return width


    def visit_entry(self, node):
        width = self.calc_col_pct( self.col )
        self.doc_tree.start( 'w:tc', {} )
        self.doc_tree.start( 'w:tcPr', {} )
        self.doc_tree.start( 'w:tcW', { 'w' : str( width ), 'w:type' : 'pct' } )
        self.doc_tree.end( 'w:tcW' )
        self.body.append( '<w:tc>\n  <w:tcPr>\n    <w:tcW w="' + str( width ) + '" w:type="pct" />\n' )
        self.has_text_output = 0
        if node.has_key('morecols') and node.has_key('morerows'):
            raise NotImplementedError( "Table cell " + str( self.col ) + "," + str( self.row ) + " can't have both merged rows and columns." )
        if node.has_key('morecols'):
            self.doc_tree.start( 'w:hmerge', { 'w:val' : 'restart' } )
            self.doc_tree.end( 'w:hmerge' )
            self.body.append( '<w:hmerge w:val="restart" />' )
            for i in range( node['morecols'] ):
                span = '{' + str( self.col + i + 1 ) + ',' + str( self.row ) + '}'
                self.spans[ span ] = ('w:hmerge', {}) # '<w:hmerge />'
        if node.has_key('morerows'):
            self.doc_tree.start( 'w:vmerge', { 'w:val' : 'restart' })
            self.doc_tree.end( 'w:vmerge' )
            self.body.append( '<w:vmerge w:val="restart" />' )
            for i in range( node['morerows'] ):
                span = '{' + str( self.col ) + ',' + str( self.row + i + 1 ) + '}'
                self.spans[ span ] = ('w:vmerge', {} ) # '<w:vmerge />'
        self.doc_tree.end( 'w:tcPr' )
        self.body.append( '</w:tcPr>\n' )


    def depart_entry(self, node):
        if self.has_text_output == 0:
            self.doc_tree.start( 'w:p', {} )
            self.doc_tree.end( 'w:p' )
            self.body.append( '  <w:p />\n' )
        self.doc_tree.end( 'w:tc' )
        self.body.append( '</w:tc>\n' )
        # if there are any cells that are part of a span, then include them here as empty cells.
        col = self.col + 1
        row = self.row
        while self.check_for_span( col, row ):
            col = col + 1
        self.col = col
        self.row = row


    def visit_enumerated_list(self, node):
        # to put any sort of customization in, it's necessary to add an entirely new listDef element.
        # so, I need to load the listDefs at the beginning of the run, and then customize them as needed.
        self.list_level += 1
        self.list_count += 1
        list_props = '<w:listPr>\n<w:ilvl w:val="' + str( self.list_level ) + '" />\n<w:ilfo w:val="' + str( self.list_count ) + '" />\n'
        #if the list has an explicit start, then set the text for it
        start = 1
        if node.has_key('start'):
            start = int( node['start'] )
        nfc = 0
        if node['enumtype'] == 'arabic':
            nfc = 0
        elif node['enumtype'] == 'upperalpha':
            nfc = 3
        elif node['enumtype'] == 'loweralpha':
            nfc = 4
        elif node['enumtype'] == 'lowerroman':
            nfc = 2
        elif node['enumtype'] == 'upperroman':
            nfc = 1
        self.list_defs.append( self.listdef_to_xml( 0, self.list_level, start, nfc ))
        self.lists.append( '<w:list w:ilfo="' + str( self.list_count ) + '">\n    <w:ilst w:val="' + str( self.list_count + 1 ) + '">\n  </w:ilst>\n</w:list>\n' )
        list_props += '</w:listPr>\n'
        self.list_properties.append( list_props )


    def depart_enumerated_list(self, node):
        self.show_list_properties -= 1
        self.list_properties.pop()
        self.list_level -= 1


    def visit_error(self, node):
        pass


    def depart_error(self, node):
        pass


    def visit_field(self, node):
        self.paragraph_properties[ 'field' ] = ( 'w:pStyle', { 'w:val' : 'BibliographMatter' })
        # self.paragraph_properties[ 'field' ] = '<w:pStyle w:val="BibliographMatter" />'
        self.visit_paragraph( node )


    def depart_field(self, node):
        del self.paragraph_properties[ 'field' ]
        self.depart_paragraph( node )


    def visit_field_body(self, node):
        pass


    def depart_field_body(self, node):
        pass


    def visit_field_list(self, node):
        pass


    def depart_field_list(self, node):
        pass


    def visit_field_name(self, node):
        pass


    def depart_field_name(self, node):
        pass


    def visit_figure(self, node):
        self.in_figure = 1


    def depart_figure(self, node):
        if self.in_figure:
            self.doc_tree.end( 'w:p' )
            self.body.append( '</w:p>\n' )
        self.in_figure = 0
        

    def visit_footer(self, node):
        pass


    def depart_footer(self, node):
        pass


    def visit_footnote(self, node):
        if not self.in_footnote:
            self.no_text += 1

    
    def depart_footnote(self, node):
        if not self.in_footnote:
            self.no_text -= 1


    def visit_footnote_reference(self, node):
        if not node.has_key( 'auto' ):
            raise TypeError( 'footnotes required to be auto numbered' )
        footnote = self.document.ids[ node['refid'] ]
        if not isinstance( footnote, nodes.footnote ):
            raise TypeError( 'not a footnote node mapped to id' )
        self.doc_tree.start( 'w:r', {} )
        self.doc_tree.start( 'w:rPr', {} )
        self.doc_tree.start( 'w:rStyle', { 'w:val' : 'EndnoteReference' })
        self.doc_tree.end( 'w:rStyle' )
        self.doc_tree.end( 'w:rPr' )
        self.doc_tree.start( 'w:endnote', {} )
        self.body.append( '<w:r>\n<w:rPr>\n<w:rStyle w:val="EndnoteReference"/>\n</w:rPr>\n<w:endnote>\n' )
        # figure out how to get the <w:endnoteRef/>
        self.in_footnote = 1
        former_paragraph_properties = self.paragraph_properties.copy()
        self.paragraph_properties = {}
        self.paragraph_properties[ 'footnote' ] = ( 'w:pStyle', { 'w:val' : 'EndnoteText' })
        # self.paragraph_properties[ 'footnote' ] = '<w:pStyle w:val="EndnoteText"/>'
        # self.body.append( '<w:p>\n<w:pPr>\n<w:pStyle w:val="EndnoteText"/>\n</w:pPr>\n<w:r>\n<w:r>\n<w:rPr>\n<w:rStyle w:val="EndnoteReference"/>\n</w:rPr>\n<w:endnoteRef/>\n' )
        # Find the label in the target footnode node and add it here.
        labels = footnote.traverse( condition=nodes.label )
        # replace label text with <w:endnoteRef />
        for n in labels:
            footnote.children.remove( n )
        #    n.children.append( nodes.Text('<w:r>\n<w:rPr>\n<w:rStyle w:val="EndnoteReference" />\n</w:rPr>\n<w:endnoteRef />\n</w:r>\n'))
        p = footnote.traverse( condition=nodes.paragraph )
        # t = TextNoEncoding( '<w:r>\n<w:rPr>\n<w:rStyle w:val="EndnoteReference" />\n</w:rPr>\n<w:endnoteRef />\n</w:r>\n')
        t = XMLRegion( xml=[( 'w:endnoteRef', {} )], styles=[( 'w:rStyle', { 'w:val' : 'EndnoteReference' })] )
        p[0].children.insert( 0, t )
        footnote.walkabout( self )
        p[0].children.remove(t)
        self.doc_tree.end( 'w:endnote' )
        self.doc_tree.end( 'w:r' )
        self.body.append( '</w:endnote>\n</w:r>\n' )
        del self.paragraph_properties[ 'footnote' ]
        self.in_footnote = 0
        self.no_text += 1
        self.paragraph_properties = former_paragraph_properties


    def depart_footnote_reference(self, node):
        # del self.paragraph_properties[ 'footnote' ]
        # self.in_footnote = 0
        self.no_text -= 1


    def visit_generated(self, node):
        pass


    def depart_generated(self, node):
        pass


    def visit_header(self, node):
        pass


    def depart_header(self, node):
        pass


    def visit_hint(self, node):
        pass


    def depart_hint(self, node):
        pass


    def visit_image(self, node):
        width = 100
        height = 100
        align = 'center'
        use_width = 0
        use_height = 0
        if Image:
            try:
                im = Image.open( node['uri'] )
                width = im.size[0]
                height = im.size[1]
                use_width = 1
                use_height = 1
            except (IOError, UnicodeError):
                pass
        if node.has_key( 'width' ):
            width = node[ 'width' ]
            use_width = 1
        if node.has_key( 'height' ):
            height = node[ 'height' ]
            use_height = 1
        if node.has_key( 'align' ):
            align = node[ 'align' ]
        self.doc_tree.start( 'w:p', {} )
        self.doc_tree.start( 'w:pPr', {} )
        self.doc_tree.start( 'w:jc', { 'w:val' : str( align ) } )
        self.doc_tree.end( 'w:jc' )
        self.doc_tree.end( 'w:pPr' )
        self.doc_tree.start( 'w:pict', {} )
        style = 'position:absolute;left:0;text-align:left;margin-left:0;margin-top:0;width:'
        if use_width:
            style += str(width)  + 'px'
        else:
            style += 'auto'
        style += ';height:'
        if use_height:
            style += str( height ) + 'px'
        else:
            style += 'auto'
        style += ';z-index:1;mso-position-horizontal:center'
        self.doc_tree.start( 'v:shape', { 'id' : str( self.gen_id() ), 'style' : style, 'coordsize' : '', 'o:spt' : '100', 'adj' : '0,,0', 'path' : '', 'stroked' : 'f' } )
        self.doc_tree.start( 'v:imagedata', { 'src' : node['uri'] } )
        self.doc_tree.end( 'v:imagedata' )
        self.doc_tree.start( 'w10:wrap', { 'type' : 'square' } )
        self.doc_tree.end( 'w10:wrap' )
        self.doc_tree.end( 'v:shape' )
        self.doc_tree.end( 'w:pict' )
        self.body.append( '<w:p>\n<w:pPr>\n<w:jc w:val="' + str( align ) + '" />\n</w:pPr>\n<w:pict>\n<v:shape id="' + str( self.gen_id() ) + '" ')
        self.body.append( 'style="position:absolute;left:0;text-align:left;margin-left:0;margin-top:0;width:' )
        if use_width:
            self.body.append(  str(width) + 'px' )
        else:
            self.body.append( 'auto' )
        self.body.append(';height:')
        if use_height:
            self.body.append( str(height) + 'px')
        else:
            self.body.append( 'auto' )
        self.body.append( ';z-index:1;mso-position-horizontal:center" coordsize="" o:spt="100" adj="0,,0" path="" stroked="f" >\n' )
        self.body.append( '<v:imagedata src="' + node['uri'] + '"/>\n<w10:wrap type="square"/>\n</v:shape>\n</w:pict>\n' )


    def depart_image(self, node):
        if not self.in_figure:
            self.doc_tree.end( 'w:p' )
            self.body.append( '</w:p>\n' )


    def visit_important(self, node):
        pass


    def depart_important(self, node):
        pass


    def visit_inline(self, node):
        pass


    def depart_inline(self, node):
        pass


    def visit_label(self, node):
        self.text_properties[ 'label' ] = ( 'w:rStyle', { 'w:val' : '"EndnoteReference"' } )
        # self.text_properties[ 'label' ] = '<w:rStyle w:val="EndnoteReference"/>\n'
        #if self.in_footnote:
        #   self.no_text += 1
        #    self.body.append( '<w:r>\n<w:rPr>\n<w:rStyle w:val="EndnoteReference"/>\n</w:rPr>\n<w:endnoteRef/>\n</w:r>\n' )
        pass

    def depart_label(self, node):
        del self.text_properties[ 'label' ]
        #if self.in_footnote:
        #    self.no_text -= 1
        pass

    def visit_legend(self, node):
        pass


    def depart_legend(self, node):
        pass


    def visit_line(self, node):
        pass


    def depart_line(self, node):
        pass


    def visit_line_block(self, node):
        pass


    def depart_line_block(self, node):
        pass


    def visit_list_item(self, node):
        pass


    def depart_list_item(self, node):
        pass


    def visit_literal(self, node):
        self.text_properties[ 'literal' ] = ( 'w:rStyle', { 'w:val' : 'Literal' } )
        # self.text_properties[ 'literal' ] = '<w:rStyle w:val="Literal"/>\n'


    def depart_literal( self, node ):
        del self.text_properties[ 'literal' ]


    def visit_literal_block(self, node):
        self.paragraph_properties[ 'literal' ] = ( 'w:pStyle', { 'w:val' : 'LiteralBlock' })
        #~ self.paragraph_properties[ 'literal' ] = '<w:pStyle w:val="LiteralBlock" />\n'
        self.visit_paragraph( node )
        self.literal_text = 1


    def depart_literal_block(self, node):
        del self.paragraph_properties[ 'literal' ]
        self.depart_paragraph( node )
        self.literal_text = 0


    def visit_meta(self, node):
        pass


    def depart_meta(self, node):
        pass


    def visit_note(self, node):
        pass


    def depart_note(self, node):
        pass


    def visit_option(self, node):
        pass


    def depart_option(self, node):
        pass


    def visit_option_argument(self, node):
        pass


    def depart_option_argument(self, node):
        pass


    def visit_option_group(self, node):
        pass


    def depart_option_group(self, node):
        pass


    def visit_option_list(self, node):
        pass


    def depart_option_list(self, node):
        pass


    def visit_option_list_item(self, node):
        pass


    def depart_option_list_item(self, node):
        pass


    def visit_option_string(self, node):
        pass


    def depart_option_string(self, node):
        pass


    def visit_organization(self, node):
        self.paragraph_properties[ 'organization' ] = ( 'w:pStyle', { 'w:val' : 'BibliographMatter' } )
        #~ self.paragraph_properties[ 'organization' ] = '<w:pStyle w:val="BibliographMatter" />'
        self.visit_paragraph( node )


    def depart_organization(self, node):
        del self.paragraph_properties[ 'organization' ]
        self.depart_paragraph( node )


    def visit_paragraph(self, node):
        self.doc_tree.start( 'w:p', {} )
        self.body.append( "<w:p>\n" )
        if len( self.paragraph_properties ) > 0 or len( self.list_properties ) > 0 or (self.indentation > 0 and not self.in_footnote):
            self.doc_tree.start( 'w:pPr', {} )
            self.body.append( "<w:pPr>\n" )
            if self.indentation > 0 and not self.in_footnote:
                self.doc_tree.start( 'w:ind', { 'w:left' : str( self.indentation ), 'w:right' : str( self.indentation ) })
                self.doc_tree.end( 'w:ind' )
                self.body.append( '<w:ind w:left="' + str( self.indentation ) +
                                  '" w:right="' + str( self.indentation ) + '" />\n' )
            for v in self.paragraph_properties.values():
                if type( v ) == type( () ):
                    element = '<' + v[0] + ' '
                    for k,a in v[1].iteritems():
                        element += k + '="' + a + '" '
                    element += '/>'
                    self.doc_tree.start( v[0], v[1] )
                    self.doc_tree.end( v[0] )
                    self.body.append( element )
                else:
                    self.body.append( v )
            if len( self.list_properties ) > 0 and isinstance( node.parent, nodes.list_item ):
                if type( self.list_properties[ -1 ] ) == type( () ):
                    t = self.list_properties[ -1 ]
                    element = '<' + t[0] + ' '
                    for k,a in t[1].iteritems():
                        element += k + '="' + a + '" '
                    element += '/>'
                    self.doc_tree.start( t[0], t[1] )
                    self.doc_tree.end( t[0] )
                    self.body.append( element )
                else:
                    self.body.append( self.list_properties[ -1 ] )
            self.doc_tree.end( 'w:pPr' )
            self.body.append( "\n</w:pPr>\n" )


    def depart_paragraph(self, node):
        self.doc_tree.end( 'w:p' );
        self.body.append( "</w:p>\n" )


    def visit_problematic(self, node):
        pass


    def depart_problematic(self, node):
        pass


    def visit_raw(self, node):
        pass


    def visit_reference(self, node):
        if node.has_key('refid'):
            self.doc_tree.start( 'w:hlink', { 'w:bookmark' : node[ 'refid' ] })
            self.body.append( '<w:hlink w:bookmark="' + node['refid'] + '" >\n' )
        if node.has_key('refuri'):
            self.doc_tree.start( 'w:hlink', { 'w:dest' : node['refuri'] })
            self.body.append( '<w:hlink w:dest="' + node['refuri'] + '" >\n' )
        if not node.has_key('refuri') and not node.has_key('refid'):
            raise NotImplementedError('Unknown reference type')
        self.text_properties[ 'ref' ] = ( 'w:rStyle', { 'w:val' : 'Hyperlink' })
        #~ self.text_properties['ref'] = '<w:rStyle w:val="Hyperlink" />\n'


    def depart_reference(self, node):
        del self.text_properties[ 'ref' ]
        self.doc_tree.end( 'w:hlink' )
        self.body.append( '</w:hlink>\n' )


    def visit_revision(self, node):
        pass


    def depart_revision(self, node):
        pass


    def visit_row(self, node):
        self.doc_tree.start( 'w:tr', {} )
        self.body.append( '<w:tr>\n' )
        while self.check_for_span( self.col, self.row ):
            self.col += 1

    def depart_row(self, node):
        self.row += 1
        self.col = 0
        self.doc_tree.end( 'w:tr' )
        self.body.append( '</w:tr>\n' )


    def visit_rubric(self, node):
        pass


    def depart_rubric(self, node):
        pass


    def visit_section(self, node):
        self.section_level += 1
        if self.section_level > 3:
            raise NotImplementedError( "Only 3 levels of headings supported." )
        if node.has_key( 'ids' ):
            for id in node['ids']:
                refid = self.gen_id()
                self.doc_tree.start( 'aml:annotation', { 'aml:id' : str( refid ), 'w:type' : 'Word.Bookmark.Start', 'w:name' : id })
                self.doc_tree.end( 'aml:annotation' )
                self.doc_tree.start( 'aml:annotation', { 'aml:id' : str( refid ), 'w:type' : 'Word.Bookmark.End', 'w:name' : id })
                self.doc_tree.end( 'aml:annotation' )
                self.body.append('<aml:annotation aml:id="' + str( refid ) + '" w:type="Word.Bookmark.Start" w:name="' + id   + '" />' )
                self.body.append('<aml:annotation aml:id="' + str( refid ) + '" w:type="Word.Bookmark.End" w:name="' + id + '" />' )


    def depart_section(self, node):
        self.section_level -= 1


    def visit_sidebar(self, node):
        pass


    def depart_sidebar(self, node):
        pass


    def visit_status(self, node):
        self.paragraph_properties[ 'status' ] = ( 'w:pStyle', { 'w:val' : 'BibliographMatter' })
        #~ self.paragraph_properties[ 'status' ] = '<w:pStyle w:val="BibliographMatter" />'
        self.visit_paragraph( node )


    def depart_status(self, node):
        del self.paragraph_properties[ 'status' ]
        self.depart_paragraph( node )


    def visit_strong(self, node):
        self.text_properties[ '**' ] = ( 'w:b', {} )
        #~ self.text_properties[ '**' ] = '<w:b/>'


    def depart_strong(self, node):
        del self.text_properties[ '**' ]


    def visit_subscript(self, node):
        self.text_properties[ 'subscript' ] = ( 'w:vertAlign', { 'w:val' : 'subscript' })
        #~ self.text_properties[ 'subscript' ] = '<w:vertAlign w:val="subscript" />'


    def depart_subscript(self, node):
        del self.text_properties[ 'subscript' ]


    def visit_substitution_definition(self, node):
        raise nodes.SkipNode

    
    def visit_substitution_reference(self, node):
        raise NotImplementedError( "substitution references not implemented" )

    
    def visit_subtitle(self, node):
        self.paragraph_properties[ 'subtitle' ] = ( 'w:pStyle', { 'w:val' : self.title_styles[ self.section_level + 1 ] })
        #~ self.paragraph_properties[ 'subtitle' ] = '<w:pStyle w:val="' + self.title_styles[ self.section_level + 1 ] + '"/>\n'
        self.visit_paragraph( node )


    def depart_subtitle(self, node):
        del self.paragraph_properties[ 'subtitle' ]
        self.depart_paragraph( node )


    def visit_superscript(self, node):
        self.text_properties[ 'superscript' ] = ( 'w:vertAlign', { 'w:val' : 'superscript' })
        #~ self.text_properties[ 'superscript' ] = '<w:vertAlign w:val="superscript" />\n'


    def depart_superscript(self, node):
        del self.text_properties[ 'superscript' ]


    def visit_system_message(self, node):
        pass


    def depart_system_message(self, node):
        pass


    def visit_table(self, node):
        #include for now the default border around the table with a top vertical alignment
        self.in_table = 1
        self.colspecs = []
        self.spans = {}
        self.total_col_width = 0
        self.row = 0
        self.col = 0
        self.doc_tree.start( 'w:tbl', {} )
        self.doc_tree.start( 'w:tblPr', {} )
        self.doc_tree.start( 'w:tblStyle', { 'w:val' : 'Normal' } )
        self.doc_tree.end( 'w:tblStyle' )
        self.doc_tree.start( 'w:tblW', { 'w:w' : '5000', 'w:type' : 'pct' })
        self.doc_tree.end( 'w:tblW' )
        self.doc_tree.start( 'w:tblBorders', {} )
        self.doc_tree.start( 'w:top', { 'w:val' : 'single', 'w:sz' : '4', 'wx:bdrwidth' : '10', 'w:space' : '0', 'w:color' : 'auto' })
        self.doc_tree.end( 'w:top' )
        self.doc_tree.start( 'w:left', { 'w:val' : 'single', 'w:sz' : '4', 'wx:bdrwidth' : '10', 'w:space' : '0', 'w:color' : 'auto' })
        self.doc_tree.end( 'w:left' )
        self.doc_tree.start( 'w:bottom', { 'w:val' : 'single', 'w:sz' : '4', 'wx:bdrwidth' : '10', 'w:space' : '0', 'w:color' : 'auto' })
        self.doc_tree.end( 'w:bottom' )
        self.doc_tree.start( 'w:right', { 'w:val' : 'single', 'w:sz' : '4', 'wx:bdrwidth' : '10', 'w:space' : '0', 'w:color' : 'auto' })
        self.doc_tree.end( 'w:right' )
        self.doc_tree.start( 'w:insideH', { 'w:val' : 'single', 'w:sz' : '6', 'wx:bdrwidth' : '15', 'w:space' : '0', 'w:color' : 'auto' })
        self.doc_tree.end( 'w:insideH' )
        self.doc_tree.start( 'w:insideV', { 'w:val' : 'single', 'w:sz' : '6', 'wx:bdrwidth' : '15', 'w:space' : '0', 'w:color' : 'auto' })
        self.doc_tree.end( 'w:insideV' )
        self.doc_tree.end( 'w:tblBorders' )
        self.doc_tree.start( 'w:tblLook', { 'w:val' : '000001e0' } )
        self.doc_tree.end( 'w:tblLook' )
        self.doc_tree.end( 'w:tblPr' )
        self.body.append( '<w:tbl>\n'
                          '  <w:tblPr>\n'
                          '    <w:tblStyle w:val="Normal" />\n'
                          '    <w:tblW w:w="5000" w:type="pct" />\n'
                          '    <w:tblBorders>\n'
                          '    <w:top w:val="single" w:sz="4" wx:bdrwidth="10" w:space="0" w:color="auto"/>\n'
                          '    <w:left w:val="single" w:sz="4" wx:bdrwidth="10" w:space="0" w:color="auto"/>\n'
                          '      <w:bottom w:val="single" w:sz="4" wx:bdrwidth="10" w:space="0" w:color="auto"/>\n'
                          '      <w:right w:val="single" w:sz="4" wx:bdrwidth="10" w:space="0" w:color="auto"/>\n'
                          '      <w:insideH w:val="single" w:sz="6" wx:bdrwidth="15" w:space="0" w:color="auto"/>\n'
                          '      <w:insideV w:val="single" w:sz="6" wx:bdrwidth="15" w:space="0" w:color="auto"/>\n'
                          '    </w:tblBorders>\n'
                          '    <w:tblLook w:val="000001e0" />\n'
                          '  </w:tblPr>\n')


    def depart_table(self, node):
        self.doc_tree.end( 'w:tbl' )
        self.doc_tree.start( 'w:p', {} )
        self.doc_tree.end( 'w:p' )
        self.body.append( '</w:tbl>\n' )
        self.body.append( '<w:p />' ) # add a blank line after the table
        self.in_table = 0


    def visit_target(self, node):
        if node.has_key('refid'):
            refid = self.gen_id()
            self.doc_tree.start( 'aml:annotation', { 'aml:id' : str(refid), 'w:type' : 'Word.Bookmark.Start', 'w:name' : node['refid'] })
            self.doc_tree.end( 'aml:annotation' )
            self.doc_tree.start( 'aml:annotation', { 'aml:id' : str(refid), 'w:type' : 'Word.Bookmark.End', 'w:name' : node['refid'] })
            self.doc_tree.end( 'aml:annotation' )
            self.body.append('<aml:annotation aml:id="' + str( refid ) + '" w:type="Word.Bookmark.Start" w:name="' + node['refid'] + '" />\n' )
            self.body.append('<aml:annotation aml:id="' + str( refid ) + '" w:type="Word.Bookmark.End" w:name="' + node['refid'] + '" />\n' )


    def depart_target(self, node):
        pass


    def visit_tbody(self, node):
        pass

    def depart_tbody(self, node):
        pass


    def visit_term(self, node):
        self.paragraph_properties[ 'term' ] = ( 'w:pStyle', { 'w:val' : 'DefinitionTerm' })
        #~ self.paragraph_properties[ 'term' ] = '<w:pStyle w:val="DefinitionTerm" />'
        self.visit_paragraph( node )


    def depart_term(self, node):
        del self.paragraph_properties[ 'term' ]
        self.depart_paragraph( node )


    def visit_tgroup(self, node):
        pass


    def depart_tgroup(self, node):
        pass


    def visit_thead(self, node):
        pass


    def depart_thead(self, node):
        pass


    def visit_tip(self, node):
        pass


    def depart_tip(self, node):
        pass


    def visit_title(self, node, move_ids=1):
        self.paragraph_properties[ 'title' ] = ( 'w:pStyle', { 'w:val' : self.title_styles[ self.section_level ] })
        #~ self.paragraph_properties[ 'title' ] = '<w:pStyle w:val="' + self.title_styles[ self.section_level ] + '"/>\n'
        self.visit_paragraph( node )


    def depart_title(self, node):
        del self.paragraph_properties[ 'title' ]
        self.depart_paragraph( node )


    def visit_title_reference(self, node):
        pass


    def depart_title_reference(self, node):
        pass


    def visit_topic(self, node):
        self.paragraph_properties[ 'topic' ] = ( 'w:pStyle', { 'w:val' : 'Topic' })
        #~ self.paragraph_properties[ 'topic' ] = '<w:pStyle w:val="BibliographMatter" />'
        self.visit_paragraph( node )


    def depart_topic(self, node):
        del self.paragraph_properties[ 'topic' ]
        self.depart_paragraph( node )


    def visit_transition(self, node):
        pass


    def depart_transition(self, node):
        pass


    def visit_version(self, node):
        self.paragraph_properties[ 'version' ] = ( 'w:pStyle', { 'w:val' : 'BibliographMatter' })
        # self.paragraph_properties[ 'version' ] = '<w:pStyle w:val="BibliographMatter" />'
        self.visit_paragraph( node )


    def depart_version(self, node):
        del self.paragraph_properties[ 'version' ]
        self.depart_paragraph( node )


    def visit_warning(self, node):
        pass


    def depart_warning(self, node):
        pass


    def visit_XMLRegion( self, node ):
        self.doc_tree.start( 'w:r', {} )
        self.doc_tree.start( 'w:rPr', {} )
        for style in node['styles']:
            self.doc_tree.start( style[0], style[1] )
            self.doc_tree.end( style[0] )
        self.doc_tree.end( 'w:rPr' )
        for tag in node['xml']:
            self.doc_tree.start( tag[0], tag[1] )
            self.doc_tree.end( tag[0] )


    def depart_XMLRegion( self, node ):
        self.doc_tree.end( 'w:r' )


    def unimplemented_visit(self, node):
        pass


