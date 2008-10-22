#!/usr/bin/env python
# -*- coding: UTF-8
"""
Semi-fork of rst2odt (borrowed ideas and helper functions)

"""
import StringIO
import os
import docutils.writers
from docutils.parsers import rst
from xml.dom import minidom
import re

import pygments
import pygments.lexers
import pygments.formatters
import zipwrap
import imagescale

try:
    from xml.etree import ElementTree as etree
except ImportError, e:
    try:
        from elementtree import ElementTree as etree
    except ImportError, e:
        print '***'
        print '*** Error: Must install either ElementTree or lxml or'
        print '***   a version of Python containing ElementTree.'
        print '***'
        raise


WIDTH = 28. # 11.02 inches in cm
HEIGHT = 21. # 8.27 inches in cm

FILL_PAT1 = re.compile(r'^ +')
FILL_PAT2 = re.compile(r' {2,}')

MIME_TYPE = 'application/vnd.oasis.opendocument.presentation'

NAME_SPACE = {
    'office:version': '1.0',
    'xmlns:chart': 'urn:oasis:names:tc:opendocument:xmlns:chart:1.0',
    'xmlns:dc': 'http://purl.org/dc/elements/1.1/',
    'xmlns:dom': 'http://www.w3.org/2001/xml-events',
    'xmlns:dr3d': 'urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0',
    'xmlns:draw': 'urn:oasis:names:tc:opendocument:xmlns:drawing:1.0',
    'xmlns:fo': 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0',
    'xmlns:form': 'urn:oasis:names:tc:opendocument:xmlns:form:1.0',
    'xmlns:math': 'http://www.w3.org/1998/Math/MathML',
    'xmlns:meta': 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0',
    'xmlns:number': 'urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0',
    'xmlns:office': 'urn:oasis:names:tc:opendocument:xmlns:office:1.0',
    'xmlns:ooo': 'http://openoffice.org/2004/office',
    'xmlns:oooc': 'http://openoffice.org/2004/calc',
    'xmlns:ooow': 'http://openoffice.org/2004/writer',
    'xmlns:presentation': 'urn:oasis:names:tc:opendocument:xmlns:presentation:1.0',
    'xmlns:script': 'urn:oasis:names:tc:opendocument:xmlns:script:1.0',
    'xmlns:style': 'urn:oasis:names:tc:opendocument:xmlns:style:1.0',
    'xmlns:svg': 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0',
    'xmlns:table': 'urn:oasis:names:tc:opendocument:xmlns:table:1.0',
    'xmlns:text': 'urn:oasis:names:tc:opendocument:xmlns:text:1.0',
    'xmlns:xforms': 'http://www.w3.org/2002/xforms',
    'xmlns:xlink': 'http://www.w3.org/1999/xlink',
    'xmlns:xsd': 'http://www.w3.org/2001/XMLSchema',
    'xmlns:xsi': 'http://www.w3.org/2001/XMLSchema-instance',
    }

URI2NS = {}
for k,v in NAME_SPACE.items():
    URI2NS[v] = k.split(":")[-1]

MANIFEST_NAMESPACE_ATTRIB = {
    'xmlns:manifest': 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0',
}
MANIFEST_NAMESPACE_DICT = {
    'manifest': 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0',
}

#add support of code-block
class SyntaxHighlightCodeBlock(rst.Directive):
    required_arguments = 1
    optional_arguments = 0
    has_content = True
    #
    # See visit_literal_block for code that processes the node 
    #   created here.
    def run(self):
        language = self.arguments[0]
        code_block = nodes.literal_block(classes=["code-block", language],
                                         language=language)
        lines = self.content
        content = '\n'.join(lines)
        text_node = nodes.Text(content)
        code_block.append(text_node)
        # Mark this node for high-lighting so that visit_literal_block
        #   will be able to hight-light those produced here and
        #   *not* high-light regular literal blocks (:: in reST).
        code_block['hilight'] = True
        # import pdb; pdb.set_trace()
        return [code_block]

rst.directives.register_directive('sourcecode', SyntaxHighlightCodeBlock)
rst.directives.register_directive('code', SyntaxHighlightCodeBlock)
class OdtPygmentsFormatter(pygments.formatter.Formatter):
    def __init__(self, rststyle_function):
        pygments.formatter.Formatter.__init__(self)
        self.rststyle_function = rststyle_function

    def rststyle(self, name, parameters=( )):
        return self.rststyle_function(name, parameters)

class OdtPygmentsProgFormatter(OdtPygmentsFormatter):
    def format(self, tokensource, outfile):
        tokenclass = pygments.token.Token
        for ttype, value in tokensource:
            value = escape_cdata(value)
            if ttype == tokenclass.Keyword:
                s2 = self.rststyle('codeblock-keyword')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            elif ttype == tokenclass.Literal.String:
                s2 = self.rststyle('codeblock-string')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            elif ttype in (
                    tokenclass.Literal.Number.Integer,
                    tokenclass.Literal.Number.Integer.Long,
                    tokenclass.Literal.Number.Float,
                    tokenclass.Literal.Number.Hex,
                    tokenclass.Literal.Number.Oct,
                    tokenclass.Literal.Number,
                    ):
                s2 = self.rststyle('codeblock-number')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            elif ttype == tokenclass.Operator:
                s2 = self.rststyle('codeblock-operator')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            elif ttype == tokenclass.Comment:
                s2 = self.rststyle('codeblock-comment')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            elif ttype == tokenclass.Name.Class:
                s2 = self.rststyle('codeblock-classname')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            elif ttype == tokenclass.Name.Function:
                s2 = self.rststyle('codeblock-functionname')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            elif ttype == tokenclass.Name:
                s2 = self.rststyle('codeblock-name')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            else:
                s1 = value
            outfile.write(s1)

class OdtPygmentsLaTeXFormatter(OdtPygmentsFormatter):
    def format(self, tokensource, outfile):
        tokenclass = pygments.token.Token
        for ttype, value in tokensource:
            value = escape_cdata(value)
            if ttype == tokenclass.Keyword:
                s2 = self.rststyle('codeblock-keyword')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            elif ttype in (tokenclass.Literal.String,
                    tokenclass.Literal.String.Backtick,
                    ):
                s2 = self.rststyle('codeblock-string')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            elif ttype == tokenclass.Name.Attribute:
                s2 = self.rststyle('codeblock-operator')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            elif ttype == tokenclass.Comment:
                if value[-1] == '\n':
                    s2 = self.rststyle('codeblock-comment')
                    s1 = '<text:span text:style-name="%s">%s</text:span>\n' % \
                        (s2, value[:-1], )
                else:
                    s2 = self.rststyle('codeblock-comment')
                    s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                        (s2, value, )
            elif ttype == tokenclass.Name.Builtin:
                s2 = self.rststyle('codeblock-name')
                s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                    (s2, value, )
            else:
                s1 = value
            outfile.write(s1)





class _ElementInterfaceWrapper(etree._ElementInterface):
    def __init__(self, tag, attrib=None):
        etree._ElementInterface.__init__(self, tag, attrib)
        if attrib is None:
            attrib = {}
        self.parent = None
    def setparent(self, parent):
        self.parent = parent
    def getparent(self):
        return self.parent 


# ElementTree support functions.
# In order to be able to get the parent of elements, must use these
#   instead of the functions with same name provided by ElementTree.
#
def Element(tag, attrib=None, nsmap=None, nsdict=NAME_SPACE):
    if attrib is None:
        attrib = {}
    #tag, attrib = fix_ns(tag, attrib, nsdict)
    el = _ElementInterfaceWrapper(tag, attrib)
    return el

def SubElement(parent, tag, attrib=None, nsmap=None, nsdict=NAME_SPACE):
    if attrib is None:
        attrib = {}
    #tag, attrib = fix_ns(tag, attrib, nsdict)
    el = _ElementInterfaceWrapper(tag, attrib)
    parent.append(el)
    el.setparent(parent)
    return el

def fix_ns(tag, attrib, nsdict):
    nstag = add_ns(tag, nsdict)
    nsattrib = {}
    for key, val in attrib.iteritems():
        nsattrib[key] = val
    return nstag, nsattrib


def to_string(et, pretty=False):
    outstream = StringIO.StringIO()
    et.write(outstream)
    s1 = outstream.getvalue()
    outstream.close()
    if pretty:
        s1 = pretty_xml(s1)
    return s1

def pretty_xml(string_input):
    doc = minidom.parseString(string_input)
    s1 = doc.toprettyxml('  ')
    return s1


def escape_cdata(text):
    text = text.replace("&", "&amp;")
    text = text.replace("<", "&lt;")
    text = text.replace(">", "&gt;")
    ascii = ''
    for char in text:
      if ord(char) >= ord("\x7f"):
          ascii += "&#x%X;" % ( ord(char), )
      else:
          ascii += char
    return ascii

class Writer(docutils.writers.Writer):
    
    supported = ('odp',)
    output = ('odp',)

    def translate(self):
        # iterate over self.document
        visitor = Translator(self.document)
        try:
            self.document.walkabout(visitor)

        except Exception, e:
            print "E", e
            raise
        visitor.get_output()
        self.output = visitor.output
        self.images = visitor.images
        self.create_odp(visitor.odp)

    def create_odp(self, zip):
        manifest = self.create_manifest()
        zip.touch("META-INF/manifest.xml", manifest)
        zip.touch("content.xml", self.output)
        fout = open('/tmp/content.xml', 'w')
        fout.write(self.output)
        zip.zipit()
        
    def create_manifest(self):
        root = Element('manifest:manifest',
                       attrib=MANIFEST_NAMESPACE_ATTRIB,
                       nsdict=MANIFEST_NAMESPACE_DICT,
                       )
        doc = etree.ElementTree(root)
        doc.namespaces = URI2NS
        SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': MIME_TYPE,
            'manifest:full-path': '/',
            }, nsdict=MANIFEST_NAMESPACE_DICT)
        SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': 'text/xml',
            'manifest:full-path': 'content.xml',
            }, nsdict=MANIFEST_NAMESPACE_DICT)
        SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': 'text/xml',
            'manifest:full-path': 'styles.xml',
            }, nsdict=MANIFEST_NAMESPACE_DICT)
        SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': 'text/xml',
            'manifest:full-path': 'meta.xml',
            }, nsdict=MANIFEST_NAMESPACE_DICT)
        for image in self.images:
            SubElement(root, 'manifest:file-entry', 
                       attrib={"manifest:media-type":"image/jpeg",
                               'manifest:full-path':image},
                       nsdict=MANIFEST_NAMESPACE_DICT)
                    
        s1 = to_string(doc)
        return s1


class Translator(docutils.nodes.NodeVisitor):
    """
    calls visit_* and depart_* for Nodes when self.walk is called
    """
    
    def __init__(self, document):
        self.odp = zipwrap.ZipWrap("out.odp")
        document.settings.strict_visitor = False
        docutils.nodes.NodeVisitor.__init__(self, document)
        document.settings.traceback = True
        
        self.root = Element('office:document-content', attrib=NAME_SPACE)

        self.automatic_sytles = self.append_child("office:automatic-styles",
                                                  parent=self.root)
        self.styles_added = {}
        self.create_basic_styles()

        body = self.append_child("office:body", parent=self.root)

        self.preso = self.append_child("office:presentation", parent=body)
        self.current_element = self.preso
        self.images = []
       
        self.classes = {} # style classes
        self.format_map = {} # from odt
        self._reset_values()

    def _reset_values(self):
        """
        These are the state of where we are
        At the start of every section these should be reset
        """
        self.list_depth = 0
        self.in_enumerated_list = False
        self.sections = 0 # used for page count
        self.para_styles = [] # Stack of current paragraph style
        self.block_quote = False
        self.page = None
        self.para = None # current paragraph, only support 1 per section
        self.in_comment = False
        self.in_emphasis = False

    def get_output(self):
        tree = etree.ElementTree(self.root)
        self.output =  to_string(tree) 
        
        
    def append_child(self, tag, attrib=None, parent=None):
        if parent is None:
            parent = self.current_element
        if attrib is None:
            el = SubElement(parent, tag)
        else:
            el = SubElement(parent, tag, attrib)
        return el
        
    def create_basic_styles(self):
        ## Preso styles
        self._add_style("dp1",
                        {"style:name":"dp1", "style:family":"drawing-page"},
                        'style:drawing-page-properties',
                        { "presentation:background-visible":"true",
                          "presentation:background-objects-visible":"true",
                          "presentation:display-footer":"true",
                          "presentation:display-page-number":"false",
                          "presentation:display-date-time":"true"})

        basic_paragraph = {'fo:margin-left':'0cm',
                           'fo:margin-right':'0cm',
                           'fo:text-indent':'0cm'}
        
        ## Paragraph Styles
        # Title (P1), left (P2), center (P3), right (P4), P6 (block quote)
        for name, dict_items in [('P1', {}),
                                 ('left_align', {'fo:text-align':'start'}),
                                 ('center_align', {'fo:text-align':'center'}),
                                 ('right_align', {'fo:text-align':'end'}),
                                 ('P6', {'fo:margin-left':'1.2cm',
                                         'fo:text-indent':'-0.9cm'}),
                                 ]:
            attr = basic_paragraph.copy()
            for key, value in dict_items.items():
                attr[key] = value
            self._add_style(name,  
                            {'style:family':'paragraph', 
                             'style:name':name},
                            'style:paragraph-properties', 
                            attr)


        ## Text styles
        # T1
        bold_style = self._add_style('bold', 
                                     {'style:name':'bold',
                                      'style:family':'text'},
                                     'style:text-properties',
                                     {'fo:font-weight':'bold',
                                      'style:font-weight-asian':'bold', 
                                      'style:font-weight-complex':'bold'})

        # T2                             
        italic_style = self._add_style('italic', 
                                       {'style:name':'italic',
                                        'style:family':'text'},
                                       'style:text-properties',
                                       {'fo:font-weight':'normal',
                                        'style:font-weight-asian':'italic', 
                                        'style:font-weight-complex':'normal'})
                                     
                                
        ## Image styles
        self._add_style('gr2',
                        {"style:name":'gr2',
                         "style:family":"graphic",
                         "style:parent-style-name":"standard"},
                        'style:graphic',
                        {"draw:stroke":"none", 
                         "draw:fill":"none", 
                         "draw:textarea-horizontal-align":"center", 
                         "draw:textarea-vertical-align":"middle", 
                         "draw:color-mode":"standard", 
                         "draw:luminance":"0%", 
                         "draw:contrast":"0%", 
                         "draw:gamma":"100%", 
                         "draw:red":"0%", 
                         "draw:green":"0%", 
                         "draw:blue":"0%", 
                         "fo:clip":"rect(0cm 0cm 0cm 0cm)", 
                         "draw:image-opacity":"100%", 
                         "style:mirror":"none"})



    def __getattr__(self, name):
        print "GET ATTR", str(name)
        return docutils.nodes.NodeVisitor.__getattr__(name)
    
    def _dumb_depart(self, node):
        pass

    def set_to_parent(self):
        self.current_element = self.current_element.getparent()

    def visit_document(self, node):
        print "DOC", node
        fout = open("/tmp/doc.xml",'w')
        fout.write(str(node))
        fout.close()

    depart_document = _dumb_depart

    def visit_section(self, node):
        self._reset_values()

        self.sections = self.sections + 1
        style_name = "dp1"
        self.page = self.append_child('draw:page', 
                                      attrib={"draw:name":"page%d" % self.sections,
                                              "draw:style-name":style_name,
                                              "draw:master-page-name":"Default",
                                              "presentation:presentation-page-layout-name":"AL1T0"
                                              },
                                      parent=self.preso)
        self.append_child('office:forms',
                          attrib={"form:apply-design-mode":"false",
                                  "form:automatic-focus":"false"},
                          parent=self.page)
        self.current_element = self.page
                          


    def depart_section(self, node):
        self.current_element = self.preso
        self.page = None


    def visit_title(self, node):
        if self.page is None:
            self.visit_section(node)

        self.para_styles.append("P1")
        self._create_text_frame(attrib={ "presentation:style-name":"pr1",
                                    "draw:layer":"layout",
                                    "svg:width":"25.199cm",
                                    "svg:height":"3.256cm",
                                    "svg:x":"1.4cm",
                                    "svg:y":"0.962cm",
                                    "presentation:class":"title"},)
    def depart_title(self, node):
        self.current_element = self.page
        self.para_styles.pop()


    def _create_frame(self, attrib=None):
        attrib = attrib or {}
        frame = self.append_child('draw:frame',
                                  attrib=attrib,
                                  parent=self.page)
        return frame

    def _create_image_frame(self, node, attrib=None):
        style_name = "gr2" # might need to increment?
        uri = node.attributes['uri']
        im = imagescale.ImageScale(uri)
        x, y, w, h = im.adjust_size(WIDTH, HEIGHT)
        x_str = "%fcm" % x
        y_str = "%fcm" % y
        w_str = "%fcm" % w
        h_str = "%fcm" % h
        frame = self._create_frame(attrib={ "draw:style-name":style_name,
                                            "draw:text-style-name":"P6",
                                            "draw:layer":"layout",
                                            "svg:width":w_str, #"31.585cm",
                                            "svg:height":h_str, #"21cm",
                                            "svg:x":x_str, #"-1.781cm",
                                            "svg:y":y_str #"0cm"
                                            })
        

        uri = node.attributes['uri']
        filename =  os.path.basename(uri)
        ext = filename.split(".")[-1]
        base = filename[:-len(ext)-1]
        dest = os.path.join("Pictures", "%s.%s"%( base, ext))
        count = 0
        while True:
            try:
                self.odp.cat(dest)
                
            except IOError, e:
                #shutil.copy(uri, os.path.join("data",dest))
                self.odp.touch(dest, open(uri).read())
                break

            dest = os.path.join( "Pictures", "%s%d.%s"%(base, count,ext))
            count = count + 1
        
        self.images.append(dest)
        # Get dimensions from node.uri
        image = self.append_child('draw:image',
                                  attrib={"xlink:href":dest,
                                          "xlink:type":"simple",
                                          "xlink:show":"embed",
                                          "xlink:actuate":"onLoad"},
                                  parent=frame)
        # add text:p for kicks
        p = self.append_child('text:p',
                              attrib={"text:style-name":"P1"},
                              parent=image)
        
        self.current_element = image

        return frame
        
    def _create_text_frame(self, attrib=None):
        frame = self._create_frame(attrib)

        text_box = self.append_child('draw:text-box',
                                     parent=frame)
        self.current_element = text_box
        if self.para_styles:
            sname = self.para_styles[-1]
            p = self.append_child('text:p',
                              attrib={"text:style-name":sname},
                              parent=text_box)
            self.current_element = p

        return frame

    def _create_list_frame(self, attrib=None):
        frame = self._create_frame(attrib)

        self.frame_text_box = self.append_child('draw:text-box',
                                     attrib={"presentation:style-name":"pr4",
                                    "draw:layer":"layout",
                                    "svg:width":"25.199cm",
                                    "svg:height":"13.609cm",
                                    "svg:x":"1.4cm",
                                    "svg:y":"5.039cm",
                                    "presentation:class":"outline"},
                                     parent=frame)
        self.current_element = self.frame_text_box
        return frame
        

    def visit_Text(self, node):
        if self.in_comment:
            return
        if self.list_depth > 0:
            # don't wrap lists....
            pass
        elif self.in_emphasis:
            style = "bold"
            attrib = {"text:style-name":style}
            span = self.append_child("text:span",
                                     attrib=attrib)
            self.current_element = span
            
        if isinstance(node.parent, docutils.nodes.literal_block):
            #isinstance(node.parent, docutils.nodes.term) or \
            #isinstance(node.parent, docutils.nodes.definition):
            return
        text = node.astext()
        # Are we in mixed content?  If so, add the text to the
        #   etree tail of the previous sibling element.
        if len(self.current_element.getchildren()) > 0:
            #print '*** (visit_Text) 1. text: %s' % text
            if self.current_element.getchildren()[-1].tail:
                self.current_element.getchildren()[-1].tail += text
            else:
                self.current_element.getchildren()[-1].tail = text
        else:
            if self.current_element.text:
                self.current_element.text += text
            else:
                self.current_element.text = text


    def depart_Text(self, node):
        pass


    def visit_paragraph(self, node):
        if self.list_depth == 0 and self.para is None:
            # Not necessary if in a bullet list
            self.para = self._create_text_frame(attrib={"presentation:style-name":"pr2",
                                            "draw:layer":"layout",
                                            "svg:width":"25.199cm",
                                            "svg:height":"13.609cm",
                                            "svg:x":"1.4cm",
                                            "svg:y":"5.039cm",
                                            "presentation:class":"subtitle"})

        else:

            print "PARA IN LIST!"


    def depart_paragraph(self, node):
        pass

    def visit_block_quote(self, node):
        self.para_styles.append("P6")
        self.block_quote = True

    def depart_block_quote(self, node):
        self.para_styles.pop()
        self.block_quote = False

    def visit_image(self, node):
        self._create_image_frame(node)

    depart_image = _dumb_depart

    def visit_enumerated_list(self, node):
        self.in_enumerated_list = True

    def depart_enumerated_list(self, node):
        self.in_enumerated_list = False

    def visit_bullet_list(self, node):
        if self.list_depth == 0:
            self._create_list_frame()
            print "BULLET", node
            self.list_depth = 0
        else:
            # sublist
            self.current_element = self.frame_text_box

    depart_bullet_list = _dumb_depart

    def visit_list_item(self, node):
        """
        Create something like this

            <text:list text:style-name="L2">
	      <text:list-item>
		<text:p text:style-name="P2">
		Item2</text:p>
	      </text:list-item>
	    </text:list>
	    <text:list text:style-name="L2">
	      <text:list-item>
		<text:list>
		  <text:list-item>
		    <text:p text:style-name="P3">
		    item2a</text:p>
		  </text:list-item>
		</text:list>
	      </text:list-item>
	    </text:list>

           style-name starts at P2 for first level, P3 for second
        """
        self.list_depth = self.list_depth + 1
        print "LI", node
        parent = self.current_element
        for i in range(self.list_depth):
            if i == 0:
                attrib = {"text:style-name":"L2"}
            else:
                attrib = {}
            if self.in_enumerated_list:
                attrib['text:enable-numbering'] = "true"
            list = self.append_child("text:list", attrib, parent=parent)
            list_item = self.append_child("text:list-item", {}, parent=list)
            parent = list_item # needed for nesting

        p = self.append_child("text:p", {"text:style-name":"P"+str(self.list_depth+1)},
                              parent=list_item)
        self.current_element = p
    

    def depart_list_item(self, node):
        self.list_depth = self.list_depth - 1
        self.current_element = self.frame_text_box


    def visit_emphasis(self, node):
        self.in_emphasis = True

    def depart_emphasis(self, node):
        self.in_emphasis = False


    def visit_comment(self, node):
        self.in_comment = True

    def depart_comment(self, node):
        self.in_comment = False


    def visit_line_block(self, node):
        if self.para is None:
            self.visit_paragraph(node)
        
        p = self.append_child('text:p', {'text:style-name':'right_align'})
        self.current_element = p


    def depart_line_block(self, node):
        self.set_to_parent()

    def visit_line(self, node):
        pass

    def depart_line(self, node):
        self.append_child('text:line-break')


    def visit_reference(self, node):
        """
        convert:
        <reference refuri="http://panela.blog-city.com/">http://panela.blog-city.com/</reference>
        to:
        <text:a xlink:href="#http://yahoo.com">Reference</text:a>"""
        print "REF", node
        a = self.append_child('text:a',
                              attrib={'xlink:href':node.attributes.get("refuri")},
                              parent=self.current_element)
        a.text = node.attributes.get("name")
        self.current_element = a

    def depart_reference(self, node):
        self.set_to_parent()



    def visit_system_message(self, node):
        pass
    depart_system_message = _dumb_depart

    def visit_literal_block(self, node):
        wrapper1 = '<text:p text:style-name="%s">%%s</text:p>' % (
            self.rststyle('codeblock'), )
        language = node.get('language', 'python')
        source = self._add_syntax_highlighting(node.astext(), language)
        lines = source.split('\n')
        lines1 = ['<wrappertag1 xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0">']

        my_lines = []
        for my_line in lines:
            my_line = self.fill_line(my_line)
            my_line = my_line.replace("&#10;", "\n")
            my_lines.append(my_line)
        my_lines_str = '<text:line-break/>'.join(my_lines)
        my_lines_str2 = wrapper1 % (my_lines_str, )
        lines1.append(my_lines_str2)
        lines1.append('</wrappertag1>')
        s1 = ''.join(lines1)
        #if WhichElementTree != "lxml":
        s1 = s1.encode("utf-8")
        el1 = etree.fromstring(s1)
        children = el1.getchildren()
        for child in children:
            self.current_element.append(child)

    depart_literal_block = _dumb_depart

    def fill_line(self, line):
        line = FILL_PAT1.sub(self.fill_func1, line)
        line = FILL_PAT2.sub(self.fill_func2, line)
        return line

    def fill_func1(self, matchobj):
        spaces = matchobj.group(0)
        repl = '<text:s text:c="%d"/>' % (len(spaces), )
        return repl

    def fill_func2(self, matchobj):
        spaces = matchobj.group(0)
        repl = ' <text:s text:c="%d"/>' % (len(spaces) - 1, )
        return repl


    def visit_code_block(self, node):
        print "CODE BLOCK", node
        
    depart_code_block = _dumb_depart
    
    def visit_footnote(self, node):
        """
        <footnote auto="1" ids="id1" names="1"><label>1</label><paragraph>Where "7" is 7+/-2, and "magic number" is working memory capacity
(Miller 1956)</paragraph></footnote>
        """
        print "FOOTNOTE", node 
        # figure out what to do here
        

    depart_footnote = _dumb_depart

    def visit_label(self, node):
        pass
    depart_label = _dumb_depart

    def _add_syntax_highlighting(self, insource, language):
        #print '(_add_syntax_highlighting) using lexer: %s' % (language, )
        lexer = pygments.lexers.get_lexer_by_name(language, stripall=True)
        if language in ('latex', 'tex'):
            fmtr = OdtPygmentsLaTeXFormatter(lambda name, parameters=():
                self.rststyle(name, parameters))
        else:
            fmtr = OdtPygmentsProgFormatter(lambda name, parameters=():
                self.rststyle(name, parameters))
        outsource = pygments.highlight(insource, lexer, fmtr)
        return outsource

    def rststyle(self, name, parameters=( )):
        """
        Returns the style name to use for the given style.

        If `parameters` is given `name` must contain a matching number of ``%`` and
        is used as a format expression with `parameters` as the value.
        """
##        template = self.format_map.get(name, 'rststyle-%s' % name)
##        return template % parameters
        name1 = name % parameters
        stylename = self.format_map.get(name1, 'rststyle-%s' % name1)
        return stylename

    def _adjust_classes(self, node):
        classes = node.attributes.get("classes", None)
        if classes:
            self.classes = {}
            for klass in classes:
                self.classes[klass] = 1
                if klass == "right":
                    pass
                elif klass == "big":
                    pass

    def _add_style(self, name, parent_attr, child_tag=None,child_attr=None):
        if name not in self.styles_added:
            self.styles_added[name] = 1
            e1 = SubElement(self.automatic_sytles, 'style:style',
                            parent_attr)
            if child_tag:
                e1_1 = SubElement(e1, child_tag,
                                  child_attr)

