"""
Open Document Format (ODF) Writer for slides

"""
import os
import sys
from xml.dom import minidom
import zipfile

from docutils import utils, frontend

import odtwriter as odt

def cwd_decorator(func):
    """
    decorator to change cwd to directory containing rst for this function
    """
    def wrapper(*args, **kw):
        cur_dir = os.getcwd()
        found = False
        for arg in sys.argv:
            if arg.endswith(".rst"):
                found = arg
                break
        if found:
            os.chdir(os.path.dirname(arg))
        data = func(*args, **kw)
        os.chdir(cur_dir)        
        return data
    return wrapper



def pretty_xml(string_input):
    doc = minidom.parseString(string_input)
    s1 = doc.toprettyxml('  ')
    return s1


class Writer(odt.Writer):

    EXTENSION = '.odp'
    default_stylesheet = 'styles' + EXTENSION
    default_stylesheet_path = utils.relative_path(
        os.path.join(os.getcwd(), 'dummy'),
        os.path.join(os.path.dirname(__file__), default_stylesheet))
    settings_spec = (
        'ODF-Specific Options',
        None,
        (
            ('Specify a stylesheet URL, used verbatim.  Overrides '
            '--stylesheet-path.',
            ['--stylesheet'],
            {'metavar': '<URL>', 'overrides': 'stylesheet_path'}),
        ('Specify a stylesheet file, relative to the current working '
            'directory.  The path is adjusted relative to the output ODF '
            'file.  Overrides --stylesheet.  Default: "%s"'
            % default_stylesheet_path,
            ['--stylesheet-path'],
            {'metavar': '<file>', 'overrides': 'stylesheet',
                'default': default_stylesheet_path}),
        ('Specify a configuration/mapping file relative to the '
            'current working '
            'directory for additional ODF options.  '
            'In particular, this file may contain a section named '
            '"Formats" that maps default style names to '
            'names to be used in the resulting output file allowing for '
            'adhering to external standards. '
            'For more info and the format of the configuration/mapping file, '
            'see the odtwriter doc.',
            ['--odf-config-file'],
            {'metavar': '<file>'}),
        ('Obfuscate email addresses to confuse harvesters while still '
            'keeping email links usable with standards-compliant browsers.',
            ['--cloak-email-addresses'],
            {'default': False, 'action': 'store_true',
                'validator': frontend.validate_boolean}),
        ('Specify the thickness of table borders in thousands of a cm.  '
            'Default is 35.',
            ['--table-border-thickness'],
            {'default': 35,
                'validator': frontend.validate_nonnegative_int}),
        ('Add syntax highlighting in literal code blocks.'
            'Default is No.  Requires installation of Pygments.',
            ['--add-syntax-highlighting'],
            {'default': False, 'action': 'store_true',
                'validator': frontend.validate_boolean}),
        ('Create sections for headers. '
            'Default is Yes.',
            ['--create-sections'],
            {'default': True, 'action': 'store_true',
                'validator': frontend.validate_boolean}),
        ('Create no sections for headers.',
            ['--no-create-sections'],
            {'action': 'store_false',
                'dest': 'create_sections',
                'validator': frontend.validate_boolean}),
        ('Create links. '
            'Default is No.',
            ['--create-links'],
            {'default': False, 'action': 'store_true',
                'validator': frontend.validate_boolean}),
        ('Create no links.',
            ['--no-create-links'],
            {'action': 'store_false',
                'dest': 'create_links',
                'validator': frontend.validate_boolean}),
        ))


    def __init__(self):
        self.MIME_TYPE = 'application/vnd.oasis.opendocument.presentation'
        odt.Writer.__init__(self)
        self.translator_class = ODPTranslator


    def translate(self):
        odt.Writer.translate(self)
        content = self.visitor.content_astext()
        fout  = open("/tmp/content.xml", 'w')
        fout.write(pretty_xml(content))
        print pretty_xml(content)


class ODPTranslator(odt.ODFTranslator):
    def __init__(self, document):
        odt.ODFTranslator.__init__(self, document)
        self._reset_values()

    def _reset_values(self):
        """we reset on each section/slide"""
        self.text_box = None # only supporting one text box (inside frame) (not including title)
        self.page = None
        self.in_enumerated_list = False
        self.in_bullet = False
        self.prev_stack = None

        # not a new one, but needs to be reset
        self.paragraph_style_stack = [self.rststyle('textbody'),]


    def generate_content_element(self, root):
        return odt.SubElement(root, 'office:presentation')

    def setup_paper(self, root_el):
        w, h = 792, 612 # Letter Landscape
        SNSD = odt.SNSD
        def walk(el):
            if el.tag == "{%s}page-layout-properties" % SNSD["style"] and \
		    not el.attrib.has_key("{%s}page-width" % SNSD["fo"]):
                el.attrib["{%s}page-width" % SNSD["fo"]] = "%.3fpt" % w
                el.attrib["{%s}page-height" % SNSD["fo"]] = "%.3fpt" % h
                el.attrib["{%s}margin-left" % SNSD["fo"]] = \
                        el.attrib["{%s}margin-right" % SNSD["fo"]] = \
                        "%.3fpt" % (.1 * w)
                el.attrib["{%s}margin-top" % SNSD["fo"]] = \
                        el.attrib["{%s}margin-bottom" % SNSD["fo"]] = \
                        "%.3fpt" % (.1 * h)
            else:
                for subel in el.getchildren(): walk(subel)
        walk(root_el)

    def append_pending_ids(self, el):
        """  !!! placing text:reference-mark in text:p crashes OOo"""
        while el.tag == 'text:p':
            el = el.parent
        if self.settings.create_links:
            for id in self.pending_ids:
                odt.SubElement(el, 'text:reference-mark', attrib={
                        'text:name': id})
        self.pending_ids = [ ]

    def visit_document(self, node):
        print "DOC", node

    def visit_section(self, node):
        self._reset_values()
        self.section_count += 1
        style_name = "dp1"
        self.page = self.append_child('draw:page', 
                                      attrib={"draw:name":"page%d" % self.section_count,
                                              "draw:style-name":style_name,
                                              "draw:master-page-name":"Default",
                                              "presentation:presentation-page-layout-name":"AL1T0"
                                              },
                                      parent=self.body_text_element)
        self.append_child('office:forms',
                          attrib={"form:apply-design-mode":"false",
                                  "form:automatic-focus":"false"},
                          parent=self.page)
        self.current_element = self.page


    def depart_section(self, node):
        self.current_element = self.body_text_element
        self.page = None


    def visit_title(self, node):
        """
        text:h in super class crashes OOo!!!
        """
        if self.page is None:
            self.visit_section(node)
        self._create_text_frame(attrib={ "presentation:style-name":"pr1",
                                    "draw:layer":"layout",
                                    "svg:width":"25.199cm",
                                    "svg:height":"3.256cm",
                                    "svg:x":"1.4cm",
                                    "svg:y":"0.962cm",
                                    "presentation:class":"title"},)


    def depart_title(self, node):
        self.current_element = self.page
        self.text_box = None


    def _create_text_frame(self, attrib=None):
        frame = self._create_frame(attrib)
        text_box = self.append_child('draw:text-box',
                                     parent=frame)
        self.text_box = text_box
        self.current_element = text_box
        return frame


    def _create_frame(self, attrib=None):
        attrib = attrib or {}
        frame = self.append_child('draw:frame',
                                  attrib=attrib,
                                  parent=self.page)
        return frame


    def _create_text_area(self):
        if self.text_box is None:
            el = self._create_text_frame(attrib={"presentation:style-name":"pr2",
                                                 "draw:layer":"layout",
                                                 "svg:width":"25.199cm",
                                                 "svg:height":"13.609cm",
                                                 "svg:x":"1.4cm",
                                                 "svg:y":"5.039cm",
                                                 "presentation:class":"subtitle"})


    def _manage_classes(self, node, called_from_para=False):
        """
        !!! I have right and big classes (from s5) that are currently ignored
        """
        return


    def visit_line_block(self, node):
        if self.text_box is None:
            self._create_text_area()
        self.prev_stack = self.paragraph_style_stack[:]

            
    def depart_line_block(self, node):
        self.paragraph_style_stack = self.prev_stack
        self.blockstyle = ''
        odt.ODFTranslator.depart_line_block(self, node)
        self.current_element = self.text_box

    def depart_line(self, node):
        self.append_child('text:line-break')

    # !!! visit_literal_block creating broken xml (<ns0:... instead of <text:line-break

    def visit_block_quote(self, node):
        if self.text_box is None:
            self._create_text_area()
        odt.ODFTranslator.visit_block_quote(self, node)

    def visit_paragraph(self, node):
        if self.text_box is None:
            self._create_text_area()
        odt.ODFTranslator.visit_paragraph(self, node)
        
        
    def visit_literal_block(self, node):
        if self.text_box is None:
            self._create_text_area()
        odt.ODFTranslator.visit_literal_block(self, node)
        

    def _in_p(self):
        """
        Determine if we are already in a text:p, odp doesn't like
        nested ones too much
        """
        node = self.current_element
        while not node is None:
            if node.tag == 'text:p':
                return True
            node = node.parent
        return False
         
    def visit_emphasis(self, node):
        if not self._in_p():
            self.current_element = self.append_p("default")
        odt.ODFTranslator.visit_emphasis(self, node) 
    
    def visit_Text(self, node):
        if not self._in_p():
            self.current_element = self.append_p("default")
        odt.ODFTranslator.visit_Text(self, node)

    def visit_enumerated_list(self, node):
        self.in_enumerated_list = True

    def depart_enumerated_list(self, node):
        self.in_enumerated_list = False
        odt.ODFTranslator.depart_enumerated_list(self, node)

    def visit_bullet_list(self, node):
        if self.text_box is None:
            self._create_text_area()

        self.in_bullet = True


    def depart_bullet_list(self, node):
        odt.ODFTranslator.depart_bullet_list(self, node)
        self.in_bullet = False

    def visit_list_item(self, node):
        """
        !!!odt bullet handling isn't correct
        doesn't put </text:list> around EVERY item
        """
        if self.in_bullet:
            odt.ODFTranslator.visit_bullet_list(self, node)
        if self.in_enumerated_list:
            odt.ODFTranslator.visit_enumerated_list(self, node)
        
        odt.ODFTranslator.visit_list_item(self, node)

            
    def depart_list_item(self, node):
        odt.ODFTranslator.depart_list_item(self, node)
        #need an extra dosing here, for bug in lists
        self.set_to_parent()

    def set_to_parent(self, node=None):
        # setting to parent can be bad for presos
        if self.text_box and self.text_box == self.current_element:
            return
        odt.ODFTranslator.set_to_parent(self)


    @cwd_decorator
    def visit_image(self, node):
        """
        needs to look like this (under draw:page)

	<draw:frame draw:style-name="gr2" draw:text-style-name="P2" draw:layer="layout" svg:width="19.589cm" svg:height="13.402cm" svg:x="3.906cm" svg:y="4.378cm">
	  <draw:image xlink:href="Pictures/10000201000002F800000208188B22AE.png" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad">
	    <text:p text:style-name="P1"/>
	  </draw:image>
	</draw:frame>
        """
        # odp images can only be attached to page
        old = self.current_element
        self.current_element = self.page
        # also need to pretend we are in a paragraph (else won't show)
        prev_para = self.in_paragraph
        self.in_paragraph = True
        # anther hack 
        node.attributes['scale'] = 100 # percent
        # might be nice to add scale, crop, fill attributes....

        odt.ODFTranslator.visit_image(self, node)

        self.current_element = old
        self.in_paragraph = prev_para


