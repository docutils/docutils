"""
Open Document Format (ODF) Writer for slides

Todo:

 move custom styles to child of automaticstyles in styles.xml

"""
import os
import sys
from xml.dom import minidom

from docutils import utils, frontend

import odtwriter as odt


S5_COLORS = dict(
    black='#000000',
    gray='#545454',
    silver='#c0c0c0',
    white='#ffffff',
    maroon='#b03060',
    red='#ff0000',
    magenta='#ff00ff',
    fuchsia='#ff00ff',
    pink='#ff1493',
    orange='#ffa500',
    yellow='#ffff00',
    lime='#32cd32',
    green='#00ff00',
    olive='#6b8e23',
    teal='#008080',
    cyan='#00ffff',
    aqua='#00ffff',
    blue='#0000ff',
    navy='#000080',
    purple='#a020f0'
)
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
            directory = os.path.dirname(arg)
            if directory:
                os.chdir(directory)
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
            {'default': True, 'action': 'store_true',
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
        pretty = pretty_xml(content).encode('utf-8')
        fout.write(pretty)
        print pretty


class ODPTranslator(odt.ODFTranslator):
    def __init__(self, document):
        odt.ODFTranslator.__init__(self, document)
        self.styles_seen = {}
        self._reset_values()


    def _reset_values(self):
        """we reset on each section/slide"""
        self.text_stack = [] # styles to apply to text
        self.para_stack = ['left'] # styles for paragraphs
        self.text_box = None # only supporting one text box (inside frame) (not including title)
        self.page = None
        self.handout_text = None
        self.in_enumerated_list = False
        self.in_bullet = False
        self.prev_stack = None
        self.in_handout = False
        self.in_comment = False

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
        #style_name = 'pr1'
        style_name = 'title2'
        
        self._create_text_frame(attrib={ "presentation:style-name":style_name,
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


    def _create_frame(self, attrib=None, parent=None):
        parent = parent or self.page
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
        also handout class
        """
        pass

    def visit_line_block(self, node):
        if self.text_box is None:
            self._create_text_area()
        self.prev_stack = self.paragraph_style_stack[:]

            
    def depart_line_block(self, node):
        self.paragraph_style_stack = self.prev_stack
        self.blockstyle = ''
        odt.ODFTranslator.depart_line_block(self, node)
        self.current_element = self.text_box

    def visit_line(self, node):
        if not self._in_p():
            style = 'lineblock%d' % self.line_indent_level
            el1 = odt.SubElement(self.current_element, 'text:p', attrib={
                    'text:style-name': self.rststyle(style),
                    })
            self.current_element = el1

    def depart_line(self, node):
        self.append_child('text:line-break')

    # !!! visit_literal_block creating broken xml (<ns0:... instead of <text:line-break

    def visit_block_quote(self, node):
        if self.text_box is None:
            self._create_text_area()
        odt.ODFTranslator.visit_block_quote(self, node)

    def visit_container(self, node):
        if not self.in_handout:
            self.in_handout = 'handout' in node.attributes.get('classes', [])
        odt.ODFTranslator.visit_container(self, node)

    def depart_container(self, node):
        self.in_handout = False
        odt.ODFTranslator.depart_container(self, node)

    def visit_paragraph(self, node):
        if self.text_box is None:
            self._create_text_area()

        classes = node.attributes.get('classes', [])
        if not self.in_handout:
            self.in_handout = 'handout' in classes
        self.para_stack.append(' '.join(classes))

        ##odt.ODFTranslator.visit_paragraph(self, node)
        #style_name = self.paragraph_style_stack[-1]
        style_name = generate_paragraph_style(self, self.para_stack)
        el = self.append_child('text:p',
                               attrib={'text:style-name': self.rststyle(style_name)})
        self.append_pending_ids(el)
        self.set_current_element(el)

        

    def depart_paragraph(self, node):
        self.in_handout = False
        self.para_stack.pop()
        odt.ODFTranslator.depart_paragraph(self, node)
        
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
        
    def visit_inline(self, node):
        classes = node.attributes.get('classes', [])
        text_classes = []
        for c in classes:
            if c in S5_COLORS:
                text_classes.append('color:%s' % S5_COLORS[c])
        self.text_stack.append(' '.join(text_classes))
        sname = generate_text_style(self, self.text_stack)
        el = odt.SubElement(self.current_element, 'text:span',
                        attrib={'text:style-name': self.rststyle(sname)})
        self.set_current_element(el)

    def depart_inline(self, node):
        self.text_stack.pop()
        self.set_to_parent()
 
    def visit_literal(self, node):
        self.text_stack.append('pitch:fixed')
        #ipshell('At visit_literal')
        sname = generate_text_style(self, self.text_stack)
        el = odt.SubElement(self.current_element, 'text:span',
            attrib={'text:style-name': self.rststyle(sname)})
        self.set_current_element(el)

    def depart_literal(self, node):
        self.text_stack.pop()
        self.set_to_parent()
   
    
    def visit_emphasis(self, node):
        self.text_stack.append('bold')
        if not self._in_p():
            self.current_element = self.append_p('emphasis')
        
        sname = generate_text_style(self, self.text_stack)

        ##odt.ODFTranslator.visit_emphasis(self, node, stylename=sname) 
        el = odt.SubElement(self.current_element, 'text:span',
                        attrib={'text:style-name': self.rststyle(sname)})
        self.set_current_element(el)

    def depart_emphasis(self, node):
        self.text_stack.pop()
        odt.ODFTranslator.depart_emphasis(self, node)

    def visit_Text(self, node):
        if self.in_comment:
            return

        if not self._in_p():
            self.current_element = self.append_p("default")

        prev_current = self.current_element
        if self.in_handout:
            self.create_handout_text()
            self.current_element = self.handout_text

        odt.ODFTranslator.visit_Text(self, node)
        
        if self.in_handout:
            self.append_child('text:line-break')

        self.current_element = prev_current

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

    

    def create_handout_text(self):
        if not self.handout_text is None:
            return

        preso_notes = self.append_child('presentation:notes',
                                        attrib={'draw:style-name':'dp2'},
                                        parent=self.page)
        thumbnail = self.append_child('draw:page-thumbnail',
                                      attrib={'draw:style-name':'gr1',
                                              'draw:layer':'layout',
                                              'svg:width':'13.968cm',
                                              'svg:height':'10.476cm',
                                              'svg:x':'3.81cm',
                                              'svg:y':'2.123cm',
                                              'draw:page-number':str(self.section_count),
                                              'presentation:class':'page'},
                                      
                                      parent=preso_notes)
        frame = self.append_child('draw:frame',
                                  attrib={'presentation:style-name':'pr6',
                                          'draw:text-style-name':'P5',
                                          'draw:layer':'layout',
                                          'svg:width':'17.271cm',
                                          'svg:height':'12.322cm',
                                          'svg:x':'2.159cm',
                                          'svg:y':'13.271cm',
                                          'presentation:class':'notes',
                                          'presentation:placeholde':'true'},
                                  parent=preso_notes)
        text_box = self.append_child('draw:text-box', parent=frame)
        p = self.append_child('text:p', parent=text_box)
        self.handout_text = p
        


    def set_to_parent(self, node=None):
        # setting to parent can be bad for presos
        if self.text_box and self.text_box == self.current_element:
            return
        odt.ODFTranslator.set_to_parent(self)


    def visit_date(self, node):
        """!!! odt doesn't work, duplicates date"""
        el = self.generate_labeled_block(node, 'Date: ')
        self.set_current_element(el)

    def depart_date(self, node):
        self.set_to_parent()


    def visit_docinfo(self, node):
        """!!! crashes OOo"""
        pass
    def depart_docinfo(self, node):
        """!!! crashes OOo"""
        pass

    def visit_comment(self, node):
        self.in_comment = True

    def depart_comment(self, node):
        self.in_comment = False

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

    def _add_style(self, name, parent_attr, child_tag=None,child_attr=None):
        if name not in self.styles_seen:
            self.styles_seen[name] = 1
            e1 = odt.SubElement(self.automatic_styles, 'style:style',
                                parent_attr)
            if child_tag:
                e1_1 = odt.SubElement(e1, child_tag,
                                      child_attr)


def flatten_stack(stack):
    results = "_".join([x for x in stack])
    results.replace(" ", "_")
    return results


def generate_paragraph_style(translator, para_stack):
    """
    para_stack may include:
    * left, center, right
    * block_indent

    """
    para_name = flatten_stack(para_stack)
    para_attrs = {}
    for items in para_stack:
        for item in items.split():
            if item == 'left':
                para_attrs['fo:text-align'] = 'start'
            elif item == 'center':
                para_attrs['fo:text-align'] = 'center'
            elif item == 'right':
                para_attrs['fo:text-align'] = 'end'
            elif item == 'block_indent':
                para_attrs['fo:margin-left'] = '1.2cm'
                para_attrs['fo:margin-right'] = '-0.9cm'

    if para_name not in translator.styles_seen:
        para_style = translator._add_style(para_name,
                                           {'style:name':translator.rststyle(para_name),
                                            'style:family':'paragraph'},
                                           'style:paragraph-properties',
                                           para_attrs)
    return para_name

def generate_text_style(translator, text_stack):
    """
    given a translater, and text_stack (stack of text styles (can have
    multiple by space delimiting)), add the styles to the automatic
    sytles portion of the translator and return style_name
    
    Following are text_stack items:
    * bold
    * italic
    * normal
    * underline
    * font:name
    * pitch:fixed
    * outline
    * shadow
    * color:color_name
    * bgcolor:color_name (#cc32f5 or `transparent`)
    * size:[huge|big|small|tiny]
    """
    color2hex = dict(black="black",
                     gray="gray")
    text_name = flatten_stack(text_stack)
    text_attrs = {}
    for items in text_stack:
        # go in the order of items in the stack applying all of them
        # if later items override previous ones, that's ok
        for item in items.split():
            if item == 'bold':
                text_attrs['fo:font-weight'] = 'bold'
                text_attrs['style:font-weight-asian'] = 'bold'
                text_attrs['style:font-weight-complex'] = 'bold'
            elif item == 'italic':
                text_attrs['style:font-weight-asian'] = 'italic'
            elif item == 'normal':
                text_attrs['fo:font-weight'] = 'normal'
                text_attrs['style:font-weight-asian'] = 'normal'
                text_attrs['style:font-weight-complex'] = 'normal'
            elif item.startswith('color:'):
                text_attrs['fo:color'] = item.split(':')[1]
            elif item.startswith('bgcolor:'):
                text_attrs['fo:background-color'] = item.split(':')[1]
            elif item == 'outline':
                text_attrs['style:text-outline'] = 'true'
            elif item == 'shadow':
                text_attrs['fo:text-shadow'] = '1pt 1pt'
            elif item.startswith('pitch:'):
                text_attrs['style:font-pitch'] = item.split(':')[1]
                text_attrs['fo:font-family'] = "'Courier New'"
            elif item.startswith('size:'):
                size = item.split(':')[1]
                if size == 'huge':
                    text_attrs['fo:font-size'] = '66pt'
                elif size == 'big':
                    text_attrs['fo:font-size'] = '44pt'
                elif size == 'normal':
                    text_attrs['fo:font-size'] = '18pt'
                elif size == 'small':
                    text_attrs['fo:font-size'] = '12pt'
                elif size == 'tiny':
                    text_attrs['fo:font-size'] = '8pt'
            
            
    if text_name not in translator.styles_seen:
        text_style = translator._add_style(text_name, 
                                           {'style:name':translator.rststyle(text_name),
                                            'style:family':'text'},
                                           'style:text-properties',
                                           text_attrs)

    return text_name
