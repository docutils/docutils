#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Object oriented lib to Open Office Presentations

Copyright 2008 Matt Harrison
Licensed under Apache License, Version 2.0 (current)
"""

import copy
import cStringIO as sio
import xml.etree.ElementTree as et
from xml.dom import minidom
import os
import sys
import tempfile

import pygments
from pygments import formatter, lexers
import zipwrap
import Image

DOC_CONTENT_ATTRIB = {
    'office:version': '1.0',
    'xmlns:anim':'urn:oasis:names:tc:opendocument:xmlns:animation:1.0',
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
    'xmlns:presentation': 'urn:oasis:names:tc:opendocument:xmlns:presentation:1.0',
    'xmlns:ooo': 'http://openoffice.org/2004/office',
    'xmlns:oooc': 'http://openoffice.org/2004/calc',
    'xmlns:ooow': 'http://openoffice.org/2004/writer',
    'xmlns:script': 'urn:oasis:names:tc:opendocument:xmlns:script:1.0',
    'xmlns:smil':'urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0',
    'xmlns:style': 'urn:oasis:names:tc:opendocument:xmlns:style:1.0',
    'xmlns:svg': 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0',
    'xmlns:table': 'urn:oasis:names:tc:opendocument:xmlns:table:1.0',
    'xmlns:text': 'urn:oasis:names:tc:opendocument:xmlns:text:1.0',
    'xmlns:xforms': 'http://www.w3.org/2002/xforms',
    'xmlns:xlink': 'http://www.w3.org/1999/xlink',
    'xmlns:xsd': 'http://www.w3.org/2001/XMLSchema',
    'xmlns:xsi': 'http://www.w3.org/2001/XMLSchema-instance',
    }

NS2PREFIX = {}
for key, value in DOC_CONTENT_ATTRIB.items():
    NS2PREFIX[value] = key.split(':')[-1]


TEXT_COUNT = 100
DATA_DIR =  os.path.join(os.path.dirname(__file__), 'data')


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

class PrefixedWriter(et.ElementTree):
    """ hacked to pass NS2PREFIX to _write """
    def write(self, file, encoding="us-ascii"):
        assert self._root is not None
        if not hasattr(file, "write"):
            file = open(file, "wb")
        if not encoding:
            encoding = "us-ascii"
        elif encoding != "utf-8" and encoding != "us-ascii":
            file.write("<?xml version='1.0' encoding='%s'?>\n" % encoding)
        print "WRITING",  self._root
        self._write(file, self._root, encoding, NS2PREFIX)
        #self._write(file, self._root, encoding, {})

# Wrap etree elements to add parent attribute
def el(tag, attrib=None):
    attrib = attrib or {}
    el = et.Element(tag, attrib)
    el.parent = None
    return el

def sub_el(parent, tag, attrib=None):
    attrib = attrib or {}
    el = et.SubElement(parent, tag, attrib)
    el.parent = parent
    return el

def to_xml(node):
    """ convert an etree node to xml """
    fout = sio.StringIO()
    etree = PrefixedWriter(node)
    etree.write(fout)
    xml = fout.getvalue()
    return xml

def pretty_xml(string_input):
    """ pretty indent string_input """
    doc = minidom.parseString(string_input)
    s1 = doc.toprettyxml('  ')
    return s1


class Preso(object):
    mime_type = 'application/vnd.oasis.opendocument.presentation'

    def __init__(self):
        self.slides = []
        self._pictures = [] # list of Picture instances
        self._footer_count = 0
        # xml elements 
        self._root = None
        self._auto_styles = None
        self._presentation = None

        self._styles_added = {}

        self._init_xml()
        
    def _init_xml(self):
        self._root = el('office:document-content', attrib=DOC_CONTENT_ATTRIB)
        o_scripts = sub_el(self._root, 'office:scripts')
        self._auto_styles = sub_el(self._root, 'office:automatic-styles')
        o_body = sub_el(self._root, 'office:body')
        self._presentation = sub_el(o_body, 'office:presentation')

    def get_data(self, style_file=None):
        fd, filename = tempfile.mkstemp()
        zip_odp = self.to_file()
        if style_file:
            self.add_otp_style(zip_odp, style_file)
        zip_odp.zipit(filename)
        data = open(filename).read()
        os.remove(filename)
        return data

    def add_otp_style(self, zip_odp, style_file):
        """
        takes the slide content and merges in the style_file
        """
        style = zipwrap.ZipWrap(style_file)
        if 'Pictures' in style.cat(''):
            for p in style.cat('Pictures'):
                picture_file = 'Pictures/'+p 
                zip_odp.touch(picture_file, style.cat(picture_file))
        zip_odp.touch('styles.xml', style.cat('styles.xml'))
        return zip_odp
                   

    def to_file(self, filename=None):
        """
        >>> p = Preso()
        >>> z = p.to_file('/tmp/foo.odp')
        >>> z.cat('/')
        ['settings.xml', 'META-INF', 'styles.xml', 'meta.xml', 'content.xml', 'mimetype']
        """
        out = zipwrap.ZipWrap('')
        out.touch('mimetype', self.mime_type)
        for p in self._pictures:
            out.touch('Pictures/%s' % p.internal_name, p.get_data())
        out.touch('content.xml', self.to_xml())
        out.touch('styles.xml', self.styles_xml())
        out.touch('meta.xml', self.meta_xml())
        out.touch('settings.xml', self.settings_xml())
        out.touch('META-INF/manifest.xml', self.manifest_xml(out))
        if filename:
            out.zipit(filename)
        return out

    def manifest_xml(self, zip):
        content = """<?xml version="1.0" encoding="UTF-8"?>
<manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0">
 <manifest:file-entry manifest:media-type="application/vnd.oasis.opendocument.presentation" manifest:full-path="/"/>
"""
        for filename in zip.cat('/'):
            filetype = ''
            if filename.endswith('.xml'):
                filetype = 'text/xml'
            elif filename.endswith('.jpg'):
                filetype = 'image/jpeg'
            elif filename == 'Configurations2/':
                filetype = 'application/vnd.sun.xml.ui.configuration'
            
            content += """ <manifest:file-entry manifest:media-type="%s" manifest:full-path="%s"/> """ % (filetype, filename)

        content += """</manifest:manifest>"""
        return content
        

    def meta_xml(self):
        return """<?xml version="1.0" encoding="UTF-8"?>
<office:document-meta xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0" xmlns:presentation="urn:oasis:names:tc:opendocument:xmlns:presentation:1.0" xmlns:ooo="http://openoffice.org/2004/office" xmlns:smil="urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0" xmlns:anim="urn:oasis:names:tc:opendocument:xmlns:animation:1.0" office:version="1.1">
  <office:meta>
    <meta:generator>odplib(python)</meta:generator>
    <meta:creation-date>2008-09-15T11:12:02</meta:creation-date>
    <dc:date>2008-10-01T20:32:43</dc:date>
    <meta:editing-cycles>3</meta:editing-cycles>
    <meta:editing-duration>PT26M35S</meta:editing-duration>
    <meta:user-defined meta:name="Info 1"/>
    <meta:user-defined meta:name="Info 2"/>
    <meta:user-defined meta:name="Info 3"/>
    <meta:user-defined meta:name="Info 4"/>
    <meta:document-statistic meta:object-count="37"/>
  </office:meta>
</office:document-meta>
"""

    def settings_xml(self):
        filename = os.path.join(DATA_DIR, 'settings.xml')
        return open(filename).read()

    def styles_xml(self):
        filename = os.path.join(DATA_DIR, 'styles.xml')
        return open(filename).read()

    def to_xml(self):
        for i, slide in enumerate(self.slides):
            if slide.footer:
                footer_node = slide.footer.get_node()
                self._presentation.append(footer_node)
                footer_node.parent = self._presentation
            node = slide.get_node()
            self._presentation.append(node)
            node.parent = self._presentation
        return to_xml(self._root)

    def add_style(self, style):
        name = style.name
        node = style.style_node()
        if name not in self._styles_added:
            self._styles_added[name] = 1
            self._auto_styles.append(node)
            
        
    def add_slide(self):
        pnum = len(self.slides)+1
        s = Slide(self, page_number=pnum)
        self.slides.append(s)
        return s

    def copy_slide(self, s):
        new_s = s._copy()
        self.slides.append(new_s)
        return new_s

    def add_footer(self, f):
        f.name = 'ftr%d'%(self._footer_count)
        self._footer_count += 1
        self.slides[-1].footer = f
    

class Animation(object):
    ANIM_COUNT = 1
    def __init__(self):
        self.id = self._get_id()

    def _get_id(self):
        my_id = "id%d" % self.__class__.ANIM_COUNT
        self.__class__.ANIM_COUNT += 1
        return my_id

    def get_node(self):
        """   
	    <anim:par smil:begin="next">
	      <anim:par smil:begin="0s">
		<anim:par smil:begin="0s" smil:fill="hold" presentation:node-type="on-click" presentation:preset-class="entrance" presentation:preset-id="ooo-entrance-appear">
		  <anim:set smil:begin="0s" smil:dur="0.001s" smil:fill="hold" smil:targetElement="id1" anim:sub-item="text" smil:attributeName="visibility" smil:to="visible"/>
		</anim:par>
	      </anim:par>
	    </anim:par>
        """
        par = el('anim:par', attrib={'smil:begin':'next'})
        par2 = sub_el(par, 'anim:par', attrib={'smil:begin':'0s'})
        par3 = sub_el(par2, 'anim:par', attrib={'smil:begin':'0s',
                                                'smil:fill':'hold',
                                                'presentation:node-type':'on-click',
                                                'presentation:preset-class':'entrance',
                                                'presentation:preset-id':'ooo-entrance-appear'})
        anim_set = sub_el(par3, 'anim:set', attrib={'smil:begin':'0s',
                                                    'smil:dur':'0.001s',
                                                    'smil:fill':'hold',
                                                    'smil:targetElement':self.id,
                                                    'anim:sub-item':'text',
                                                    'smil:attributeName':'visibility',
                                                    'smil:to':'visible'})
        return par

class Picture(object):
    """
    Need to convert to use image scale::
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
    """
    COUNT = 0
    CM_SCALE = 30.

    def __init__(self, filepath):
        self.filepath = filepath
        image = Image.open(filepath)
        self.w, self.h = image.size
        self.internal_name = self._gen_name()

    def _gen_name(self):
        ext = os.path.splitext(self.filepath)[1]
        name = str(Picture.COUNT) + ext
        Picture.COUNT += 1
        return name
    
    def get_width(self, measurement):
        if measurement == 'cm':
            scale = Picture.CM_SCALE
        return str(self.w/scale)+measurement

    def get_height(self, measurement):
        if measurement == 'cm':
            scale = Picture.CM_SCALE
        return str(self.h/scale)+measurement



    def get_data(self):
        return open(self.filepath).read()

class Slide(object):
    def __init__(self, preso, page_number=None):
        self.title_frame = None
        self.text_frames = []
        self._cur_text_frame = None
        self.pic_frame = None
        self._preso = preso
        self.footer_frame = None
        self.notes_frame = None
        self.page_number = page_number
        self.bullet_list = None # current bullet list
        self.footer = None
        self.animations = []
        self.paragraph_attribs = {} # used to mark id's for animations
        self.page_number_listeners = [self]

        self.element_stack = [] # allow us to push pop
        self.cur_element = None # if we write it could be to title,
                                # text or notes (Subclass of
                                # MixedContent)

        # xml elements
        self._page = None

        self._init_xml()

    def start_animation(self, anim):
        self.animations.append(anim)
        self.paragraph_attribs['text:id'] = anim.id

    def end_animation(self):
        # jump out of text:p
        self.parent_of('text:p')
        del self.paragraph_attribs['text:id']

    def push_style(self, style):
        if self.cur_element is None:
            self.add_text_frame()
        self.cur_element.pending_styles.append(style)
    
    def pop_style(self):
        self.cur_element.pending_styles.pop()

    def add_code(self, code, language):
        if self.cur_element is None:
            self.add_text_frame()
        style = ParagraphStyle(**{'fo:text-align':'start'})
        self.push_style(style)
        output = pygments.highlight(code, lexers.get_lexer_by_name(language, stripall=True),
                                    OdtCodeFormatter(self.cur_element, self._preso))
        self.pop_style()
        self.pop_node()

    def add_picture(self, p):
        """
        needs to look like this (under draw:page)

	<draw:frame draw:style-name="gr2" draw:text-style-name="P2" draw:layer="layout" svg:width="19.589cm" svg:height="13.402cm" svg:x="3.906cm" svg:y="4.378cm">
	  <draw:image xlink:href="Pictures/10000201000002F800000208188B22AE.png" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad">
	    <text:p text:style-name="P1"/>
	  </draw:image>
	</draw:frame>
        """
        # pictures should be added the the draw:frame element
        self.pic_frame = PictureFrame(self, p)
        self.pic_frame.add_node('draw:image', attrib={'xlink:href':os.path.join('Pictures', p.internal_name),
                                                      'xlink:type':'simple',
                                                      'xlink:show':'embed',
                                                      'xlink:actuate':'onLoad' })
        self._preso._pictures.append(p)
        node =  self.pic_frame.get_node()
        self._page.append(node)
        node.parent = self._page

    def push_element(self):
        self.element_stack.append(self.cur_element)

    def pop_element(self):
        self.cur_element = self.element_stack.pop()
        
    def to_xml(self):
        node = self.get_node()
        return to_xml(node)

    def _fire_page_number(self, new_num):
        for listener in self.page_number_listeners:
            listener.new_page_num(new_num)

    def new_page_num(self, new_num):
        self._page.attrib['draw:name'] = 'page%d' % self.page_number

    def _copy(self):
        ''' needs to update page numbers '''
        ins = copy.copy(self)
        ins._fire_page_number(self.page_number+1)
        return ins
    
    def _init_xml(self):
        self._page = el('draw:page', attrib={
                'draw:name':'page%d' % self.page_number,
                'draw:style-name':'dp1',
                'draw:master-page-name':'Default',
                'presentation:presentation-page-layout-name':'AL1T0'
                })
        office_forms = sub_el(self._page, 'office:forms',
                              attrib={'form:automatic-focus':'false',
                                      'form:apply-design-mode':'false'})

    def get_node(self):
        """return etree Element representing this slide"""
        # already added title, text frames
        # add animation chunks
        if self.animations:
            anim_par = el('anim:par', attrib={'presentation:node-type':'timing-root'})
            self._page.append(anim_par)
            anim_par.parent = self._page
            anim_seq = sub_el(anim_par, 'anim:seq', attrib={'presentation:node-type':'main-sequence'})
            for a in self.animations:
                a_node = a.get_node()
                anim_seq.append(a_node)
                a_node.parent = anim_seq

        # add notes now (so they are last)
        if self.notes_frame:
            notes = self.notes_frame.get_node()
            self._page.append(notes)
            notes.parent = self._page
        if self.footer:
            self._page.attrib['presentation:use-footer-name'] = self.footer.name
        return self._page
        
    def add_text_frame(self):
        self.text_frames.append(TextFrame(self))
        node = self.text_frames[-1].get_node()
        self._page.append(node)
        node.parent = self._page
        self.cur_element = self.text_frames[-1]
        return self.text_frames[-1]

    def add_title_frame(self):
        self.title_frame = TitleFrame(self)
        node = self.title_frame.get_node()
        self._page.append(node)
        node.parent = self._page
        self.cur_element = self.title_frame
        return self.title_frame

    def add_notes_frame(self):
        self.notes_frame = NotesFrame(self)
        self.page_number_listeners.append(self.notes_frame)
        self.cur_element = self.notes_frame
        return self.notes_frame

    def add_bullet_list(self, bl):
        """
        note that this pushes the cur_element, but doesn't pop it.
        You'll need to do that
        """
        # text:list doesn't like being a child of text:p
        if self.cur_element is None:
            self.add_text_frame()
        self.push_element()
        self.cur_element.node.attrib['presentation:class'] = 'outline'
        self.cur_element._text_box.append(bl.node)
        bl.node.parent = self.cur_element._text_box
        if 'default-list' not in self._preso._styles_added:
            self._preso._styles_added['default-list'] = 1
            self._preso._auto_styles.append(et.fromstring(bl.default_styles())[0])
        self.cur_element = bl

            
    def write(self, text, **kw):
        if self.cur_element is None:
            self.add_text_frame()
        self.cur_element.write(text, **kw)

    def add_node(self, node, attrib):
        if self.cur_element is None:
            self.add_text_frame()
        self.cur_element.add_node(node, attrib)

    def add_p(self):
        self.cur_element.add_p()

    def pop_node(self):
        self.cur_element.pop_node()

    def parent_of(self, name):
        """ 
        like pop_node, but traverse up parents.  When you find a node
        with name, set cur_node to that 
        """
        if self.cur_element:
            self.cur_element.parent_of(name)

class MixedContent(object):
    def __init__(self, slide, name, attrib=None):
        self.slide = slide
        if attrib is None:
            attrib = {}
        self.node = el(name, attrib)
        self.cur_node = self.node
        self.pending_styles = []

    def parent_of(self, name):
        """ 
        go to parent of node with name, and set as cur_node.  Useful
        for creating new paragraphs
        """
        if not self._in_tag(name):
            return
        node = self.cur_node
        while node.tag != name:
            node = node.parent
        self.cur_node = node.parent

    def _in_p(self):
        """
        Determine if we are already in a text:p, odp doesn't like
        nested ones too much
        """
        return self._in_tag('text:p')

    def _is_node(self, tagname, attributes=None, node=None):
        node = node or self.cur_node
        if attributes:
            return node.tag == tagname and node.attrib == attributes
        else:
            return node.tag == tagname

    def _is_parent(self, tagname, attributes=None):
        return self._is_node(tagname, attributes, self.cur_node.parent)

    def _in_tag(self, tagname, attributes=None):
        """
        Determine if we are already in a certain tag.
        If we give attributes, make sure they match.
        """
        node = self.cur_node
        while not node is None:
            if node.tag == tagname:
                if attributes and node.attrib == attributes:
                    return True
                elif attributes:
                    return False
                return True
            node = node.parent
        return False

    def to_xml(self):
        return to_xml(self.node)

    def get_node(self):
        return self.node

    def append(self, node):
        self.cur_node.append(node)
        node.parent = self.cur_node
    
    def _add_node(self, parent, name, attrib):
        new_node = sub_el(parent, name, attrib)
        return new_node
        
    def add_node(self, node_name, attrib=None):
        if attrib is None:
            attrib = {}
        new_node = self._add_node(self.cur_node, node_name, attrib)
        self.cur_node = new_node

    def pop_node(self):
        if self.cur_node.parent == self.node:
            print "CAN'T POP"
            return
        if self.cur_node.parent is None:
            return
        self.cur_node = self.cur_node.parent

    def _add_styles(self, add_paragraph=True, add_text=True):
        p_styles = {'fo:text-align':self._default_align}
        t_styles = {}
        for s in self.pending_styles:
            if isinstance(s, ParagraphStyle):
                p_styles.update(s.styles)
            elif isinstance(s, TextStyle):
                t_styles.update(s.styles)
                
        para = ParagraphStyle(**p_styles)     

        if add_paragraph or self.slide.paragraph_attribs:
            p_attrib = {'text:style-name':para.name}
            p_attrib.update(self.slide.paragraph_attribs)
            if not self._in_tag('text:p', p_attrib):
                self.parent_of('text:p')
                # Create paragraph style first
                self.slide._preso.add_style(para)
                self.add_node('text:p', attrib=p_attrib)

        # span is only necessary if style changes
        if add_text:
            if t_styles:
                text = TextStyle(**t_styles)
                if not self._is_node('text:span', {'text:style-name':text.name}):
                    # Create text style
                    self.slide._preso.add_style(text)
                    self.add_node('text:span', attrib={'text:style-name':text.name})

    def write(self, text, add_p_style=True, add_t_style=True):
        """
        see mixed content
        http://effbot.org/zone/element-infoset.htm#mixed-content
        Writing is complicated by requirements of odp to ignore
        duplicate spaces together.  Deal with this by splitting on
        white spaces then dealing with the '' (empty strings) which
        would be the extra spaces
        """

        self._add_styles(add_p_style, add_t_style)

        seen_space = False
        for chunk in text.split(' '): # deal with white space 
            # see  http://books.evc-cit.info/odbook/ch03.html#whitespace-section 
            if chunk == '' and not seen_space:
                seen_space = True
                continue
                # skip the first space
            elif chunk == '':
                self.add_node('text:s', {})
                self.pop_node()
                continue
            else:
                seen_space = False

            children = self.cur_node.getchildren()
            if self.cur_node.text and children:
                child = children[-1]
                if child.tail:
                    chunk = ' ' + chunk
                cur_text = child.tail or ''
                child.tail = cur_text + chunk
            else:    
                if self.cur_node.text:
                    # add a space
                    chunk = ' ' + chunk
                cur_text = self.cur_node.text or ''
                self.cur_node.text = cur_text + chunk


class Footer(MixedContent):
    def __init__(self, slide):
        self._default_align = 'center'
        MixedContent.__init__(self, slide, 'presentation:footer-decl')
        self.name = None

    def get_node(self):
        if self.name is None:
            raise Exception("set footer name")
        self.node.attrib['presentation:name'] = self.name
        return self.node
                                                      
        

class PictureFrame(MixedContent):
    def __init__(self, slide, picture, attrib=None):
        attrib = attrib or {
            'presentation:style-name':'pr2',
            'draw:style-name':'gr2',
            'draw:layer':'layout',
            'svg:width':picture.get_width('cm'),
            'svg:height':picture.get_height('cm'),
            'svg:x':'1.4cm',
            'svg:y':'4.577cm',
            }
        MixedContent.__init__(self, slide, 'draw:frame', attrib=attrib)
        

class TextFrame(MixedContent):
    def __init__(self, slide, attrib=None):
        attrib = attrib or {
            'presentation:style-name':'pr2',
            'draw:layer':'layout',
            'svg:width':'25.199cm',
            'svg:height':'13.86cm',
            'svg:x':'1.4cm',
            'svg:y':'4.577cm',
            'presentation:class':'subtitle'
            }

        MixedContent.__init__(self, slide,  'draw:frame', attrib=attrib)
        self._text_box = sub_el(self.node, 'draw:text-box')
        self.cur_node = self._text_box
        self.text_styles = ['P1']

        self.cur_node = self._text_box
        self._default_align = 'start'

    def to_xml(self):
        return to_xml(self.get_node())
    
    def _in_bullet(self):
        return self._in_tag('text:list')

    def add_p(self):
        if not self._in_p():
            style = ParagraphStyle(**{'fo:text-align':self._default_align,
                                      'fo:margin-left':'1.2cm',
                                      'fo:margin-right':'-.9cm',})
            self.slide.push_style(style)


    def generate_style(self):
        ''' return the current text style name and add any automatic styles
        needed (not generated yet)'''
        return self.text_styles[-1]
        
class TitleFrame(TextFrame):
    def __init__(self, slide, attrib=None):
        attrib = attrib or {
            'presentation:style-name':'Default-title',
            'draw:layer':'layout',
            'svg:width':'25.199cm',
            'svg:height':'1.737cm',
            'svg:x':'1.4cm',
            'svg:y':'1.721cm',
            'presentation:class':'title'
            }

        TextFrame.__init__(self, slide, attrib)
        self._default_align = 'center'        
        
class NotesFrame(TextFrame):
    def __init__(self, slide, attrib=None):
        attrib = attrib or {
            'presentation:style-name':'pr1',
            'draw:layer':'layout',
            'svg:width':'17.271cm',
            'svg:height':'12.322cm',
            'svg:x':'2.159cm',
            'svg:y':'13.271cm',
            'presentation:class':'notes',
            'presentation:placeholder':'true'
            }
        TextFrame.__init__(self, slide, attrib)
        self._preso_notes = el('presentation:notes', attrib={'draw:style-name':'dp2'})
        self._page_thumbnail = sub_el(self._preso_notes, 
                                      'draw:page-thumbnail', 
                                      attrib={
                'presentation:style-name':'gr1',
                'draw:layer':'layout',
                'svg:width':'13.968cm',
                'svg:height':'10.476cm',
                'svg:x':'3.81cm',
                'svg:y':'2.123cm',
                'draw:page-number':'%d'%slide.page_number,
                'presentation:class':'page'})
        self._preso_notes.append(self.node)
        self.node.parent = self._preso_notes
        
    def new_page_num(self, new_num):
        self._page_thumbnail.attrib['draw:page-number']='%d'%new_num

    def get_node(self):
        return self._preso_notes
        

class TextStyle(object):
    """
    based on 
    http://books.evc-cit.info/odbook/ch03.html#char-para-styling-section
    """
    font_weight = dict(
        BOLD = 'bold',
        NORMAL = 'normal'
        )
    font_style = dict(
        ITALIC = 'italic',
        NORMAL = 'normal'
        )
    text_underline_style = dict(
        NONE = 'none',
        SOLID = 'solid',
        DOTTED = 'dotted',
        DASH = 'dash',
        LONG_DASH = 'long-dash',
        DOT_DASH = 'dot-dash',
        DOT_DOT_DASH = 'dot-dot-dash',
        WAVE = 'wave'
        )
    text_underline_type = dict(
        NONE = 'none',
        SINGLE = 'single', #default
        DOUBLE = 'double'
        )
    text_underline_width = dict(
        AUTO = 'auto',
        NORMAL = 'normal',
        BOLD = 'bold',
        THIN = 'thin',
        DASH = 'dash',
        MEDIUM = 'medium',
        THICK = 'thick'
        )
    text_underline_mode = dict(
        SKIP_WHITE_SPACE = 'skip-white-space'
        )
    font_variant = dict(
        NORMAL = 'normal',
        SMALL_CAPS = 'small-caps'
        )
    text_transform = dict(
        NONE = 'none',
        LOWERCASE = 'lowercase',
        UPPERCASE = 'uppercase',
        CAPITALIZE = 'capitalize',
        SMALL_CAPS = 'small-caps'
        )
    text_outline = dict(
        TRUE = 'true'
        )
    text_rotation_angle = dict(
        ZERO = '0',
        NINETY = '90',
        TWOSEVENTY = '270'
        )
    text_rotation_scale = dict(
        LINE_HEIGHT = 'line-height',
        FIXED = 'fixed'
        )
    
    FAMILY = 'text'
    STYLE_PROP = 'style:text-properties'
    PREFIX = 'T%d'
    ATTRIB2NAME = {}
    TEXT_COUNT = 0
    def __init__(self, **kw):
        '''
        pass in a dictionary containing the style attributes you want for your text
        '''
        self.styles = kw
        self.name = self._gen_name()

    def _gen_name(self):
        key = self.styles.items()
        key.sort()
        key = tuple(key)
        if key in self.__class__.ATTRIB2NAME:
            return self.__class__.ATTRIB2NAME[key]
        else:
            name = self.PREFIX % self.__class__.TEXT_COUNT
            self.__class__.TEXT_COUNT += 1
            self.__class__.ATTRIB2NAME[key] = name
            return name

    def style_node(self, additional_style_attrib=None):
        """
        generate a style node (for automatic-styles)
        
        could specify additional attributes such as
        'style:parent-style-name' or 'style:list-style-name'
       
        """
        style_attrib = {'style:name':self.name,
                        'style:family':self.FAMILY}
        if additional_style_attrib:
            style_attrib.update(additional_style_attrib)

        node = el('style:style', attrib=style_attrib)
        props = sub_el(node, self.STYLE_PROP,
                            attrib=self.styles)
        return node


class ParagraphStyle(TextStyle):
    text_align = dict(
        START = 'start',
        END = 'end',
        CENTER = 'center',
        JUSTIFY = 'justify'
        )
        
    FAMILY = 'paragraph'
    STYLE_PROP = 'style:paragraph-properties'
    PREFIX = 'P%d'


class OdtCodeFormatter(formatter.Formatter):  
    def __init__(self, writable, preso):
        formatter.Formatter.__init__(self)
        self.writable = writable
        self.preso = preso

    def format(self, source, outfile):
        tclass = pygments.token.Token
        for ttype, value in source:
            style_attrib = self.get_style(ttype)
            tstyle = TextStyle(**style_attrib)
            self.writable.slide.push_style(tstyle)
            parts = value.split('\n')
            for part in parts[:-1]:
                self.writable.write(part)
                self.writable.add_node('text:line-break', {})
                self.writable.pop_node()
            self.writable.write(parts[-1])
            self.writable.slide.pop_style()
            self.writable.pop_node()
            
    def get_style(self, tokentype):
        while not self.style.styles_token(tokentype):
            tokentype = tokentype.parent
        value = self.style.style_for_token(tokentype)
        # default to monospace
        results = {
            'fo:font-family':"Courier New",
            'style:font-family-generic':"swiss",
            'style:font-pitch':"fixed"}
        if value['color']:
            results['fo:color'] = '#' + value['color']
        if value['bold']:
            results['fo:font-weight'] = 'bold'
        if value['italic']:
            results['fo:font-weight'] = 'italic'
        return results
          

class OutlineList(MixedContent):
    """
    see the following for lists
    http://books.evc-cit.info/odbook/ch03.html#list-spec-fig

    >>> o = OutlineList()
    >>> o.new_item('dogs')
    >>> o.indent()
    >>> o.new_item('small')
    >>> o.indent()
    >>> o.new_item('weiner')
    >>> o.write(' - more junk about German dogs')
    >>> o.new_item('fido')
    >>> o.dedent()
    >>> o.dedent()
    >>> o.new_item('cats')
    >>> o.to_xml()
    '<text:list text:style-name="L1"><text:list_item><text:p text:style-name="P1">dogs</text:p><text:list><text:list_item><text:p text:style-name="P1">small</text:p><text:list><text:list_item><text:p text:style-name="P1">weiner</text:p><text:p text:style-name="P1"> - more junk about German dogs</text:p></text:list_item><text:list_item><text:p text:style-name="P1">fido</text:p></text:list_item></text:list></text:list_item><text:list_item><text:p text:style-name="P1">cats</text:p></text:list_item></text:list></text:list_item></text:list>'

    See also:
http://books.evc-cit.info/odbook/ch03.html#bulleted-numbered-lists-section

    Bonafide OOo output looks like this:
      
 <draw:text-box>
	    <text:list text:style-name="L2">
	      <text:list-item>
		<text:p text:style-name="P1">Foo</text:p>
	      </text:list-item>
	      <text:list-item>
		<text:p text:style-name="P1">Bar</text:p>
	      </text:list-item>
	      <text:list-item> <!-- Important for indents!!! -->
		<text:list>
		  <text:list-item>
		    <text:p text:style-name="P1">barbie</text:p>
		  </text:list-item>
		  <text:list-item>
		    <text:p text:style-name="P1">ken</text:p>
		  </text:list-item>
		</text:list>
	      </text:list-item>
	      <text:list-item>
		<text:p text:style-name="P1">Baz</text:p>
	      </text:list-item>
	    </text:list>
	  </draw:text-box>
    """
    def __init__(self, slide):
        self._default_align = 'start'
        MixedContent.__init__(self, slide, 'text:list', attrib={'text:style-name':'L2'})
        self.parents = [self.node]
        self.level = 0

    def new_item(self, text=None):
        li = self._add_node(self.parents[-1], 'text:list-item', {})
        self.cur_node = li
        if text:
            self.write(text)

    def indent(self):
        self.level += 1
        li = self._add_node(self.parents[-1], 'text:list-item', {})
        l = self._add_node(li, 'text:list', {})
        self.cur_node = l
        self.parents.append(self.cur_node)

    def dedent(self):
        self.level -= 1
        self.parents.pop()
        self.cur_node = self.parents[-1]

    def default_styles(self):
        filename =  os.path.join(DATA_DIR, 'auto_list.xml')
        return open(filename).read()

def _test():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    _test()
        
