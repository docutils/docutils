
"""
Open Document Format (ODF) Writer.

"""

VERSION = '1.0a'

__docformat__ = 'reStructuredText'


import sys
import os
import os.path
import tempfile
import zipfile
from xml.dom import minidom
import codecs
import time
import re
from types import ListType
try:
    import Image                        # check for the Python Imaging Library
except ImportError:
    Image = None
import docutils
from docutils import frontend, nodes, utils, writers, languages


ElementTree = None
try:
    from lxml import etree
    #print '*** using lxml'
except ImportError, e:
    try:
        from elementtree import ElementTree as etree
        #print '*** using ElementTree'
    except ImportError, e:
        print '***'
        print '*** Error: Must install either ElementTree or lxml.'
        print '***'
        raise


from IPython.Shell import IPShellEmbed
args = ['-pdb', '-pi1', 'In <\\#>: ', '-pi2', '   .\\D.: ',
    '-po', 'Out<\\#>: ', '-nosep']
ipshell = IPShellEmbed(args,
    banner = 'Entering IPython.  Press Ctrl-D to exit.',
    exit_msg = 'Leaving Interpreter, back to program.')


#
# Constants and globals

# Turn tracing on/off.  See methods trace_visit_node/trace_depart_node.
DEBUG = 0
SPACES_PATTERN = re.compile(r'( +)')
TABS_PATTERN = re.compile(r'(\t+)')

TableStylePrefix = 'rststyle-Table'

GENERATOR_DESC = 'Docutils.org/odtwriter'


CONTENT_NAMESPACE_DICT = {
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

STYLES_NAMESPACE_DICT = {
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
    'xmlns:script': 'urn:oasis:names:tc:opendocument:xmlns:script:1.0',
    'xmlns:style': 'urn:oasis:names:tc:opendocument:xmlns:style:1.0',
    'xmlns:svg': 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0',
    'xmlns:table': 'urn:oasis:names:tc:opendocument:xmlns:table:1.0',
    'xmlns:text': 'urn:oasis:names:tc:opendocument:xmlns:text:1.0',
    'xmlns:xlink': 'http://www.w3.org/1999/xlink',
    }

MANIFEST_NAMESPACE_DICT = {
    'xmlns:manifest': 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0',
}

META_NAMESPACE_DICT = {
    'office:version': '1.0',
    'xmlns:dc': 'http://purl.org/dc/elements/1.1/',
    'xmlns:meta': 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0',
    'xmlns:office': 'urn:oasis:names:tc:opendocument:xmlns:office:1.0',
    'xmlns:ooo': 'http://openoffice.org/2004/office',
    'xmlns:xlink': 'http://www.w3.org/1999/xlink',
}

MIME_TYPE = 'application/vnd.oasis.opendocument.text'


class Writer(writers.Writer):

    supported = ('html', 'html4css1', 'xhtml')
    """Formats this writer supports."""

    default_stylesheet = 'styles.odt'

    default_stylesheet_path = utils.relative_path(
        os.path.join(os.getcwd(), 'dummy'),
        os.path.join(os.path.dirname(__file__), default_stylesheet))

    default_template = 'template.txt'

    default_template_path = utils.relative_path(
        os.path.join(os.getcwd(), 'dummy'),
        os.path.join(os.path.dirname(__file__), default_template))

##    settings_spec = (
##        'ODF-Specific Options',
##        None,
##        (('Specify the template file (UTF-8 encoded).  Default is "%s".'
##          % default_template_path,
##          ['--template'],
##          {'default': default_template_path, 'metavar': '<file>'}),
##        ('Specify a stylesheet URL, used verbatim.  Overrides '
##          '--stylesheet-path.',
##          ['--stylesheet'],
##          {'metavar': '<URL>', 'overrides': 'stylesheet_path'}),
##         ('Specify a stylesheet file, relative to the current working '
##          'directory.  The path is adjusted relative to the output ODF '
##          'file.  Overrides --stylesheet.  Default: "%s"'
##          % default_stylesheet_path,
##          ['--stylesheet-path'],
##          {'metavar': '<file>', 'overrides': 'stylesheet',
##           'default': default_stylesheet_path}),
##         ('Specify the initial header level.  Default is 1 for "<h1>".  '
##          'Does not affect document title & subtitle (see --no-doc-title).',
##          ['--initial-header-level'],
##          {'choices': '1 2 3 4 5 6'.split(), 'default': '1',
##           'metavar': '<level>'}),
##         ('Specify the maximum width (in characters) for one-column field '
##          'names.  Longer field names will span an entire row of the table '
##          'used to render the field list.  Default is 14 characters.  '
##          'Use 0 for "no limit".',
##          ['--field-name-limit'],
##          {'default': 14, 'metavar': '<level>',
##           'validator': frontend.validate_nonnegative_int}),
##         ('Specify the maximum width (in characters) for options in option '
##          'lists.  Longer options will span an entire row of the table used '
##          'to render the option list.  Default is 14 characters.  '
##          'Use 0 for "no limit".',
##          ['--option-limit'],
##          {'default': 14, 'metavar': '<level>',
##           'validator': frontend.validate_nonnegative_int}),
##         ('Format for footnote references: one of "superscript" or '
##          '"brackets".  Default is "brackets".',
##          ['--footnote-references'],
##          {'choices': ['superscript', 'brackets'], 'default': 'brackets',
##           'metavar': '<format>',
##           'overrides': 'trim_footnote_reference_space'}),
##         ('Format for block quote attributions: one of "dash" (em-dash '
##          'prefix), "parentheses"/"parens", or "none".  Default is "dash".',
##          ['--attribution'],
##          {'choices': ['dash', 'parentheses', 'parens', 'none'],
##           'default': 'dash', 'metavar': '<format>'}),
##         ('Remove extra vertical whitespace between items of "simple" bullet '
##          'lists and enumerated lists.  Default: enabled.',
##          ['--compact-lists'],
##          {'default': 1, 'action': 'store_true',
##           'validator': frontend.validate_boolean}),
##         ('Disable compact simple bullet and enumerated lists.',
##          ['--no-compact-lists'],
##          {'dest': 'compact_lists', 'action': 'store_false'}),
##         ('Remove extra vertical whitespace between items of simple field '
##          'lists.  Default: enabled.',
##          ['--compact-field-lists'],
##          {'default': 1, 'action': 'store_true',
##           'validator': frontend.validate_boolean}),
##         ('Disable compact simple field lists.',
##          ['--no-compact-field-lists'],
##          {'dest': 'compact_field_lists', 'action': 'store_false'}),
##         ('Omit the XML declaration.  Use with caution.',
##          ['--no-xml-declaration'],
##          {'dest': 'xml_declaration', 'default': 1, 'action': 'store_false',
##           'validator': frontend.validate_boolean}),
##         ('Obfuscate email addresses to confuse harvesters while still '
##          'keeping email links usable with standards-compliant browsers.',
##          ['--cloak-email-addresses'],
##          {'action': 'store_true', 'validator': frontend.validate_boolean}),
##        ))

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
         ('Obfuscate email addresses to confuse harvesters while still '
          'keeping email links usable with standards-compliant browsers.',
          ['--cloak-email-addresses'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         ('Specify the thickness of table borders in thousands of a cm.  '
           'Default is 35.',
          ['--table-border-thickness'],
          {'default': 35,
           'validator': frontend.validate_nonnegative_int}),
        ))

    settings_defaults = {
        'output_encoding_error_handler': 'xmlcharrefreplace',
        }

    relative_path_settings = (
        'stylesheet_path',
        )

    config_section = 'opendocument odf writer'
    config_section_dependencies = (
        'writers',
        )

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = ODFTranslator

    def translate(self):
        #import pdb; pdb.set_trace()
        self.settings = self.document.settings
        self.visitor = self.translator_class(self.document)
        self.document.walkabout(self.visitor)
        self.assemble_my_parts()
        self.output = self.parts['whole']

    def assemble_my_parts(self):
        """Assemble the `self.parts` dictionary.  Extend in subclasses.
        """
        #ipshell('At assemble_parts')
        writers.Writer.assemble_parts(self)
        f = tempfile.NamedTemporaryFile()
        zfile = zipfile.ZipFile(f, 'w', zipfile.ZIP_DEFLATED)
        zinfo = zipfile.ZipInfo('content.xml')
        zfile.writestr(zinfo, self.visitor.content_astext())
        zfile.writestr('mimetype', MIME_TYPE)
        s1 = self.create_manifest()
        zinfo = zipfile.ZipInfo('META-INF/manifest.xml')
        zfile.writestr(zinfo, s1)
        s1 = self.create_meta()
        zinfo = zipfile.ZipInfo('meta.xml')
        zfile.writestr(zinfo, s1)
        s1 = self.get_stylesheet()
        zinfo = zipfile.ZipInfo('styles.xml')
        zfile.writestr(zinfo, s1)
        zfile.close()
        f.seek(0)
        whole = f.read()
        f.close()
        self.parts['whole'] = whole
        self.parts['encoding'] = self.document.settings.output_encoding
        self.parts['version'] = docutils.__version__

    def get_stylesheet(self):
        """Retrieve the stylesheet from either a .xml file or from
        a .odt (zip) file.  Return the content as a string.
        """
        stylespath = utils.get_stylesheet_reference(self.settings,
            os.path.join(os.getcwd(), 'dummy'))
        ext = os.path.splitext(stylespath)[1]
        if ext == '.xml':
            stylesfile = open(stylespath, 'r')
            s1 = stylesfile.read()
            stylesfile.close()
        elif ext == '.odt':
            zfile = zipfile.ZipFile(stylespath, 'r')
            s1 = zfile.read('styles.xml')
            zfile.close()
        else:
            raise RuntimeError, 'stylesheet path must be .odt or .xml file.'
        return s1

    def assemble_parts(self):
        pass

    def create_manifest(self):
        root = etree.Element('manifest:manifest',
            attrib=MANIFEST_NAMESPACE_DICT)
        doc = etree.ElementTree(root)
        etree.SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': 'application/vnd.oasis.opendocument.text',
            'manifest:full-path': '/',
            })
        etree.SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': 'text/xml',
            'manifest:full-path': 'content.xml',
            })
        etree.SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': 'text/xml',
            'manifest:full-path': 'styles.xml',
            })
        etree.SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': 'text/xml',
            'manifest:full-path': 'meta.xml',
            })
        s1 = etree.tostring(doc)
        doc = minidom.parseString(s1)
        s1 = doc.toprettyxml('  ')
        return s1

    def create_meta(self):
        root = etree.Element('office:document-meta', attrib=META_NAMESPACE_DICT)
        doc = etree.ElementTree(root)
        root = etree.SubElement(root, 'office:meta')
        el1 = etree.SubElement(root, 'meta:generator')
        el1.text = 'Docutils/rst2odf.py/%s' % (VERSION, )
        s1 = os.environ.get('USER', '')
        el1 = etree.SubElement(root, 'meta:initial-creator')
        el1.text = s1
        s2 = time.strftime('%Y-%m-%dT%H:%M:%S', time.localtime())
        el1 = etree.SubElement(root, 'meta:creation-date')
        el1.text = s2
        el1 = etree.SubElement(root, 'dc:creator')
        el1.text = s1
        el1 = etree.SubElement(root, 'dc:date')
        el1.text = s2
        el1 = etree.SubElement(root, 'dc:language')
        el1.text = 'en-US'
        el1 = etree.SubElement(root, 'meta:editing-cycles')
        el1.text = '1'
        el1 = etree.SubElement(root, 'meta:editing-duration')
        el1.text = 'PT00M01S'
        title = self.visitor.get_title()
        el1 = etree.SubElement(root, 'dc:title')
        if title:
            el1.text = title
        else:
            el1.text = '[no title]'
        s1 = etree.tostring(doc)
        #doc = minidom.parseString(s1)
        #s1 = doc.toprettyxml('  ')
        return s1

# class ODFTranslator(nodes.SparseNodeVisitor):

class ODFTranslator(nodes.GenericNodeVisitor):

    def __init__(self, document):
        #nodes.SparseNodeVisitor.__init__(self, document)
        nodes.GenericNodeVisitor.__init__(self, document)
        self.settings = document.settings
        self.section_level = 0
        self.section_count = 0
        # Create ElementTree content and styles documents.
        root = etree.Element(
            'office:document-content', CONTENT_NAMESPACE_DICT
            )
        self.content_tree = etree.ElementTree(element=root)
        self.current_element = root
        etree.SubElement(root, 'office:scripts')
        etree.SubElement(root, 'office:font-face-decls')
        el = etree.SubElement(root, 'office:automatic-styles')
        self.automatic_styles = el
        el = etree.SubElement(root, 'office:body')
        el = etree.SubElement(el, 'office:text')
        self.current_element = el
        root = etree.Element(
            'office:document-styles', STYLES_NAMESPACE_DICT
            )
        self.styles_tree = etree.ElementTree(element=root)
        self.paragraph_style_stack = ['rststyle-textbody', ]
        self.omit = False
        self.table_count = 0
        self.column_count = ord('A') - 1
        self.trace_level = -1
        self.optiontablestyles_generated = False
        self.footer_element = None
        self.field_name = None
        self.field_element = None
        self.title = None

    def astext(self):
        root = self.content_tree.getroot()
        s1 = etree.tostring(root)
        return s1

    def content_astext(self):
        return self.astext()

    def styles_astext(self):
        root = self.styles_tree.getroot()
        s1 = etree.tostring(root)
        return s1

    def set_title(self, title): self.title = title
    def get_title(self): return self.title

    #
    # Utility methods

    def append_child(self, tag, attrib=None):
        if attrib is None:
            el = etree.SubElement(self.current_element, tag)
        else:
            el = etree.SubElement(self.current_element, tag, attrib)
        return el

    def set_current_element(self, el):
        self.current_element = el

    def set_to_parent(self):
        self.current_element = self.current_element.getparent()

    def generate_labeled_block(self, node, label):
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-textbody'})
        el1 = etree.SubElement(el, 'text:span',
            attrib={'text:style-name': 'rststyle-strong'})
        el1.text = label
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-blockindent'})
        return el

    def encode(self, text):
        text = text.replace(u'\u00a0', " ")
        return text

    def trace_visit_node(self, node):
        if DEBUG >= 1:
            self.trace_level += 1
            self._trace_show_level(self.trace_level)
            if DEBUG >= 2:
                print '(visit_%s) node: %s' % (node.tagname, node.astext(), )
            else:
                print '(visit_%s)' % node.tagname

    def trace_depart_node(self, node):
        if not DEBUG:
            return
        self._trace_show_level(self.trace_level)
        print '(depart_%s)' % node.tagname
        self.trace_level -= 1

    def _trace_show_level(self, level):
        for idx in range(level):
            print '   ',

##    def translate_escapes(self, mo):
##        val = mo.group()
##        if val == '\\n':
##            result ='\n'
##        elif val == '\\t':
##            result = '\t'
##        return result


    #
    # Visitor functions
    #
    # In alphabetic order.
    #   See docutils.docutils.nodes.node_class_names.
    #

    def default_visit(self, node):
        #ipshell('At default_visit')
        print 'missing visit_%s' % (node.tagname, )

    def default_departure(self, node):
        print 'missing depart_%s' % (node.tagname, )

    def visit_Text(self, node):
        #ipshell('At visit_Text')
        # Skip nodes whose text has been processed in parent nodes.
        if isinstance(node.parent, docutils.nodes.title) or \
            isinstance(node.parent, docutils.nodes.literal_block):
            #isinstance(node.parent, docutils.nodes.term) or \
            #isinstance(node.parent, docutils.nodes.definition):
            return
        text = node.astext()
        #print '(visit_Text) text:', text.encode('utf-8'), node.parent.__class__
        # Are we in mixed content?  If so, add the text to the
        #   etree tail of the previous sibling element.
        if len(self.current_element.getchildren()) > 0:
            #print '*** (visit_Text) 1. text: %s' % text
            self.current_element.getchildren()[-1].tail = text
        else:
            self.current_element.text = text

    def depart_Text(self, node):
        pass

    def visit_address(self, node):
        #ipshell('At visit_address')
        el = self.generate_labeled_block(node, 'Address: ')
        self.set_current_element(el)

    def depart_address(self, node):
        self.set_to_parent()

    def visit_author(self, node):
        #self.trace_visit_node(node)
        if isinstance(node.parent, nodes.authors):
            el = self.append_child('text:p', attrib={
                'text:style-name': 'rststyle-blockindent'})
        else:
            el = self.generate_labeled_block(node, 'Author: ')
        self.set_current_element(el)

    def depart_author(self, node):
        #self.trace_depart_node(node)
        self.set_to_parent()

    def visit_authors(self, node):
        #ipshell('At visit_authors')
        #self.trace_visit_node(node)
        label = 'Authors:'
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-textbody'})
        el1 = etree.SubElement(el, 'text:span',
            attrib={'text:style-name': 'rststyle-strong'})
        el1.text = label

    def depart_authors(self, node):
        #self.trace_depart_node(node)
        pass

    def visit_block_quote(self, node):
        #ipshell('At visit_block_quote')
        self.paragraph_style_stack.append('rststyle-blockquote')

    def depart_block_quote(self, node):
        self.paragraph_style_stack.pop()

    def visit_bullet_list(self, node):
        #import pdb; pdb.set_trace()
        #ipshell('At visit_bullet_list')
        #print '(visit_bullet_list) node: %s' % node.astext()
        el = etree.SubElement(self.current_element, 'text:list', attrib={
            'text:style-name': 'rststyle-bulletlist',
            })
        self.set_current_element(el)
        self.paragraph_style_stack.append('rststyle-bulletitem')

    def depart_bullet_list(self, node):
        self.set_to_parent()
        self.paragraph_style_stack.pop()

    def visit_comment(self, node):
        #ipshell('At visit_comment')
        el = self.append_child('text:p',
            attrib={'text:style-name': 'rststyle-textbody'})
        el1 =  etree.SubElement(el, 'office:annotation', attrib={})
        el2 =  etree.SubElement(el1, 'text:p', attrib={})
        el2.text = node.astext()

    def depart_comment(self, node):
        pass

    def visit_copyright(self, node):
        el = self.generate_labeled_block(node, 'Copyright: ')
        self.set_current_element(el)

    def depart_copyright(self, node):
        self.set_to_parent()

    def visit_date(self, node):
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-textbody'})
        el1 = etree.SubElement(el, 'text:span',
            attrib={'text:style-name': 'rststyle-strong'})
        el1.text = 'Date: '
        el1.tail = node.astext()

    def depart_date(self, node):
        pass

    def visit_decoration(self, node):
        #global DEBUG
        #ipshell('At visit_decoration')
        #DEBUG = 1
        #self.trace_visit_node(node)
        pass

    def depart_decoration(self, node):
        #global DEBUG
        #self.trace_depart_node(node)
        #DEBUG = 0
        #ipshell('At depart_decoration')
        el = self.current_element.getchildren()[-1]
        self.current_element.remove(el)
        el1 = etree.Element('text:section', attrib={
            'text:name': '_rstFooterSection',
            })
        el2 = etree.SubElement(el1, 'text:p', attrib={
            'text:style-name': 'rststyle-horizontalline'})
        el1.append(el)
        self.footer_element = el1

    def visit_definition(self, node):
        el = self.append_child('text:p',
            attrib={'text:style-name': 'rststyle-blockindent'})
        #el1.text = node.astext()
        self.set_current_element(el)
        self.omit = True

    def depart_definition(self, node):
        self.set_to_parent()
        self.omit = False

    def visit_definition_list(self, node):
        pass

    def depart_definition_list(self, node):
        pass

    def visit_definition_list_item(self, node):
        pass

    def depart_definition_list_item(self, node):
        pass

    def visit_term(self, node):
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-textbody'})
        el1 = etree.SubElement(el, 'text:span',
            attrib={'text:style-name': 'rststyle-strong'})
        #el1.text = node.astext()
        self.set_current_element(el1)

    def depart_term(self, node):
        self.set_to_parent()
        self.set_to_parent()

    def visit_document(self, node):
        #import pdb; pdb.set_trace()
        #ipshell('At visit_document')
        #self.set_current_element(self.content_tree.getroot())
        pass

    def depart_document(self, node):
        if self.footer_element is not None:
            self.current_element.append(self.footer_element)
        pass

    def visit_docinfo(self, node):
        #self.document.reporter.debug_flag = 1
        self.trace_visit_node(node)
        self.section_level += 1
        self.section_count += 1
        el = self.append_child('text:section', attrib={
            'text:name': 'Section%d' % self.section_count,
            'text:style-name': 'Sect%d' % self.section_level,
            })
        self.set_current_element(el)

    def depart_docinfo(self, node):
        #self.document.reporter.debug_flag = 0
        self.trace_depart_node(node)
        self.section_level -= 1
        self.set_to_parent()

    def visit_emphasis(self, node):
        el = etree.SubElement(self.current_element, 'text:span',
            attrib={'text:style-name': 'rststyle-emphasis'})
        self.set_current_element(el)

    def depart_emphasis(self, node):
        self.set_to_parent()

    def visit_enumerated_list(self, node):
        #ipshell('At visit_enumerated_list')
        #import pdb; pdb.set_trace()
        #print '(visit_enumerated_list) node: %s' % node.astext()
        el = etree.SubElement(self.current_element, 'text:list', attrib={
            'text:style-name': 'rststyle-enumlist',
            })
        self.set_current_element(el)
        self.paragraph_style_stack.append('rststyle-enumitem')

    def depart_enumerated_list(self, node):
        self.set_to_parent()
        self.paragraph_style_stack.pop()

    def visit_list_item(self, node):
        #import pdb; pdb.set_trace()
        #ipshell('At visit_document')
        #print '(visit_list_item) node: %s' % node.astext()
        el = etree.SubElement(self.current_element, 'text:list-item')
        self.set_current_element(el)

    def depart_list_item(self, node):
        self.set_to_parent()

    def visit_footer(self, node):
        #ipshell('At visit_footer')
        #self.trace_visit_node(node)
        pass

    def depart_footer(self, node):
        #self.trace_depart_node(node)
        pass

    def visit_field(self, node):
        #ipshell('At visit_field')
        #self.trace_visit_node(node)
        pass

    def depart_field(self, node):
        #self.trace_depart_node(node)
        #self.current_element.append(self.field_element)
        pass

    def visit_field_name(self, node):
        #ipshell('At visit_field_name')
        #self.trace_visit_node(node)
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-textbody'})
        el1 = etree.SubElement(el, 'text:span',
            attrib={'text:style-name': 'rststyle-strong'})
        el1.text = node.astext()

    def depart_field_name(self, node):
        #self.trace_depart_node(node)
        pass

    def visit_field_body(self, node):
        #ipshell('At visit_field_body')
        #self.trace_visit_node(node)
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-blockindent'})
        el.text = node.astext()
        raise nodes.SkipChildren()

    def depart_field_body(self, node):
        #self.trace_depart_node(node)
        pass

    def visit_generated(self, node):
        pass

    def depart_generated(self, node):
        pass

    def visit_literal(self, node):
        #ipshell('At visit_literal')
        el = etree.SubElement(self.current_element, 'text:span',
            attrib={'text:style-name': 'rststyle-inlineliteral'})
        self.set_current_element(el)

    def depart_literal(self, node):
        self.set_to_parent()

    def calculate_code_block_padding(self, line):
        count = 0
        matchobj = SPACES_PATTERN.match(line)
        if matchobj:
            pad = matchobj.group()
            count = len(pad)
        else:
            matchobj = TABS_PATTERN.match(line)
            if matchobj:
                pad = matchobj.group()
                count = len(pad) * 8
        return count

    def visit_literal_block(self, node):
        #ipshell('At visit_literal_block')
        lines = node.astext().split('\n')
        for line in lines:
            padcount = self.calculate_code_block_padding(line)
            line = line.lstrip(' \t')
            el = self.append_child('text:p', attrib={
                'text:style-name': 'rststyle-codeblock',
                })
            if padcount > 0:
                el1 = etree.SubElement(el, 'text:s', attrib={
                    'text:c': '%d' % padcount, })
                el1.tail = line
            else:
                el.text = line

    def depart_literal_block(self, node):
        pass

    visit_doctest_block = visit_literal_block
    depart_doctest_block = depart_literal_block

    def show_message(self, msg):
        #print '*** tagname: [[%s]] msg: %s' % (msg.starttag(), msg.astext(), )
        #print '*** msg: %s' % (dir(msg), )
        #print '*** msg.asdom(): %s' % (msg.asdom(), )
        print '*** msg.astext(): %s' % (msg.astext(), )

    def visit_option_list(self, node):
        #self.document.reporter.debug_flag = 1
        #self.document.reporter.attach_observer(self.show_message)
        table_name = 'rststyle-tableoption'
        #
        # Generate automatic styles
        if not self.optiontablestyles_generated:
            self.optiontablestyles_generated = True
            el = etree.SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': table_name,
                'style:family': 'table'})
            el1 = etree.SubElement(el, 'style:table-properties', attrib={
                'style:width': '17.59cm',
                'table:align': 'left',
                'style:shadow': 'none'})
            el = etree.SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.A' % table_name,
                'style:family': 'table-column'})
            el1 = etree.SubElement(el, 'style:table-column-properties', attrib={
                'style:column-width': '4.999cm'})
            el = etree.SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.B' % table_name,
                'style:family': 'table-column'})
            el1 = etree.SubElement(el, 'style:table-column-properties', attrib={
                'style:column-width': '12.587cm'})
            el = etree.SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.A1' % table_name,
                'style:family': 'table-cell'})
            el1 = etree.SubElement(el, 'style:table-cell-properties', attrib={
                'fo:background-color': 'transparent',
                'fo:padding': '0.097cm',
                'fo:border-left': '0.035cm solid #000000',
                'fo:border-right': 'none',
                'fo:border-top': '0.035cm solid #000000',
                'fo:border-bottom': '0.035cm solid #000000'})
            el2 = etree.SubElement(el1, 'style:background-image')
            el = etree.SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.B1' % table_name,
                'style:family': 'table-cell'})
            el1 = etree.SubElement(el, 'style:table-cell-properties', attrib={
                'fo:padding': '0.097cm',
                'fo:border': '0.035cm solid #000000'})
            el = etree.SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.A2' % table_name,
                'style:family': 'table-cell'})
            el1 = etree.SubElement(el, 'style:table-cell-properties', attrib={
                'fo:padding': '0.097cm',
                'fo:border-left': '0.035cm solid #000000',
                'fo:border-right': 'none',
                'fo:border-top': 'none',
                'fo:border-bottom': '0.035cm solid #000000'})
            el = etree.SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.B2' % table_name,
                'style:family': 'table-cell'})
            el1 = etree.SubElement(el, 'style:table-cell-properties', attrib={
                'fo:padding': '0.097cm',
                'fo:border-left': '0.035cm solid #000000',
                'fo:border-right': '0.035cm solid #000000',
                'fo:border-top': 'none',
                'fo:border-bottom': '0.035cm solid #000000'})
        #
        # Generate table data
        el = self.append_child('table:table', attrib={
            'table:name': table_name,
            'table:style-name': table_name,
            })
        el1 = etree.SubElement(el, 'table:table-column', attrib={
            'table:style-name': '%s.A' % table_name})
        el1 = etree.SubElement(el, 'table:table-column', attrib={
            'table:style-name': '%s.B' % table_name})
        el1 = etree.SubElement(el, 'table:table-header-rows')
        el2 = etree.SubElement(el1, 'table:table-row')
        el3 = etree.SubElement(el2, 'table:table-cell', attrib={
            'table:style-name': '%s.A1' % table_name,
            'office:value-type': 'string'})
        el4 = etree.SubElement(el3, 'text:p', attrib={
            'text:style-name': 'Table_20_Heading'})
        el4.text= 'Option'
        el3 = etree.SubElement(el2, 'table:table-cell', attrib={
            'table:style-name': '%s.B1' % table_name,
            'office:value-type': 'string'})
        el4 = etree.SubElement(el3, 'text:p', attrib={
            'text:style-name': 'Table_20_Heading'})
        el4.text= 'Description'
        self.set_current_element(el)

    def depart_option_list(self, node):
        #self.document.reporter.debug_flag = 0
        self.set_to_parent()

    def visit_option_list_item(self, node):
        el = self.append_child('table:table-row')
        self.set_current_element(el)

    def depart_option_list_item(self, node):
        self.set_to_parent()

    def visit_option_group(self, node):
        el = self.append_child('table:table-cell', attrib={
            'table:style-name': 'Table%d.A2' % self.table_count,
            'office:value-type': 'string',
        })
        self.set_current_element(el)

    def depart_option_group(self, node):
        self.set_to_parent()

    def visit_option(self, node):
        el = self.append_child('text:p', attrib={
            'text:style-name': 'Table_20_Contents'})
        el.text = node.astext()

    def depart_option(self, node):
        pass

    def visit_option_string(self, node):
        pass

    def depart_option_string(self, node):
        pass

    def visit_option_argument(self, node):
        #ipshell('At visit_option_argument')
        pass

    def depart_option_argument(self, node):
        pass

    def visit_description(self, node):
        el = self.append_child('table:table-cell', attrib={
            'table:style-name': 'Table%d.B2' % self.table_count,
            'office:value-type': 'string',
        })
        el1 = etree.SubElement(el, 'text:p', attrib={
            'text:style-name': 'Table_20_Contents'})
        el1.text = node.astext()
        raise nodes.SkipChildren()

    def depart_description(self, node):
        pass

    def visit_paragraph(self, node):
        #ipshell('At visit_paragraph')
        #self.trace_visit_node(node)
        if self.omit:
            return
        style_name = self.paragraph_style_stack[-1]
        el = self.append_child('text:p',
            attrib={'text:style-name': style_name})
        self.set_current_element(el)

    def depart_paragraph(self, node):
        #self.trace_depart_node(node)
        if self.omit:
            return
        self.set_to_parent()

    def visit_problematic(self, node):
        print '(visit_problematic) node: %s' % (node.astext(), )

    def depart_problematic(self, node):
        pass

    def visit_reference(self, node):
        #self.trace_visit_node(node)
        text = node.astext()
        if node.has_key('refuri'):
            href = node['refuri']
            if ( self.settings.cloak_email_addresses
                 and href.startswith('mailto:')):
                href = self.cloak_mailto(href)
        elif node.has_key('refid'):
            href = '#' + node['refid']
        else:
            raise RuntimeError, 'References must have "refuri" or "refid" attribute.'
        #print '(visit_reference) href: "%s"  text: "%s"' % (href, text, )
        el = self.append_child('text:a', attrib={
            'xlink:href': '%s' % href,
            'xlink:type': 'simple',
            })
        self.set_current_element(el)

    def depart_reference(self, node):
        #self.trace_depart_node(node)
        self.set_to_parent()

    def visit_revision(self, node):
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-textbody'})
        el1 = etree.SubElement(el, 'text:span',
            attrib={'text:style-name': 'rststyle-strong'})
        el1.text = 'Revision: '
        el1.tail = node.astext()

    def depart_revision(self, node):
        pass

    def visit_section(self, node, move_ids=1):
        #ipshell('At visit_section')
        self.section_level += 1
        self.section_count += 1
        el = self.append_child('text:section', attrib={
            'text:name': 'Section%d' % self.section_count,
            'text:style-name': 'Sect%d' % self.section_level,
            })
        self.set_current_element(el)

    def depart_section(self, node):
        self.section_level -= 1
        self.set_to_parent()

    def visit_strong(self, node):
        #ipshell('At visit_strong')
        el = etree.SubElement(self.current_element, 'text:span',
            attrib={'text:style-name': 'rststyle-strong'})
        self.set_current_element(el)

    def depart_strong(self, node):
        self.set_to_parent()

    def visit_system_message(self, node):
        print '(visit_system_message) node: %s' % (node.astext(), )

    def depart_system_message(self, node):
        pass

    def visit_table(self, node):
        #self.trace_visit_node(node)
        #ipshell('At visit_table')
        self.table_count += 1
        table_name = '%s%d' % (TableStylePrefix, self.table_count, )
        el1 = etree.SubElement(self.automatic_styles, 'style:style', attrib={
            'style:name': '%s' % table_name,
            'style:family': 'table',
            })
        el1_1 = etree.SubElement(el1, 'style:table-properties', attrib={
            #'style:width': '17.59cm',
            'table:align': 'margins',
            })
        # We use a single cell style for all cells in this table.
        # That's probably not correct, but seems to work.
        el2 = etree.SubElement(self.automatic_styles, 'style:style', attrib={
            'style:name': '%s.A1' % table_name,
            'style:family': 'table-cell',
            })
        line_style1 = '0.%03dcm solid #000000' % self.settings.table_border_thickness
        el2_1 = etree.SubElement(el2, 'style:table-cell-properties', attrib={
            'fo:padding': '0.049cm',
            'fo:border-left': line_style1,
            'fo:border-right': line_style1,
            'fo:border-top': line_style1,
            'fo:border-bottom': line_style1,
            })
        el3 = etree.SubElement(self.current_element, 'table:table', attrib={
            'table:name': '%s' % table_name,
            'table:style-name': '%s' % table_name,
            })
        self.set_current_element(el3)
        self.current_table_style = el1
        self.table_width = 0

    def depart_table(self, node):
        #self.trace_depart_node(node)
        #ipshell('At depart_table')
        self.current_table_style.attrib['style:width'] = \
            '%dcm' % self.table_width
        self.set_to_parent()

    def visit_tgroup(self, node):
        #self.trace_visit_node(node)
        #ipshell('At visit_tgroup')
        self.column_count = ord('A') - 1

    def depart_tgroup(self, node):
        #self.trace_depart_node(node)
        pass

    def visit_colspec(self, node):
        #self.trace_visit_node(node)
        #ipshell('At visit_colspec')
        self.column_count += 1
        table_name = '%s%d' % (TableStylePrefix, self.table_count, )
        colspec_name = '%s%d.%s' % (
            TableStylePrefix, self.table_count, chr(self.column_count),)
        colwidth = node['colwidth']
        el1 = etree.SubElement(self.automatic_styles, 'style:style', attrib={
            'style:name': colspec_name,
            'style:family': 'table-column',
            })
        el1_1 = etree.SubElement(el1, 'style:table-column-properties', attrib={
            'style:column-width': '%dcm' % colwidth })
        el2 = self.append_child('table:table-column', attrib={
            'table:style-name': colspec_name,
            })
        self.table_width += colwidth

    def depart_colspec(self, node):
        #self.trace_depart_node(node)
        pass

    def visit_thead(self, node):
        #self.trace_visit_node(node)
        #ipshell('At visit_thead')
        el = self.append_child('table:table-header-rows')
        self.set_current_element(el)
        self.in_thead = True
        self.paragraph_style_stack.append('Table_20_Heading')

    def depart_thead(self, node):
        #self.trace_depart_node(node)
        self.set_to_parent()
        self.in_thead = False
        self.paragraph_style_stack.pop()

    def visit_row(self, node):
        #self.trace_visit_node(node)
        #ipshell('At visit_row')
        self.column_count = ord('A') - 1
        el = self.append_child('table:table-row')
        self.set_current_element(el)

    def depart_row(self, node):
        #self.trace_depart_node(node)
        self.set_to_parent()

    def visit_entry(self, node):
        #self.trace_visit_node(node)
        #ipshell('At visit_entry')
        table_name = '%s%d' % (TableStylePrefix, self.table_count, )
        self.column_count += 1
        colspec_name = '%s%d.%s' % (
            TableStylePrefix, self.table_count, chr(self.column_count),)
        cellspec_name = '%s%d.A1' % (
            TableStylePrefix, self.table_count,)
        el1 = self.append_child('table:table-cell', attrib={
            'table:style-name': cellspec_name,
            'office:value-type': 'string',
            })
        morecols = node.get('morecols', 0)
        if morecols > 0:
            el1.attrib['table:number-columns-spanned'] = '%d' % (morecols + 1,)
            self.column_count += morecols
        self.set_current_element(el1)

    def depart_entry(self, node):
        #self.trace_depart_node(node)
        self.set_to_parent()

    def visit_tbody(self, node):
        #self.trace_visit_node(node)
        #ipshell('At visit_')
        pass

    def depart_tbody(self, node):
        #self.trace_depart_node(node)
        pass

    def visit_target(self, node):
        #
        # I don't know how to implement targets in ODF.
        # How do we create a target in oowriter?  A cross-reference?
        if not (node.has_key('refuri') or node.has_key('refid')
                or node.has_key('refname')):
            pass
        else:
            pass

    def depart_target(self, node):
        pass

    def visit_title(self, node, move_ids=1):
        #ipshell('At visit_title')
        if isinstance(node.parent, docutils.nodes.section):
            section_level = self.section_level
            if section_level > 5:
                print 'Warning: Heading/section levels greater than 3 not supported.'
                print '    Reducing to heading level 3 for heading:'
                print '    "%s"' % node.astext()
                section_level = 3
            el1 = etree.SubElement(self.current_element, 'text:h', attrib = {
                'text:outline-level': '%d' % section_level,
                #'text:style-name': 'Heading_20_%d' % section_level,
                'text:style-name': 'rststyle-heading%d' % section_level,
                })
            text = node.astext()
            #el1.text = text.decode('latin-1').encode('utf-8')
            el1.text = self.encode(text)
        elif isinstance(node.parent, docutils.nodes.document):
            el1 = etree.SubElement(self.current_element, 'text:h', attrib = {
                'text:outline-level': '1',
                'text:style-name': 'rststyle-heading1',
                })
            text = node.astext()
            text = text.decode('latin-1').encode('utf-8')
            el1.text = text
            self.title = text

    def depart_title(self, node):
        pass

    def visit_title_reference(self, node):
        #ipshell('At visit_title_reference')
        el = self.append_child('text:span', attrib={
            'text:style-name': 'rststyle-quotation'})
        el.text = self.encode(node.astext())

    def depart_title_reference(self, node):
        pass

    def visit_topic(self, node):
        #ipshell('At visit_topic')
        #import pdb; pdb.set_trace()
        if 'classes' in node.attributes:
            if 'contents' in node.attributes['classes']:
                el = self.append_child('text:p', attrib={
                    'text:style-name': 'rststyle-horizontalline'})
                el = self.append_child('text:p', attrib={
                    'text:style-name': 'rststyle-centeredtextbody'})
                el1 = etree.SubElement(el, 'text:span',
                    attrib={'text:style-name': 'rststyle-strong'})
                el1.text = 'Contents'
            elif 'abstract' in node.attributes['classes']:
                el = self.append_child('text:p', attrib={
                    'text:style-name': 'rststyle-horizontalline'})
                el = self.append_child('text:p', attrib={
                    'text:style-name': 'rststyle-centeredtextbody'})
                el1 = etree.SubElement(el, 'text:span',
                    attrib={'text:style-name': 'rststyle-strong'})
                el1.text = 'Abstract'

    def depart_topic(self, node):
        #ipshell('At depart_topic')
        if 'classes' in node.attributes:
            if 'contents' in node.attributes['classes']:
                el = self.append_child('text:p', attrib={
                    'text:style-name': 'rststyle-horizontalline'})

    def visit_transition(self, node):
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-horizontalline'})

    def depart_transition(self, node):
        pass

