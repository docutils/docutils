
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
import time
import re
import StringIO
import inspect
import imp
import docutils
from docutils import frontend, nodes, utils, writers, languages
from docutils.parsers import rst


WhichElementTree = ''
try:
    # 1. Try to use lxml.
    from lxml import etree
    #print '*** using lxml'
    WhichElementTree = 'lxml'
except ImportError, e:
    try:
        # 2. Try to use ElementTree from the Python standard library.
        from xml.etree import ElementTree as etree
        #print '*** using ElementTree'
        WhichElementTree = 'elementtree'
    except ImportError, e:
        try:
            # 3. Try to use a version of ElementTree installed as a separate
            #    product.
            from elementtree import ElementTree as etree
            WhichElementTree = 'elementtree'
        except ImportError, e:
            print '***'
            print '*** Error: Must install either ElementTree or lxml or'
            print '***   a version of Python containing ElementTree.'
            print '***'
            raise

try:
    import pygments
    import pygments.formatter
    import pygments.lexers
    class OdtPygmentsFormatter(pygments.formatter.Formatter):
        pass
    class OdtPygmentsProgFormatter(OdtPygmentsFormatter):
        def format(self, tokensource, outfile):
            tokenclass = pygments.token.Token
            for ttype, value in tokensource:
                value = escape_cdata(value)
                if ttype == tokenclass.Keyword:
                    s2 = 'rststyle-codeblock-keyword'
                    s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                        (s2, value, )
                elif ttype == tokenclass.Literal.String:
                    s2 = 'rststyle-codeblock-string'
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
                    s2 = 'rststyle-codeblock-number'
                    s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                        (s2, value, )
                elif ttype == tokenclass.Operator:
                    s2 = 'rststyle-codeblock-operator'
                    s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                        (s2, value, )
                elif ttype == tokenclass.Comment:
                    s2 = 'rststyle-codeblock-comment'
                    s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                        (s2, value, )
                elif ttype == tokenclass.Name.Class:
                    s2 = 'rststyle-codeblock-classname'
                    s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                        (s2, value, )
                elif ttype == tokenclass.Name.Function:
                    s2 = 'rststyle-codeblock-functionname'
                    s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                        (s2, value, )
                elif ttype == tokenclass.Name:
                    s2 = 'rststyle-codeblock-name'
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
                    s2 = 'rststyle-codeblock-keyword'
                    s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                        (s2, value, )
                elif ttype in (tokenclass.Literal.String,
                        tokenclass.Literal.String.Backtick,
                        ):
                    s2 = 'rststyle-codeblock-string'
                    s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                        (s2, value, )
                elif ttype == tokenclass.Name.Attribute:
                    s2 = 'rststyle-codeblock-operator'
                    s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                        (s2, value, )
                elif ttype == tokenclass.Comment:
                    if value[-1] == '\n':
                        s2 = 'rststyle-codeblock-comment'
                        s1 = '<text:span text:style-name="%s">%s</text:span>\n' % \
                            (s2, value[:-1], )
                    else:
                        s2 = 'rststyle-codeblock-comment'
                        s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                            (s2, value, )
                elif ttype == tokenclass.Name.Builtin:
                    s2 = 'rststyle-codeblock-name'
                    s1 = '<text:span text:style-name="%s">%s</text:span>' % \
                        (s2, value, )
                else:
                    s1 = value
                outfile.write(s1)

except ImportError, e:
    pygments = None

#
# Is the PIL imaging library installed?
try:
    import Image
except ImportError, exp:
    Image = None

##from IPython.Shell import IPShellEmbed
##args = ['-pdb', '-pi1', 'In <\\#>: ', '-pi2', '   .\\D.: ',
##    '-po', 'Out<\\#>: ', '-nosep']
##ipshell = IPShellEmbed(args,
##    banner = 'Entering IPython.  Press Ctrl-D to exit.',
##    exit_msg = 'Leaving Interpreter, back to program.')


#
# ElementTree does not support getparent method (lxml does).
# This wrapper class and the following support functions provide
#   that support for the ability to get the parent of an element.
#
if WhichElementTree == 'elementtree':
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


#
# Constants and globals

# Turn tracing on/off.  See methods trace_visit_node/trace_depart_node.
DEBUG = 0
SPACES_PATTERN = re.compile(r'( +)')
TABS_PATTERN = re.compile(r'(\t+)')
FILL_PAT1 = re.compile(r'^ +')
FILL_PAT2 = re.compile(r' {2,}')
# Match a section number followed by 3 \xa0 bytes.
# Note that we actually check for the class "auto-toc" instead.
#SECTNUM_PAT = re.compile(r'^\d*(\.\d*)*\240\240\240')

TableStylePrefix = 'rststyle-Table'

GENERATOR_DESC = 'Docutils.org/odtwriter'

NAME_SPACE_1 = 'urn:oasis:names:tc:opendocument:xmlns:office:1.0'

CONTENT_NAMESPACE_DICT = CNSD = {
#    'office:version': '1.0',
    'chart': 'urn:oasis:names:tc:opendocument:xmlns:chart:1.0',
    'dc': 'http://purl.org/dc/elements/1.1/',
    'dom': 'http://www.w3.org/2001/xml-events',
    'dr3d': 'urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0',
    'draw': 'urn:oasis:names:tc:opendocument:xmlns:drawing:1.0',
    'fo': 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0',
    'form': 'urn:oasis:names:tc:opendocument:xmlns:form:1.0',
    'math': 'http://www.w3.org/1998/Math/MathML',
    'meta': 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0',
    'number': 'urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0',
    'office': NAME_SPACE_1,
    'ooo': 'http://openoffice.org/2004/office',
    'oooc': 'http://openoffice.org/2004/calc',
    'ooow': 'http://openoffice.org/2004/writer',
    'script': 'urn:oasis:names:tc:opendocument:xmlns:script:1.0',
    'style': 'urn:oasis:names:tc:opendocument:xmlns:style:1.0',
    'svg': 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0',
    'table': 'urn:oasis:names:tc:opendocument:xmlns:table:1.0',
    'text': 'urn:oasis:names:tc:opendocument:xmlns:text:1.0',
    'xforms': 'http://www.w3.org/2002/xforms',
    'xlink': 'http://www.w3.org/1999/xlink',
    'xsd': 'http://www.w3.org/2001/XMLSchema',
    'xsi': 'http://www.w3.org/2001/XMLSchema-instance',
    }

STYLES_NAMESPACE_DICT = SNSD = {
#    'office:version': '1.0',
    'chart': 'urn:oasis:names:tc:opendocument:xmlns:chart:1.0',
    'dc': 'http://purl.org/dc/elements/1.1/',
    'dom': 'http://www.w3.org/2001/xml-events',
    'dr3d': 'urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0',
    'draw': 'urn:oasis:names:tc:opendocument:xmlns:drawing:1.0',
    'fo': 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0',
    'form': 'urn:oasis:names:tc:opendocument:xmlns:form:1.0',
    'math': 'http://www.w3.org/1998/Math/MathML',
    'meta': 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0',
    'number': 'urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0',
    'office': NAME_SPACE_1,
    'ooo': 'http://openoffice.org/2004/office',
    'oooc': 'http://openoffice.org/2004/calc',
    'ooow': 'http://openoffice.org/2004/writer',
    'script': 'urn:oasis:names:tc:opendocument:xmlns:script:1.0',
    'style': 'urn:oasis:names:tc:opendocument:xmlns:style:1.0',
    'svg': 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0',
    'table': 'urn:oasis:names:tc:opendocument:xmlns:table:1.0',
    'text': 'urn:oasis:names:tc:opendocument:xmlns:text:1.0',
    'xlink': 'http://www.w3.org/1999/xlink',
    }

MANIFEST_NAMESPACE_DICT = MANNSD = {
    'manifest': 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0',
}

META_NAMESPACE_DICT = METNSD = {
#    'office:version': '1.0',
    'dc': 'http://purl.org/dc/elements/1.1/',
    'meta': 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0',
    'office': NAME_SPACE_1,
    'ooo': 'http://openoffice.org/2004/office',
    'xlink': 'http://www.w3.org/1999/xlink',
}

MIME_TYPE = 'application/vnd.oasis.opendocument.text'


#
# Attribute dictionaries for use with ElementTree (not lxml), which
#   does not support use of nsmap parameter on Element() and SubElement().

CONTENT_NAMESPACE_ATTRIB = {
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
    'xmlns:office': NAME_SPACE_1,
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

STYLES_NAMESPACE_ATTRIB = {
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
    'xmlns:office': NAME_SPACE_1,
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

MANIFEST_NAMESPACE_ATTRIB = {
    'xmlns:manifest': 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0',
}

META_NAMESPACE_ATTRIB = {
    'office:version': '1.0',
    'xmlns:dc': 'http://purl.org/dc/elements/1.1/',
    'xmlns:meta': 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0',
    'xmlns:office': NAME_SPACE_1,
    'xmlns:ooo': 'http://openoffice.org/2004/office',
    'xmlns:xlink': 'http://www.w3.org/1999/xlink',
}


#
# Functions
#

#
# ElementTree support functions.
# In order to be able to get the parent of elements, must use these
#   instead of the functions with same name provided by ElementTree.
#
def Element(tag, attrib=None, nsmap=None, nsdict=CNSD):
    if attrib is None:
        attrib = {}
    tag, attrib = fix_ns(tag, attrib, nsdict)
    if WhichElementTree == 'lxml':
        el = etree.Element(tag, attrib, nsmap=nsmap)
    else:
        el = _ElementInterfaceWrapper(tag, attrib)
    return el

def SubElement(parent, tag, attrib=None, nsmap=None, nsdict=CNSD):
    if attrib is None:
        attrib = {}
    tag, attrib = fix_ns(tag, attrib, nsdict)
    if WhichElementTree == 'lxml':
        el = etree.SubElement(parent, tag, attrib, nsmap=nsmap)
    else:
        el = _ElementInterfaceWrapper(tag, attrib)
        parent.append(el)
        el.setparent(parent)
    return el

def fix_ns(tag, attrib, nsdict):
    nstag = add_ns(tag, nsdict)
    nsattrib = {}
    for key, val in attrib.iteritems():
        nskey = add_ns(key, nsdict)
        nsattrib[nskey] = val
    return nstag, nsattrib

def add_ns(tag, nsdict=CNSD):
    if WhichElementTree == 'lxml':
        nstag, name = tag.split(':')
        ns = nsdict.get(nstag)
        if ns is None:
            raise RuntimeError, 'Invalid namespace prefix: %s' % nstag
        tag = '{%s}%s' % (ns, name,)
        #print '*** tag: "%s"' % tag
    return tag

def ToString(et):
    outstream = StringIO.StringIO()
    et.write(outstream)
    s1 = outstream.getvalue()
    outstream.close()
    return s1

def escape_cdata(text):
    text = text.replace("&", "&amp;")
    text = text.replace("<", "&lt;")
    text = text.replace(">", "&gt;")
    return text


#
# Classes
#

# Does this version of Docutils has Directive support?
if hasattr(rst, 'Directive'):
    #
    # Class to control syntax highlighting.
##    class SyntaxHighlight(rst.Directive):
##        required_arguments = 1
##        optional_arguments = 0
##        #
##        # See visit_field for code that processes the node created here.
##        def run(self):
##            arguments = ' '.join(self.arguments)
##            paragraph = nodes.paragraph(arguments, arguments)
##            field_body = nodes.field_body()
##            field_body += paragraph
##            paragraph = nodes.paragraph('syntaxhighlight', 'syntaxhighlight')
##            field_name = nodes.field_name()
##            field_name += paragraph
##            field = nodes.field()
##            field += field_name
##            field += field_body
##            return [field]
##    
##    rst.directives.register_directive('sourcecode', SyntaxHighlight)

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
            #import pdb; pdb.set_trace()
            return [code_block]

    rst.directives.register_directive('sourcecode', SyntaxHighlightCodeBlock)
    rst.directives.register_directive('code', SyntaxHighlightCodeBlock)


#
# Register directives defined in a module named "odtwriter_plugins".
#
def load_plugins():
    plugin_mod = None
    count = 0
    try:
        name = 'odtwriter_plugins'
        fp, pathname, description = imp.find_module(name)
        plugin_mod = imp.load_module(name, fp, pathname, description)
        #import odtwriter_plugins
        #plugin_mod = odtwriter_plugins
    except ImportError, e:
        pass
    if plugin_mod is None:
        return count
    klasses = inspect.getmembers(plugin_mod, inspect.isclass)
    for klass in klasses:
        if register_plugin(*klass):
            count += 1
    return count

def register_plugin(name, klass):
    plugin_name = getattr(klass, 'plugin_name', None)
    if plugin_name is not None:
        rst.directives.register_directive(plugin_name, klass)

load_plugins()


class Writer(writers.Writer):

    supported = ('html', 'html4css1', 'xhtml')
    """Formats this writer supports."""

    default_stylesheet = 'styles.odt'
##    default_plugins_name = 'docutils_plugins'

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
##        ('Specify a plugins/directives module (without .py). '
##            'Default: "%s"' % default_plugins_name,
##            ['--plugins-module-name'],
##            {'default': default_plugins_name}),
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
        self.visitor.add_doc_title()
        self.assemble_my_parts()
        self.output = self.parts['whole']

    def assemble_my_parts(self):
        """Assemble the `self.parts` dictionary.  Extend in subclasses.
        """
        #ipshell('At assemble_parts')
        writers.Writer.assemble_parts(self)
        f = tempfile.NamedTemporaryFile()
        zfile = zipfile.ZipFile(f, 'w', zipfile.ZIP_DEFLATED)
        self.write_zip_str(zfile, 'content.xml', self.visitor.content_astext())
        self.write_zip_str(zfile, 'mimetype', MIME_TYPE)
        s1 = self.create_manifest()
        self.write_zip_str(zfile, 'META-INF/manifest.xml', s1)
        s1 = self.create_meta()
        self.write_zip_str(zfile, 'meta.xml', s1)
        s1 = self.get_stylesheet()
        self.write_zip_str(zfile, 'styles.xml', s1)
        self.store_embedded_files(zfile)
        zfile.close()
        f.seek(0)
        whole = f.read()
        f.close()
        self.parts['whole'] = whole
        self.parts['encoding'] = self.document.settings.output_encoding
        self.parts['version'] = docutils.__version__

    def write_zip_str(self, zfile, name, bytes):
        localtime = time.localtime(time.time())
        zinfo = zipfile.ZipInfo(name, localtime)
        # Add some standard UNIX file access permissions (-rw-r--r--).
        zinfo.external_attr = (0x81a4 & 0xFFFF) << 16L
        zinfo.compress_type = zipfile.ZIP_DEFLATED
        zfile.writestr(zinfo, bytes)

    def store_embedded_files(self, zfile):
        embedded_files = self.visitor.get_embedded_file_list()
        for source, destination in embedded_files:
            if source is None:
                continue
            try:
                # encode/decode
                destination1 = destination.decode('latin-1').encode('utf-8')
                zfile.write(source, destination1, zipfile.ZIP_STORED)
            except OSError, e:
                print "Error: Can't open file %s." % (source, )

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
        s1 = self.visitor.add_header_footer(s1)
        return s1

    def assemble_parts(self):
        pass

    def create_manifest(self):
        if WhichElementTree == 'lxml':
            root = Element('manifest:manifest',
                nsmap=MANIFEST_NAMESPACE_DICT,
                nsdict=MANIFEST_NAMESPACE_DICT,
                )
        else:
            root = Element('manifest:manifest',
                attrib=MANIFEST_NAMESPACE_ATTRIB,
                nsdict=MANIFEST_NAMESPACE_DICT,
                )
        doc = etree.ElementTree(root)
        SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': 'application/vnd.oasis.opendocument.text',
            'manifest:full-path': '/',
            }, nsdict=MANNSD)
        SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': 'text/xml',
            'manifest:full-path': 'content.xml',
            }, nsdict=MANNSD)
        SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': 'text/xml',
            'manifest:full-path': 'styles.xml',
            }, nsdict=MANNSD)
        SubElement(root, 'manifest:file-entry', attrib={
            'manifest:media-type': 'text/xml',
            'manifest:full-path': 'meta.xml',
            }, nsdict=MANNSD)
        s1 = ToString(doc)
        doc = minidom.parseString(s1)
        s1 = doc.toprettyxml('  ')
        return s1

    def create_meta(self):
        if WhichElementTree == 'lxml':
            root = Element('office:document-meta',
                nsmap=META_NAMESPACE_DICT,
                nsdict=META_NAMESPACE_DICT,
                )
        else:
            root = Element('office:document-meta',
                attrib=META_NAMESPACE_ATTRIB,
                nsdict=META_NAMESPACE_DICT,
                )
        doc = etree.ElementTree(root)
        root = SubElement(root, 'office:meta', nsdict=METNSD)
        el1 = SubElement(root, 'meta:generator', nsdict=METNSD)
        el1.text = 'Docutils/rst2odf.py/%s' % (VERSION, )
        s1 = os.environ.get('USER', '')
        el1 = SubElement(root, 'meta:initial-creator', nsdict=METNSD)
        el1.text = s1
        s2 = time.strftime('%Y-%m-%dT%H:%M:%S', time.localtime())
        el1 = SubElement(root, 'meta:creation-date', nsdict=METNSD)
        el1.text = s2
        el1 = SubElement(root, 'dc:creator', nsdict=METNSD)
        el1.text = s1
        el1 = SubElement(root, 'dc:date', nsdict=METNSD)
        el1.text = s2
        el1 = SubElement(root, 'dc:language', nsdict=METNSD)
        el1.text = 'en-US'
        el1 = SubElement(root, 'meta:editing-cycles', nsdict=METNSD)
        el1.text = '1'
        el1 = SubElement(root, 'meta:editing-duration', nsdict=METNSD)
        el1.text = 'PT00M01S'
        title = self.visitor.get_title()
        el1 = SubElement(root, 'dc:title', nsdict=METNSD)
        if title:
            el1.text = title
        else:
            el1.text = '[no title]'
        s1 = ToString(doc)
        #doc = minidom.parseString(s1)
        #s1 = doc.toprettyxml('  ')
        return s1

# class ODFTranslator(nodes.SparseNodeVisitor):

class ODFTranslator(nodes.GenericNodeVisitor):

    def __init__(self, document):
        #nodes.SparseNodeVisitor.__init__(self, document)
        nodes.GenericNodeVisitor.__init__(self, document)
        self.settings = document.settings
        self.section_level = 1
        self.section_count = 0
        # Create ElementTree content and styles documents.
        if WhichElementTree == 'lxml':
            root = Element(
                'office:document-content',
                nsmap=CONTENT_NAMESPACE_DICT,
                )
        else:
            root = Element(
                'office:document-content',
                attrib=CONTENT_NAMESPACE_ATTRIB,
                )
        self.content_tree = etree.ElementTree(element=root)
        self.current_element = root
        SubElement(root, 'office:scripts')
        SubElement(root, 'office:font-face-decls')
        el = SubElement(root, 'office:automatic-styles')
        self.automatic_styles = el
        el = SubElement(root, 'office:body')
        el = SubElement(el, 'office:text')
        self.current_element = el
        self.body_text_element = el
# test styles
##        if WhichElementTree == 'lxml':
##            root = Element(
##                'office:document-styles',
##                nsmap=STYLES_NAMESPACE_DICT,
##                nsdict=STYLES_NAMESPACE_DICT,
##                )
##        else:
##            root = Element('office:document-styles',
##                attrib=STYLES_NAMESPACE_ATTRIB,
##                nsdict=STYLES_NAMESPACE_DICT,
##                )
##        self.styles_tree = etree.ElementTree(element=root)
        self.paragraph_style_stack = ['rststyle-textbody', ]
        self.list_style_stack = []
        self.omit = False
        self.table_count = 0
        self.column_count = ord('A') - 1
        self.trace_level = -1
        self.optiontablestyles_generated = False
        self.field_name = None
        self.field_element = None
        self.title = None
        self.image_count = 0
        self.image_style_count = 0
        self.image_dict = {}
        self.embedded_file_list = []
        self.syntaxhighlighting = 1
        self.syntaxhighlight_lexer = 'python'
        self.header_content = []
        self.footer_content = []
        self.in_header = False
        self.in_footer = False
        self.blockstyle = ''
        self.in_table_of_contents = False
        self.footnote_dict = {}
        self.footnote_found = False
        self.in_paragraph = False
        self.found_doc_title = False

    def add_doc_title(self):
        text = self.settings.title
        if text:
            self.title = text
            if not self.found_doc_title:
                el = Element('text:h', attrib = {
                    'text:outline-level': '1',
                    'text:style-name': 'rststyle-heading1',
                    })
                el.text = text
                self.body_text_element.insert(0, el)

    def add_header_footer(self, content):
        if len(self.header_content) <= 0 and len(self.footer_content) <= 0:
            return content
        root_el = etree.fromstring(content)
        path = '{%s}master-styles' % (NAME_SPACE_1, )
        master_el = root_el.find(path)
        if master_el is None:
            return content
        path = '{%s}master-page' % (SNSD['style'], )
        master_el = master_el.find(path)
        if master_el is None:
            return content
        el1 = master_el
        if len(self.header_content) > 0:
            if WhichElementTree == 'lxml':
                el2 = SubElement(el1, 'style:header', nsdict=SNSD)
            else:
                el2 = SubElement(el1, 'style:header',
                    attrib=STYLES_NAMESPACE_ATTRIB,
                    nsdict=STYLES_NAMESPACE_DICT,
                    )
            for el in self.header_content:
                attrkey = add_ns('text:style-name', nsdict=SNSD)
                el.attrib[attrkey] = 'rststyle-header'
                el2.append(el)
        if len(self.footer_content) > 0:
            if WhichElementTree == 'lxml':
                el2 = SubElement(el1, 'style:footer', nsdict=SNSD)
            else:
                el2 = SubElement(el1, 'style:footer',
                    attrib=STYLES_NAMESPACE_ATTRIB,
                    nsdict=STYLES_NAMESPACE_DICT,
                    )
            for el in self.footer_content:
                attrkey = add_ns('text:style-name', nsdict=SNSD)
                el.attrib[attrkey] = 'rststyle-footer'
                el2.append(el)
        #new_tree = etree.ElementTree(root_el)
        #new_content = ToString(new_tree)
        new_content = etree.tostring(root_el)
        return new_content

    def astext(self):
        root = self.content_tree.getroot()
        et = etree.ElementTree(root)
        s1 = ToString(et)
        return s1

    def content_astext(self):
        return self.astext()

# test styles
##    def styles_astext(self):
##        root = self.styles_tree.getroot()
##        et = etree.ElementTree(root)
##        s1 = ToString(et)
##        return s1

    def set_title(self, title): self.title = title
    def get_title(self): return self.title
    def set_embedded_file_list(self, embedded_file_list):
        self.embedded_file_list = embedded_file_list
    def get_embedded_file_list(self): return self.embedded_file_list

    #
    # Utility methods

    def append_child(self, tag, attrib=None):
        if attrib is None:
            el = SubElement(self.current_element, tag)
        else:
            el = SubElement(self.current_element, tag, attrib)
        return el

    def set_current_element(self, el):
        self.current_element = el

    def set_to_parent(self):
        self.current_element = self.current_element.getparent()

    def generate_labeled_block(self, node, label):
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-textbody'})
        el1 = SubElement(el, 'text:span',
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

    #
    # Visitor functions
    #
    # In alphabetic order, more or less.
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

    def visit_address(self, node):
        #ipshell('At visit_address')
        el = self.generate_labeled_block(node, 'Address: ')
        self.set_current_element(el)

    def depart_address(self, node):
        self.set_to_parent()

    def visit_attribution(self, node):
        #ipshell('At visit_attribution')
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-attribution'})
        el.text = node.astext()

    def depart_attribution(self, node):
        #ipshell('At depart_attribution')
        pass

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
        el1 = SubElement(el, 'text:span',
            attrib={'text:style-name': 'rststyle-strong'})
        el1.text = label

    def depart_authors(self, node):
        #self.trace_depart_node(node)
        pass

    def visit_block_quote(self, node):
        #ipshell('At visit_block_quote')
        if 'epigraph' in node.attributes['classes']:
            self.paragraph_style_stack.append('rststyle-epigraph')
            self.blockstyle = 'rststyle-epigraph'
        elif 'highlights' in node.attributes['classes']:
            self.paragraph_style_stack.append('rststyle-highlights')
            self.blockstyle = 'rststyle-highlights'
        else:
            self.paragraph_style_stack.append('rststyle-blockquote')
            self.blockstyle = 'rststyle-blockquote'

    def depart_block_quote(self, node):
        self.paragraph_style_stack.pop()
        self.blockstyle = ''

    def visit_bullet_list(self, node):
        #ipshell('At visit_bullet_list')
        if self.in_table_of_contents:
            if node.has_key('classes') and \
                    'auto-toc' in node.attributes['classes']:
                el = SubElement(self.current_element, 'text:list', attrib={
                    'text:style-name': 'rststyle-tocenumlist',
                    })
                self.list_style_stack.append('rststyle-enumitem')
            else:
                el = SubElement(self.current_element, 'text:list', attrib={
                    'text:style-name': 'rststyle-tocbulletlist',
                    })
                self.list_style_stack.append('rststyle-bulletitem')
        else:
            if self.blockstyle == 'rststyle-blockquote':
                el = SubElement(self.current_element, 'text:list', attrib={
                    'text:style-name': 'rststyle-blockquote-bulletlist',
                    })
                self.list_style_stack.append('rststyle-blockquote-bulletitem')
            elif self.blockstyle == 'rststyle-highlights':
                el = SubElement(self.current_element, 'text:list', attrib={
                    'text:style-name': 'rststyle-highlights-bulletlist',
                    })
                self.list_style_stack.append('rststyle-highlights-bulletitem')
            elif self.blockstyle == 'rststyle-epigraph':
                el = SubElement(self.current_element, 'text:list', attrib={
                    'text:style-name': 'rststyle-epigraph-bulletlist',
                    })
                self.list_style_stack.append('rststyle-epigraph-bulletitem')
            else:
                el = SubElement(self.current_element, 'text:list', attrib={
                    'text:style-name': 'rststyle-bulletlist',
                    })
                self.list_style_stack.append('rststyle-bulletitem')
        self.set_current_element(el)

    def depart_bullet_list(self, node):
        self.set_to_parent()
        self.list_style_stack.pop()

    def visit_caption(self, node):
        raise nodes.SkipChildren()
        pass

    def depart_caption(self, node):
        pass

    def visit_comment(self, node):
        #ipshell('At visit_comment')
        el = self.append_child('text:p',
            attrib={'text:style-name': 'rststyle-textbody'})
        el1 =  SubElement(el, 'office:annotation', attrib={})
        el2 =  SubElement(el1, 'text:p', attrib={})
        el2.text = node.astext()

    def depart_comment(self, node):
        pass

    def visit_container(self, node):
        styles = node.attributes.get('classes', ())
        if len(styles) > 0:
            self.paragraph_style_stack.append(styles[0])

    def depart_container(self, node):
        #ipshell('At depart_container')
        styles = node.attributes.get('classes', ())
        if len(styles) > 0:
            self.paragraph_style_stack.pop()

    def visit_copyright(self, node):
        el = self.generate_labeled_block(node, 'Copyright: ')
        self.set_current_element(el)

    def depart_copyright(self, node):
        self.set_to_parent()

    def visit_date(self, node):
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-textbody'})
        el1 = SubElement(el, 'text:span',
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
        #ipshell('At depart_decoration')
        pass

    def visit_definition(self, node):
        el = self.append_child('text:p',
            attrib={'text:style-name': 'rststyle-blockindent'})
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
        el1 = SubElement(el, 'text:span',
            attrib={'text:style-name': 'rststyle-strong'})
        #el1.text = node.astext()
        self.set_current_element(el1)

    def depart_term(self, node):
        self.set_to_parent()
        self.set_to_parent()

    def visit_document(self, node):
        #ipshell('At visit_document')
        #self.set_current_element(self.content_tree.getroot())
        pass

    def depart_document(self, node):
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
        el = SubElement(self.current_element, 'text:span',
            attrib={'text:style-name': 'rststyle-emphasis'})
        self.set_current_element(el)

    def depart_emphasis(self, node):
        self.set_to_parent()

    def visit_enumerated_list(self, node):
        if self.blockstyle == 'rststyle-blockquote':
            el = SubElement(self.current_element, 'text:list', attrib={
                'text:style-name': 'rststyle-blockquote-enumlist',
                })
            self.list_style_stack.append('rststyle-blockquote-enumitem')
        elif self.blockstyle == 'rststyle-highlights':
            el = SubElement(self.current_element, 'text:list', attrib={
                'text:style-name': 'rststyle-highlights-enumlist',
                })
            self.list_style_stack.append('rststyle-highlights-enumitem')
        elif self.blockstyle == 'rststyle-epigraph':
            el = SubElement(self.current_element, 'text:list', attrib={
                'text:style-name': 'rststyle-epigraph-enumlist',
                })
            self.list_style_stack.append('rststyle-epigraph-enumitem')
        else:
            el = SubElement(self.current_element, 'text:list', attrib={
                'text:style-name': 'rststyle-enumlist',
                })
            self.list_style_stack.append('rststyle-enumitem')
        self.set_current_element(el)

    def depart_enumerated_list(self, node):
        self.set_to_parent()
        self.list_style_stack.pop()

    def visit_list_item(self, node):
        #ipshell('At visit_list_item')
        el = self.append_child('text:list-item')
        self.paragraph_style_stack.append(self.list_style_stack[-1])
        self.set_current_element(el)

    def depart_list_item(self, node):
        self.set_to_parent()
        self.paragraph_style_stack.pop()

    def visit_header(self, node):
        #ipshell('At visit_header')
        self.in_header = True

    def depart_header(self, node):
        #ipshell('At depart_header')
        self.in_header = False

    def visit_footer(self, node):
        #ipshell('At visit_footer')
        self.in_footer = True

    def depart_footer(self, node):
        #ipshell('At depart_footer')
        self.in_footer = False

    def visit_field(self, node):
        pass
##        # Note that the syntaxhighlight directive produces this field node.
##        # See class SyntaxHighlight, above.
##        #ipshell('At visit_field')
##        children = node.children
##        if len(children) == 2:
##            name = children[0].astext()
##            if name == 'syntaxhighlight':
##                body = children[1].astext()
##                args = body.split()
##                if args[0] == 'on':
##                    self.syntaxhighlighting = 1
##                elif args[0] == 'off':
##                    self.syntaxhighlighting = 0
##                else:
##                    self.syntaxhighlight_lexer = args[0]
##                raise nodes.SkipChildren()

    def depart_field(self, node):
        pass

    def visit_field_list(self, node):
        #ipshell('At visit_field_list')
        pass

    def depart_field_list(self, node):
        #ipshell('At depart_field_list')
        pass

    def visit_field_name(self, node):
        #ipshell('At visit_field_name')
        #self.trace_visit_node(node)
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-textbody'})
        el1 = SubElement(el, 'text:span',
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

    def visit_figure(self, node):
        #ipshell('At visit_figure')
        #self.trace_visit_node(node)
        pass

    def depart_figure(self, node):
        #self.trace_depart_node(node)
        pass

    def visit_footnote(self, node):
        #ipshell('At visit_footnote')
        ids = node.attributes['ids']
        el1 = None
        for id in ids:
            if id in self.footnote_dict:
                el1 = self.footnote_dict[id]
                break
        if el1 is not None:
            el2 = SubElement(el1, 'text:note-body')
            self.paragraph_style_stack.append('rststyle-footnote')
            self.set_current_element(el2)
            self.footnote_found = True
        else:
            self.footnote_found = False

    def depart_footnote(self, node):
        #ipshell('At depart_footnote')
        if self.footnote_found:
            self.current_element.text = ''
            self.set_to_parent()
            self.set_to_parent()

    def visit_footnote_reference(self, node):
        #ipshell('At visit_footnote_reference')
        id = node.attributes['refid']
        el1 = self.append_child('text:note', attrib={
            'text:id': '%s' % (id, ),
            'text:note-class': 'footnote',
            })
        child = node.children[0]
        label = child.astext()
        el2 = SubElement(el1, 'text:note-citation', attrib={
            'text:label': label,
            })
        el2.text = label
        self.footnote_dict[id] = el1

    def depart_footnote_reference(self, node):
        #ipshell('At depart_footnote_reference')
        pass

    def visit_label(self, node):
        #ipshell('At visit_label')
        pass

    def depart_label(self, node):
        #ipshell('At depart_label')
        pass

    def visit_generated(self, node):
        pass

    def depart_generated(self, node):
        pass

    def check_file_exists(self, path):
        if os.path.exists(path):
            return 1
        else:
            return 0

    def visit_image(self, node):
        #ipshell('At visit_image')
        #self.trace_visit_node(node)
        # Capture the image file.
        if 'uri' in node.attributes:
            source = node.attributes['uri']
            if not self.check_file_exists(source):
                print 'Error: Cannot find image file %s.' % (source, )
                return
        else:
            return
        if source in self.image_dict:
            filename, destination = self.image_dict[source]
        else:
            self.image_count += 1
            filename = os.path.split(source)[1]
            destination = 'Pictures/1%08x%s' % (self.image_count, filename, )
            spec = (source, destination,)
            self.embedded_file_list.append(spec)
            self.image_dict[source] = (filename, destination,)
        # Is this a figure (containing an image) or just a plain image?
        if isinstance(node.parent, docutils.nodes.figure):
            self.generate_figure(node, source, destination)
        else:
            if self.in_paragraph:
                el1 = self.current_element
            else:
                el1 = SubElement(self.current_element, 'text:p',
                    attrib={'text:style-name': 'rststyle-textbody'})
            self.generate_image(node, source, destination, el1)

    def generate_figure(self, node, source, destination):
        caption = None
        for node1 in node.parent.children:
            if node1.tagname == 'caption':
                caption = node1.astext()
        self.image_style_count += 1
        style_name = 'rstframestyle%d' % self.image_style_count
        if 'scale' in node.attributes:
            try:
                scale = int(node.attributes['scale'])
                if scale < 1 or scale > 100:
                    raise ValueError
                scale = scale * 0.01
            except ValueError, e:
                print 'Error: Invalid scale for image: "%s"' % (
                    node.attributes['scale'], )
        else:
            scale = 1.0
        width = None
        if 'width' in node.attributes:
            try:
                width = int(node.attributes['width'])
                width = width * (35.278 / 1000.0)
                width *= scale
                #attrib['svg:width'] = '%.2fcm' % (width, )
            except ValueError, e:
                print 'Error: Invalid width for image: "%s"' % (
                    node.attributes['width'], )
        height = None
        if 'height' in node.attributes:
            try:
                height = int(node.attributes['height'])
                height = height * (35.278 / 1000.0)
                height *= scale
                #attrib['svg:height'] = '%.2fcm' % (height, )
            except ValueError, e:
                print 'Error: Invalid height for image: "%s"' % (
                    node.attributes['height'], )
        # Add the styles
        attrib = {
            'style:name': style_name,
            'style:family': 'graphic',
            'style:parent-style-name': 'Frame',
            }
        el1 = SubElement(self.automatic_styles, 
            'style:style', attrib=attrib, nsdict=SNSD)
        halign = 'center'
        valign = 'top'
        if 'align' in node.attributes:
            align = node.attributes['align'].split()
            for val in align:
                if val in ('left', 'center', 'right'):
                    halign = val
                elif val in ('top', 'middle', 'bottom'):
                    valign = val
        attrib = {
            'fo:margin-left': '0cm',
            'fo:margin-right': '0cm',
            'fo:margin-top': '0cm',
            'fo:margin-bottom': '0cm',
            'style:wrap': 'dynamic',
            'style:number-wrapped-paragraphs': 'no-limit',
            'style:vertical-pos': valign,
            'style:vertical-rel': 'paragraph',
            'style:horizontal-pos': halign,
            'style:horizontal-rel': 'paragraph',
            'fo:padding': '0cm',
            'fo:border': 'none',
            }
        #ipshell('At generate_figure')
        el2 = SubElement(el1,
            'style:graphic-properties', attrib=attrib, nsdict=SNSD)
        # Add the content
        attrib = {'text:style-name': 'rststyle-textbody'}
        el1 = SubElement(self.current_element, 'text:p', attrib=attrib)
        attrib = {
            'draw:style-name': style_name,
            'draw:name': 'Frame1',
            'text:anchor-type': 'paragraph',
            'draw:z-index': '1',
            }
        if width is not None:
            attrib['svg:width'] = '%.2fcm' % (width, )
        el1 = SubElement(el1, 'draw:frame', attrib=attrib)
        attrib = {}
        if height is not None:
            attrib['fo:min-height'] = '%.2fcm' % (height, )
        el1 = SubElement(el1, 'draw:text-box', attrib=attrib)
        attrib = {'text:style-name': 'rststyle-caption', }
        el1 = SubElement(el1, 'text:p', attrib=attrib)
        # Add the image (frame) inside the figure/caption frame.
        #ipshell('At visit_image #1')
        el2 = self.generate_image(node, source, destination, el1)
        if caption:
            el2.tail = caption

    def generate_image(self, node, source, destination, current_element):
        #ipshell('At generate_image')
        self.image_style_count += 1
        style_name = 'rstframestyle%d' % self.image_style_count
        # Add the style.
        attrib = {
            'style:name': style_name,
            'style:family': 'graphic',
            'style:parent-style-name': 'Graphics',
            }
        el1 = SubElement(self.automatic_styles, 
            'style:style', attrib=attrib, nsdict=SNSD)
        halign = None
        valign = None
        if 'align' in node.attributes:
            align = node.attributes['align'].split()
            for val in align:
                if val in ('left', 'center', 'right'):
                    halign = val
                elif val in ('top', 'middle', 'bottom'):
                    valign = val
        attrib = {
            'style:vertical-pos': 'top',
            'style:vertical-rel': 'paragraph',
            #'style:horizontal-pos': halign,
            #'style:vertical-pos': valign,
            'style:horizontal-rel': 'paragraph',
            'style:mirror': 'none',
            'fo:clip': 'rect(0cm 0cm 0cm 0cm)',
            'draw:luminance': '0%',
            'draw:contrast': '0%',
            'draw:red': '0%',
            'draw:green': '0%',
            'draw:blue': '0%',
            'draw:gamma': '100%',
            'draw:color-inversion': 'false',
            'draw:image-opacity': '100%',
            'draw:color-mode': 'standard',
            }
        if halign is not None:
            attrib['style:horizontal-pos'] = halign
        if valign is not None:
            attrib['style:vertical-pos'] = valign
        #ipshell('At generate_image')
        # If we are inside a table, add a no-wrap style.
        if self.is_in_table(node):
            attrib['style:wrap'] = 'none'
        el2 = SubElement(el1,
            'style:graphic-properties', attrib=attrib, nsdict=SNSD)
        # Add the content.
        #el = SubElement(current_element, 'text:p',
        #    attrib={'text:style-name': 'rststyle-textbody'})
        attrib={
            'draw:style-name': style_name,
            'draw:name': 'graphics2',
            #'text:anchor-type': 'paragraph',
            #'svg:width': '%fcm' % (width, ),
            #'svg:height': '%fcm' % (height, ),
            'draw:z-index': '1',
            }
        if isinstance(node.parent, nodes.TextElement):
            attrib['text:anchor-type'] = 'char'
        else:
            attrib['text:anchor-type'] = 'paragraph'
        if 'scale' in node.attributes:
            try:
                scale = int(node.attributes['scale'])
                if scale < 1: # or scale > 100:
                    raise ValueError
                scale = scale * 0.01
            except ValueError, e:
                print 'Error: Invalid scale for image: "%s"' % (
                    node.attributes['scale'], )
        else:
            scale = 1.0
        if 'width' in node.attributes:
            try:
                width = int(node.attributes['width'])
                width = width * (35.278 / 1000.0)
                width *= scale
                attrib['svg:width'] = '%.2fcm' % (width, )
            except ValueError, e:
                print 'Error: Invalid width for image: "%s"' % (
                    node.attributes['width'], )
        if 'height' in node.attributes:
            try:
                height = int(node.attributes['height'])
                height = height * (35.278 / 1000.0)
                height *= scale
                attrib['svg:height'] = '%.2fcm' % (height, )
            except ValueError, e:
                print 'Error: Invalid height for image: "%s"' % (
                    node.attributes['height'], )
        if ('scale' in node.attributes and
            ('width' not in node.attributes or
            'height' not in node.attributes)):
            if Image is not None:
                if source in self.image_dict:
                    filename, destination = self.image_dict[source]
                    imageobj = Image.open(filename, 'r')
                    width, height = imageobj.size
                    width = width * (35.278 / 1000.0)
                    width *= scale
                    attrib['svg:width'] = '%.2fcm' % (width, )
                    height = height * (35.278 / 1000.0)
                    height *= scale
                    attrib['svg:height'] = '%.2fcm' % (height, )
            else:
                raise RuntimeError, 'image has scale and no height/width and PIL not installed'
        el1 = SubElement(current_element, 'draw:frame', attrib=attrib)
        el2 = SubElement(el1, 'draw:image', attrib={
            'xlink:href': '%s' % (destination, ),
            'xlink:type': 'simple',
            'xlink:show': 'embed',
            'xlink:actuate': 'onLoad',
            })
        return el1

    def depart_image(self, node):
        pass

    def is_in_table(self, node):
        node1 = node.parent
        while node1:
            if isinstance(node1, docutils.nodes.entry):
                return True
            node1 = node1.parent
        return False

    def visit_line(self, node):
        #ipshell('At visit_line')
        pass

    def depart_line(self, node):
        #ipshell('At depart_line')
        pass

    def visit_line_block(self, node):
        #ipshell('At visit_line_block')
        s1 = node.astext()
        lines = s1.split('\n')
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-lineblock'})
        el.text = lines[0]
        first = True
        if len(lines) > 1:
            for line in lines[1:]:
                if line == '':
                    if first:
                        first = False
                        continue
                first = True
                el1 = SubElement(el, 'text:line-break')
                el1.tail = line

    def depart_line_block(self, node):
        #ipshell('At depart_line_block')
        pass

    def visit_literal(self, node):
        #ipshell('At visit_literal')
        el = SubElement(self.current_element, 'text:span',
            attrib={'text:style-name': 'rststyle-inlineliteral'})
        self.set_current_element(el)

    def depart_literal(self, node):
        self.set_to_parent()

    def _calculate_code_block_padding(self, line):
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

    def _add_syntax_highlighting(self, insource, language):
        #print '(_add_syntax_highlighting) using lexer: %s' % (language, )
        lexer = pygments.lexers.get_lexer_by_name(language, stripall=True)
        if language in ('latex', 'tex'):
            fmtr = OdtPygmentsLaTeXFormatter()
        else:
            fmtr = OdtPygmentsProgFormatter()
        outsource = pygments.highlight(insource, lexer, fmtr)
        return outsource

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

    wrapper1 = '<text:p text:style-name="rststyle-codeblock">%s</text:p>'

    def visit_literal_block(self, node):
        #ipshell('At visit_literal_block')
        #import pdb; pdb.set_trace()
        source = node.astext()
        if (pygments and 
            self.settings.add_syntax_highlighting and
            node.get('hilight', False)):
            language = node.get('language', 'python')
            source = self._add_syntax_highlighting(source, language)
        else:
            source = escape_cdata(source)
        lines = source.split('\n')
        lines1 = ['<wrappertag1 xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0">']

        my_lines = []
        for my_line in lines:
            my_line = self.fill_line(my_line)
            my_line = my_line.replace("&#10;", "\n")
            my_lines.append(my_line)
        my_lines_str = '<text:line-break/>'.join(my_lines)
        my_lines_str2 = ODFTranslator.wrapper1 % (my_lines_str, )
        lines1.append(my_lines_str2)
        lines1.append('</wrappertag1>')
        s1 = ''.join(lines1)
        el1 = etree.fromstring(s1)
        children = el1.getchildren()
        for child in children:
            self.current_element.append(child)

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
            el = SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': table_name,
                'style:family': 'table'}, nsdict=SNSD)
            el1 = SubElement(el, 'style:table-properties', attrib={
                'style:width': '17.59cm',
                'table:align': 'left',
                'style:shadow': 'none'}, nsdict=SNSD)
            el = SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.A' % table_name,
                'style:family': 'table-column'}, nsdict=SNSD)
            el1 = SubElement(el, 'style:table-column-properties', attrib={
                'style:column-width': '4.999cm'}, nsdict=SNSD)
            el = SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.B' % table_name,
                'style:family': 'table-column'}, nsdict=SNSD)
            el1 = SubElement(el, 'style:table-column-properties', attrib={
                'style:column-width': '12.587cm'}, nsdict=SNSD)
            el = SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.A1' % table_name,
                'style:family': 'table-cell'}, nsdict=SNSD)
            el1 = SubElement(el, 'style:table-cell-properties', attrib={
                'fo:background-color': 'transparent',
                'fo:padding': '0.097cm',
                'fo:border-left': '0.035cm solid #000000',
                'fo:border-right': 'none',
                'fo:border-top': '0.035cm solid #000000',
                'fo:border-bottom': '0.035cm solid #000000'}, nsdict=SNSD)
            el2 = SubElement(el1, 'style:background-image', nsdict=SNSD)
            el = SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.B1' % table_name,
                'style:family': 'table-cell'}, nsdict=SNSD)
            el1 = SubElement(el, 'style:table-cell-properties', attrib={
                'fo:padding': '0.097cm',
                'fo:border': '0.035cm solid #000000'}, nsdict=SNSD)
            el = SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.A2' % table_name,
                'style:family': 'table-cell'}, nsdict=SNSD)
            el1 = SubElement(el, 'style:table-cell-properties', attrib={
                'fo:padding': '0.097cm',
                'fo:border-left': '0.035cm solid #000000',
                'fo:border-right': 'none',
                'fo:border-top': 'none',
                'fo:border-bottom': '0.035cm solid #000000'}, nsdict=SNSD)
            el = SubElement(self.automatic_styles, 'style:style', attrib={
                'style:name': '%s.B2' % table_name,
                'style:family': 'table-cell'}, nsdict=SNSD)
            el1 = SubElement(el, 'style:table-cell-properties', attrib={
                'fo:padding': '0.097cm',
                'fo:border-left': '0.035cm solid #000000',
                'fo:border-right': '0.035cm solid #000000',
                'fo:border-top': 'none',
                'fo:border-bottom': '0.035cm solid #000000'}, nsdict=SNSD)
        #
        # Generate table data
        el = self.append_child('table:table', attrib={
            'table:name': table_name,
            'table:style-name': table_name,
            })
        el1 = SubElement(el, 'table:table-column', attrib={
            'table:style-name': '%s.A' % table_name})
        el1 = SubElement(el, 'table:table-column', attrib={
            'table:style-name': '%s.B' % table_name})
        el1 = SubElement(el, 'table:table-header-rows')
        el2 = SubElement(el1, 'table:table-row')
        el3 = SubElement(el2, 'table:table-cell', attrib={
            'table:style-name': '%s.A1' % table_name,
            'office:value-type': 'string'})
        el4 = SubElement(el3, 'text:p', attrib={
            'text:style-name': 'Table_20_Heading'})
        el4.text= 'Option'
        el3 = SubElement(el2, 'table:table-cell', attrib={
            'table:style-name': '%s.B1' % table_name,
            'office:value-type': 'string'})
        el4 = SubElement(el3, 'text:p', attrib={
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
        el1 = SubElement(el, 'text:p', attrib={
            'text:style-name': 'Table_20_Contents'})
        el1.text = node.astext()
        raise nodes.SkipChildren()

    def depart_description(self, node):
        pass

    def visit_paragraph(self, node):
        #ipshell('At visit_paragraph')
        #self.trace_visit_node(node)
        self.in_paragraph = True
        if self.omit:
            return
        if self.in_header:
            style_name = 'rststyle-header'
            el = self.append_child('text:p',
                attrib={'text:style-name': style_name})
        elif self.in_footer:
            style_name = 'rststyle-footer'
            el = self.append_child('text:p',
                attrib={'text:style-name': style_name})
        else:
            style_name = self.paragraph_style_stack[-1]
            el = self.append_child('text:p',
                attrib={'text:style-name': style_name})
        self.set_current_element(el)

    def depart_paragraph(self, node):
        #ipshell('At depart_paragraph')
        #self.trace_depart_node(node)
        self.in_paragraph = False
        if self.omit:
            return
        self.set_to_parent()
        if self.in_header:
            self.header_content.append(self.current_element.getchildren()[-1])
            self.current_element.remove(self.current_element.getchildren()[-1])
        elif self.in_footer:
            self.footer_content.append(self.current_element.getchildren()[-1])
            self.current_element.remove(self.current_element.getchildren()[-1])

    def visit_problematic(self, node):
        #print '(visit_problematic) node: %s' % (node.astext(), )
        pass

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
        el1 = SubElement(el, 'text:span',
            attrib={'text:style-name': 'rststyle-strong'})
        el1.text = 'Revision: '
        el1.tail = node.astext()

    def depart_revision(self, node):
        pass

    def visit_rubric(self, node):
        style_name = 'rststyle-rubric'
        classes = node.get('classes')
        if classes:
            class1 = classes[0]
            if class1:
                style_name = class1
        el = SubElement(self.current_element, 'text:h', attrib = {
            #'text:outline-level': '%d' % section_level,
            #'text:style-name': 'Heading_20_%d' % section_level,
            'text:style-name': style_name,
            })
        text = node.astext()
        el.text = self.encode(text)

    def depart_rubric(self, node):
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
        el = SubElement(self.current_element, 'text:span',
            attrib={'text:style-name': 'rststyle-strong'})
        self.set_current_element(el)

    def depart_strong(self, node):
        self.set_to_parent()

    def visit_substitution_definition(self, node):
        #ipshell('At visit_substitution_definition')
        raise nodes.SkipChildren()
        pass

    def depart_substitution_definition(self, node):
        #ipshell('At depart_substitution_definition')
        pass

    def visit_system_message(self, node):
        #print '(visit_system_message) node: %s' % (node.astext(), )
        pass

    def depart_system_message(self, node):
        pass

    def visit_table(self, node):
        #self.trace_visit_node(node)
        #ipshell('At visit_table')
        self.table_count += 1
        table_name = '%s%d' % (TableStylePrefix, self.table_count, )
        el1 = SubElement(self.automatic_styles, 'style:style', attrib={
            'style:name': '%s' % table_name,
            'style:family': 'table',
            }, nsdict=SNSD)
        el1_1 = SubElement(el1, 'style:table-properties', attrib={
            #'style:width': '17.59cm',
            'table:align': 'margins',
            }, nsdict=SNSD)
        # We use a single cell style for all cells in this table.
        # That's probably not correct, but seems to work.
        el2 = SubElement(self.automatic_styles, 'style:style', attrib={
            'style:name': '%s.A1' % table_name,
            'style:family': 'table-cell',
            }, nsdict=SNSD)
        line_style1 = '0.%03dcm solid #000000' % self.settings.table_border_thickness
        el2_1 = SubElement(el2, 'style:table-cell-properties', attrib={
            'fo:padding': '0.049cm',
            'fo:border-left': line_style1,
            'fo:border-right': line_style1,
            'fo:border-top': line_style1,
            'fo:border-bottom': line_style1,
            }, nsdict=SNSD)
        title = None
        for child in node.children:
            if child.tagname == 'title':
                title = child.astext()
                break
        if title is not None:
            el3 = self.append_child('text:p', attrib={
                'text:style-name': 'rststyle-table-title',
                })
            el3.text = title
        else:
            #print 'no table title'
            pass
        el4 = SubElement(self.current_element, 'table:table', attrib={
            'table:name': '%s' % table_name,
            'table:style-name': '%s' % table_name,
            })
        self.set_current_element(el4)
        self.current_table_style = el1
        self.table_width = 0

    def depart_table(self, node):
        #self.trace_depart_node(node)
        #ipshell('At depart_table')
        attribkey = add_ns('style:width', nsdict=SNSD)
        attribval = '%dcm' % self.table_width
        self.current_table_style.attrib[attribkey] = attribval
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
        el1 = SubElement(self.automatic_styles, 'style:style', attrib={
            'style:name': colspec_name,
            'style:family': 'table-column',
            }, nsdict=SNSD)
        el1_1 = SubElement(el1, 'style:table-column-properties', attrib={
            'style:column-width': '%dcm' % colwidth }, nsdict=SNSD)
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
        attrib={
            'table:style-name': cellspec_name,
            'office:value-type': 'string',
            }
        morecols = node.get('morecols', 0)
        if morecols > 0:
            attrib['table:number-columns-spanned'] = '%d' % (morecols + 1,)
            self.column_count += morecols
        el1 = self.append_child('table:table-cell', attrib=attrib)
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
            el1 = SubElement(self.current_element, 'text:h', attrib = {
                'text:outline-level': '%d' % section_level,
                #'text:style-name': 'Heading_20_%d' % section_level,
                'text:style-name': 'rststyle-heading%d' % section_level,
                })
            text = node.astext()
            # encode/decode
            #el1.text = text.decode('latin-1').encode('utf-8')
            el1.text = self.encode(text)
        elif isinstance(node.parent, docutils.nodes.document):
            if self.settings.title:
                text = self.settings.title
            else:
                text = node.astext()
            el1 = SubElement(self.current_element, 'text:h', attrib = {
                'text:outline-level': '1',
                'text:style-name': 'rststyle-heading1',
                })
            # encode/decode
            #text = text.decode('latin-1').encode('utf-8')
            el1.text = text
            self.title = text
            self.found_doc_title = True

    def depart_title(self, node):
        pass

    visit_subtitle = visit_title
    depart_subtitle = depart_title
    
    def visit_title_reference(self, node):
        #ipshell('At visit_title_reference')
        el = self.append_child('text:span', attrib={
            'text:style-name': 'rststyle-quotation'})
        el.text = self.encode(node.astext())

    def depart_title_reference(self, node):
        pass

    def visit_topic(self, node):
        #ipshell('At visit_topic')
        if 'classes' in node.attributes:
            if 'contents' in node.attributes['classes']:
                el = self.append_child('text:p', attrib={
                    'text:style-name': 'rststyle-horizontalline'})
                el = self.append_child('text:p', attrib={
                    'text:style-name': 'rststyle-centeredtextbody'})
                el1 = SubElement(el, 'text:span',
                    attrib={'text:style-name': 'rststyle-strong'})
                el1.text = 'Contents'
                self.in_table_of_contents = True
            elif 'abstract' in node.attributes['classes']:
                el = self.append_child('text:p', attrib={
                    'text:style-name': 'rststyle-horizontalline'})
                el = self.append_child('text:p', attrib={
                    'text:style-name': 'rststyle-centeredtextbody'})
                el1 = SubElement(el, 'text:span',
                    attrib={'text:style-name': 'rststyle-strong'})
                el1.text = 'Abstract'

    def depart_topic(self, node):
        #ipshell('At depart_topic')
        if 'classes' in node.attributes:
            if 'contents' in node.attributes['classes']:
                el = self.append_child('text:p', attrib={
                    'text:style-name': 'rststyle-horizontalline'})
                self.in_table_of_contents = False

    def visit_transition(self, node):
        el = self.append_child('text:p', attrib={
            'text:style-name': 'rststyle-horizontalline'})

    def depart_transition(self, node):
        pass

    #
    # Admonitions
    #
    def visit_warning(self, node):
        self.generate_admonition(node, 'warning')

    def depart_warning(self, node):
        self.paragraph_style_stack.pop()

    def visit_attention(self, node):
        self.generate_admonition(node, 'attention')

    depart_attention = depart_warning

    def visit_caution(self, node):
        self.generate_admonition(node, 'caution')

    depart_caution = depart_warning

    def visit_danger(self, node):
        self.generate_admonition(node, 'danger')

    depart_danger = depart_warning

    def visit_error(self, node):
        self.generate_admonition(node, 'error')

    depart_error = depart_warning

    def visit_hint(self, node):
        self.generate_admonition(node, 'hint')

    depart_hint = depart_warning

    def visit_important(self, node):
        self.generate_admonition(node, 'important')

    depart_important = depart_warning

    def visit_note(self, node):
        self.generate_admonition(node, 'note')

    depart_note = depart_warning

    def visit_tip(self, node):
        self.generate_admonition(node, 'tip')

    depart_tip = depart_warning

    def visit_admonition(self, node):
        #import pdb; pdb.set_trace()
        title = None
        for child in node.children:
            if child.tagname == 'title':
                title = child.astext()
        if title is None:
            classes1 = node.get('classes')
            if classes1:
                title = classes1[0]
        self.generate_admonition(node, 'generic', title)

    depart_admonition = depart_warning

    def generate_admonition(self, node, label, title=None):
        el1 = SubElement(self.current_element, 'text:p', attrib = {
            'text:style-name': 'rststyle-admon-%s-hdr' % (label, ),
            })
        if title:
            el1.text = title
        else:
            el1.text = '%s!' % (label.capitalize(), )
        s1 = 'rststyle-admon-%s-body' % (label, )
        self.paragraph_style_stack.append(s1)

