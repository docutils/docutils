#!/usr/bin/python

"""
rst2chm.py - Converts reStructuredText into Microsoft (TM) HTML Help.

Copyright (C) 2003 by Ollie Rutherfurd <oliver@rutherfurd.net>

License: Python license
"""

import glob
import optparse
import os
import sys

import docutils.core
import docutils.io
from docutils import nodes, writers

__version__ = '0.3'

# ---------------------------------------------------------------------------
# default location of HTML Help compiler

CHM_COMPILER = 'C:\\Program Files\\HTML Help Workshop\\hhc.exe'

# ---------------------------------------------------------------------------
# template for HHC (contents file)

HHC_HEADER = """\
<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
<HTML>
<HEAD>
<meta name="GENERATOR" content="rst2chm v%s">
<!-- Sitemap 1.0 -->
</HEAD><BODY>
<OBJECT type="text/site properties">
	<param name="Auto Generated" value="No">
</OBJECT>
""" % __version__

HHC_ITEM = """\
<LI><OBJECT type="text/sitemap">
    <param name="Name" value="%(name)s">
    <param name="Local" value="%(href)s">
</OBJECT></LI>
"""

HHC_FOOTER = """\
</BODY></HTML>
"""

HHP_TEMPLATE = """\
[OPTIONS]
Compatibility=1.1 or later
Compiled file=%(chm_file)s
Contents file=%(hhc_file)s
Default topic=%(default_topic)s
Display compile progress=%(display_compile_progress)s
Full-text search=%(full_text_search_on)s
Language=%(language)s
Title=%(title)s

[FILES]
%(files)s

[INFOTYPES]
"""

# ---------------------------------------------------------------------------
# just a couple languages (those supported by docutils)
#
# mappings (from "htmlhelp\langcodes.xml" in docbook xsl stylesheets)
#
# need to add the rest, but first need to figure out how to handle
# multiple mappings for the same abbreviation

LANGUAGES = {
    'af': '0x0436 Afrikaans',
    'de': '0x0407 German (Germany)',
    'en': '0x0409 English (United States)',
    'es': '0x040a Spanish (Traditional Sort)',
    'fr': '0x040c French (France)',
    'it': '0x0410 Italian',
    'ru': '0x0419 Russian',
    'sk': '0x041b Slovak',
    'sv': '0x041d Swedish',
}

# ---------------------------------------------------------------------------
# custom usage message (as we've got positional args)

USAGE = """usage: %prog [options] OUTPUT_FILE INTPUT_FILE [INPUT_FILE2 ...]"""

# ---------------------------------------------------------------------------
# option group setting spec

SETTINGS_SPEC = (
    'rst2chm-Specific Options',
    None,
    (
        ('HTML Help File Title.  Default is name of HTML Help File.',
        ['--title'],
        {'default': None}),
        
        ('Default topic.  Default is first given file.',
        ['--default-topic'],
        {'default': None, 'metavar': '<file>'}),
        
        ('Path to HTML Help Compiler (hhc.exe).  Required if hhc.exe not in '
         ' PATH or `%s`.' % CHM_COMPILER,
        ['--compiler'],
        {'default': 'hhc.exe', 'dest': 'hhc', 'metavar': '<file>'}),
        
        ('Disable full-text searching in generated CHM file.  Full-text'
         ' searching enabled by default.',
        ['--no-full-text-search'],
        {'default': 1, 'dest': 'full_text_search', 'action': 'store_false'}),
        
        ('Specify the language of input text (ISO 639 2-letter identifier).'
          '  Default is "en" (English).',
        ['--language', '-l'], 
        {'dest': 'language_code', 'default': 'en', 'metavar': '<name>'}),
        
        ("Relative path to CSS file use with HTML.",
        ['--stylesheet'],
        {'default': None, 'metavar': 'FILENAME'}),
        
        ("Don't generate HTML files from reST source files."
         " This is useful if you wish to customize the HTML generation.",
        ['--no-html'],
        {'default': '1', 'dest': 'generate_html', 'action': 'store_false'}),
        
        ("Don't remove generated .hhc & .hhp files.  By default these files" 
         " are deleted.",
        ['--no-clean'],
        {'default': '1', 'dest': 'clean', 'action': 'store_false'}),

        ('No status messages.  Default verbose output.',
        ['-q','--quiet'],
        {'default': 1, 'dest': 'verbose', 'action': 'store_false'}),

    )
)

# ---------------------------------------------------------------------------
# overrides for HTML writer

HTML_WRITER_OVERRIDES ={
    'xml_declaration': '',              # hhc.exe doesn't like it
    'stylesheet_path': 'default.css',
    'embed_stylesheet': '',             # will be set to "yes", if stylesheet given
    'language_code':'en',
    'output_encoding': 'iso-8859-1',
}

# ---------------------------------------------------------------------------
# exceptions

class CHMCompileError(Exception):
    """
    Exception raised when chm file can't be generated.
    """
    pass

# ---------------------------------------------------------------------------
# docutils Writer for HTML Help index file

class Writer(writers.Writer):

    output = None

    section_filename = ''
    """name of HTML file to be generated
    
    This is used to reference the location of the section, 
    for example::

        foo.html#section-1
    """

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = HTMLHelpContentsTranslator
        self.section_filename = ''

    def translate(self):
        visitor = HTMLHelpContentsTranslator(self.document)
        visitor.section_filename = self.section_filename
        self.document.walkabout(visitor)
        self.output = visitor.astext()

# ---------------------------------------------------------------------------
# docutils NodeVisitor that writes .hhc contents for a single file

class HTMLHelpContentsTranslator(nodes.NodeVisitor):

    """
    HTMLHelpContentsTranslator collects sections and titles
    for the HHC file.
    """

    def __init__(self, document):
        self.section_filename = ''
        self.document = document
        self.content = []
        self.section_level = 0

    def astext(self):
        return ''.join(self.content)

    def encode(self, text):
        """Encode special characters in `text` & return."""
        # taken from htmlcss1 writer
        # @@@ A codec to do these and all other HTML entities would be nice.
        text = text.replace("&", "&amp;")
        text = text.replace("<", "&lt;")
        text = text.replace('"', "&quot;")
        text = text.replace(">", "&gt;")
        text = text.replace("@", "&#64;") # may thwart some address harvesters
        return text

    def visit_document(self, node):
        self.content.append('<UL>\n')
    def depart_document(self, node):
        self.content.append('</UL>\n')

    def visit_section(self, node):
        self.section_level += 1
        self.content.append('<UL>\n')
    def depart_section(self, node):
        self.content.append('</UL>\n')
        self.section_level -= 1

    def visit_title(self, node):
        name,href = self.encode(node.astext()),''
        # only want to collection document and section titles
        if isinstance(node.parent, nodes.document):
            pass
        elif isinstance(node.parent, nodes.section):
            pass
        else:
            return

        if self.section_level == 0:
            href = self.section_filename
        else:
            # only add this to contents if we can link to it
            # and to link to it, 'id' is needed
            if len(node.parent['ids'])>0:
                href = self.section_filename + '#' + node.parent['ids'][0]
            else:
                return
        self.content.append(HHC_ITEM  % {'name': name, 'href': href})

    def depart_title(self, node):
        pass

    def unknown_visit(self, node):
        pass
    def unknown_departure(self, node):
        pass


def status(msg, options):
    """
    If `options.verbose`, `msg` is written to `sys.stdout`.
    """
    if options.verbose:
        sys.stderr.write(msg)


def txt2htmlfilename(filename):
    """
    Returns HTML filename for given txt filename.
    """
    return os.path.splitext(filename.replace('\\','/'))[0] + '.html'


def write_contents_file(filenames, options):
    """
    Creates .hhc (HTML Help Contents file).

    Uses a custom docutils.writer.Writer to extract
    section names and links from reStructuredText 
    source files.

    File created is `options.hhc_file`.
    """
    contents = []
    status('creating contents file (%s...) \n' % options.hhc_file, options)

    for i in range(len(filenames)):
        filename = filenames[i]

        status('* %s (%d of %d)... ' % (filename, i+1, len(filenames)), options)

        # this should really be relative
        html_filename = txt2htmlfilename(filename)
        writer=Writer()
        writer.section_filename = html_filename

        pub = docutils.core.Publisher()
        pub.set_reader('standalone', None, 'restructuredtext')
        pub.writer = writer
        settings = pub.get_settings(output_encoding='iso-8859-1')
        pub.settings._destination = ''
        pub.source = docutils.io.FileInput(source_path=filename, 
                                           encoding=settings.input_encoding)
        pub.destination = docutils.io.StringOutput(
                                encoding=settings.output_encoding)
        pub.document = pub.reader.read(pub.source, pub.parser, pub.settings)
        pub.apply_transforms()
        output = pub.writer.write(pub.document, pub.destination)
        pub.writer.assemble_parts()
        contents.append(output)

        status('OK\n', options)

    f = open(options.hhc_file,'w')
    print >> f, HHC_HEADER + ''.join(contents) + HHC_FOOTER
    f.close()


def write_project_file(html_files, options):
    """
    Creates HTML Help Project file (.hpp) file.

    `options.hpp_file` is the name of the created file.
    """
    if not options.default_topic:
        options.default_topic = html_files[0]

    settings = {
        'chm_file': options.chm_file,
        'hhc_file': options.hhc_file,
        'default_topic': options.default_topic,
        'display_compile_progress': ['Yes','No'][not(options.verbose)],
        'full_text_search_on': ['Yes','No'][not(options.full_text_search)],
        'language': LANGUAGES[options.language_code.lower()],
        'title': options.title,
        'files': '\n'.join(html_files),
    }

    status('creating project file (%s...) ' % options.hhp_file, options)

    f = open(options.hhp_file,'w')
    print >> f, HHP_TEMPLATE % settings
    f.close()

    status('OK\n', options)


def create_html_files(filenames, options):
    """
    Generates HTML files from reST source files using html writer.

    returns names of generated files.
    """
    html_files = []
    status('creating HTML files...\n',options)
    for i in range(len(filenames)):
        filename = filenames[i]
        status('* %s (%d of %d)... ' % (filename, i+1, len(filenames)), options)
        html_file = txt2htmlfilename(filename)
        docutils.core.publish_file(source=None, source_path=filename,
                                  destination_path=html_file, 
                                  reader=None, reader_name='standalone',
                                  parser=None, parser_name='restructuredtext',
                                  writer=None, writer_name='html',
                                  settings=None, settings_spec=None,
                                  settings_overrides=HTML_WRITER_OVERRIDES)
        html_files.append(html_file)
        status('OK\n', options)
    return html_files


def compile_chm(options):
    """
    Creates .chm file for `options.hhp_file`.

    Looks for 'hhc.exe' using PATH and the default location.
    If hhc.exe can't be found, or there's an error executing it
    then an `CHMCompileError` exception is raised.

    returns None
    """
    status('compiling %s... ' % options.chm_file, options)

    # search for hhc.exe in PATH, and append default location (for good measure)
    PATH = os.environ['PATH'].split(os.pathsep)
    exes = filter(os.path.isfile, [os.path.join(d,'hhc.exe') for d in PATH])
    exes.append(CHM_COMPILER)

    r = 1   # it appears that hhc.exe returns 1 on success

    for exe in exes:
        try:
            hhp_file = '"' + os.path.abspath(options.hhp_file) + '"'
            r = os.spawnv(os.P_WAIT, exe, [hhp_file,hhp_file])
            if r == 1:
                break
        except OSError,e:
            if e.errno == 2: # FNF
                # if exe not found (CHM_COMPILER) just keep going
                # as an error will be generated later
                continue
            else:
                raise CHMCompileError(str(e))

    if r != 1:
        raise CHMCompileError('hhc.exe not found')

    status('OK\n',options)

# ---------------------------------------------------------------------------
# main() function

def main(args=None):
    if args is None:
        args = sys.argv

    # create option parser, and add rst2chm options
    parser = optparse.OptionParser(usage=USAGE)
    name,description,options = SETTINGS_SPEC
    group = optparse.OptionGroup(parser, name, description)
    for help_text, option_strings, kwargs in options:
        group.add_option(help=help_text, *option_strings, **kwargs)
    parser.add_option_group(group)

    options,args = parser.parse_args(args)

    args = args[1:] # remove script name
    if len(args) < 2:
        parser.print_help()
        sys.exit(2)

    options.chm_file = args[0]  # output file is first arg
    # make sure user puts output file as first argument
    if options.chm_file[-4:].lower() != '.chm':
        print >> sys.stderr, "\nERROR: `%s` does not end with `.chm`.\n" % \
            options.chm_file
        print >> sys.stderr, "use `--help` for usage instructions"
        sys.exit(2)

    # get filenames from command line and expand globs
    filenames = []
    for a in args[1:]:
        filenames.extend(glob.glob(a))

    # use chm filename as base for hhc & hhp filenames
    options.hhc_file = os.path.splitext(options.chm_file)[0] + '.hhc'
    options.hhp_file = os.path.splitext(options.chm_file)[0] + '.hhp'

    if options.stylesheet:
        HTML_WRITER_OVERRIDES['stylesheet_path'] = options.stylesheet
        HTML_WRITER_OVERRIDES['embed_stylesheet'] = 'yes'
    HTML_WRITER_OVERRIDES['language_code'] = options.language_code

    if not options.title:
        options.title = options.chm_file

    write_contents_file(filenames,options)
    if options.generate_html:
        html_files = create_html_files(filenames,options)
    else:
        html_files = [txt2htmlfilename(f) for f in filenames]
    write_project_file(html_files,options)

    compile_chm(options)

    if options.clean:
        status('cleaning up... ', options)
        if options.generate_html:
            map(os.remove, html_files)
        os.remove(options.hhp_file)
        os.remove(options.hhc_file)
        status('OK\n' ,options)

# ---------------------------------------------------------------------------
# 
if __name__ == '__main__':
    try:
        main()
    except CHMCompileError,e:
        print >> sys.stderr, """
Error generating chm file.  

Error: %s

Please ensure you have the HTML Help workshop installed and that hhc.exe \
is in the PATH, or specify the location of hhc.exe using the `--compiler` \
option.""" % str(e)
    except KeyboardInterrupt,e:
        pass


# :indentSize=4:lineSeparator=\r\n:maxLineLen=80:noTabs=true:tabSize=4:deepIndent=true:
