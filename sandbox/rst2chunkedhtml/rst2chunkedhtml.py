#!/usr/bin/env python
# $Id$
"""
:Author: Andras Mohari <mayday at vizslamail dot hu>
:Date: $Date$
:Status: *Experimental*
:Revision: $Revision$
:Copyright: This module has been placed in the public domain.

An HTML chunker for reStructuredText.

Using as a Module
=================

The main class is ``ChunkedHTMLWriter``, everything else is internal to this
module, and should not be used from the outside.  This class can be used
almost the same way as any other DocUtils writer; you can instantiate it and
pass it to the ``docutils.core.publish_*`` functions, for example:

    publish_cmdline(writer=ChunkedHTMLWriter(), ...)

The writer is a bit special, it is a kind of meta-writer, i.e. it does not do
much reST to HTML conversion on its own.  Instead, it instantiates a different
writer (``html4css1.Writer`` by default) to convert chunks of the document
tree to HTML format.  This also means that you do not have to sub-class
``ChunkedHTMLWriter`` if you want to customize HTML output.  See the next
section.

Using Your Own HTML Writer
--------------------------

If you want to use your own HTML writer to convert reST documents to chunked
HTML, you should not sub-class ``ChunkedHTMLWriter``.  Instead, pass your
writer class (not the instance!) as the first argument to the
``ChunkedHTMLWriter`` constructor.  Please note that your writer must have a
``set_external_ids()`` method defined as ::

    def set_external_ids(self, external_ids)

The `external_ids` argument is a mapping of node IDs to URIs (the URIs are
properly quoted).  Whenever your writer needs to convert a node ID to an
``href`` HTML attribute, it has to check if the ID is mapped to an URI.  If it
is, the writer must use the URI, otherwise it must use the ID::

    try:
        href = external_ids[id]
    except KeyError:
        href = '#' + id

Customizing the Navigation Bars
-------------------------------

You can create a custom navigation header and footer for your HTML documents
by passing a callback function as the second argument to the
``ChunkedHTMLWriter`` constructor.  Whenever a chunk is being processed, this
function is called with a dictionary as its sole argument.  The dictionary,
which contains data for the next, previous, parent and root chunks, has the
following keys:

``next``, ``prev``, ``parent``, ``root``
    These are links to the next, previous, parent, and the root chunks,
    respectively.  They are properly quoted ("foo bar.html" will be
    "foo%20bar.html").  If the next, previous or parent link does not exist
    for a chunk, the corresponding value is an empty string.

``next_title``, ``prev_title``, ``parent_title``, ``root_title``
    The title of the next, previous, parent, and root chunk, respectively.
    Note that these are encoded values, so you can safely use them in the
    output (even as element attribute values---but place them inside double
    quotes, not single ones).  In case you need raw, un-encoded titles, you
    can access them as ``next_title_raw``, ``prev_title_raw``, and so on.

The callback must return a tuple or list of Unicode strings: the header and
the footer.  Any of the two can be an empty string.
"""


__docformat__ = 'reStructuredText'


import os
import re
import sys
from types import *

import docutils
from docutils import io, frontend, languages, nodes, utils, writers
from docutils.core import publish_cmdline, default_description
from docutils.writers import html4css1

try:
    _filesystemencoding = (sys.getfilesystemencoding() or
                           sys.getdefaultencoding())
except AttributeError:
    # sys.getfilesystemencoding() is new in Python 2.3.
    # Use sys.getdefaultencoding() instead...
    _filesystemencoding = sys.getdefaultencoding()

try:
    True
except NameError:
    True = 1
    False = 0

if sys.version_info[:3] < (2, 4, 0):
    class set:
        # Minimal set type for older Python versions.
        def __init__(self, iterable):
            self._map = dict([(n, 1) for n in iterable])
        def add(self, item):
            self._map[item] = 1
        def __contains__(self, item):
            return item in self._map


_quote_re = re.compile('[^a-zA-Z0-9/_.-]')

def quote_filename(filename):
    """quote_filename('foo bar.html') -> 'foo%20bar.html'.  If `filename` is
    a unicode string, it will be converted to filesystem encoding first.
    """
    def substitute(m):
        return '%%%02x' % ord(m.group(0))

    if type(filename) is unicode:
        try:
            filename = filename.encode(_filesystemencoding)
        except UnicodeEncodeError, err:
            raise ValueError('Cannot convert Unicode filename %r to "%s" '
                             'encoding. (%s)' % (filename, _filesystemencoding,
                                                 err))
    return _quote_re.sub(substitute, filename)

def default_navigation_callback(vars):
    """Create and return the default navigation header and footer."""
    prev = root = next = up = '&nbsp;'
    prev_title = root_title = next_title = '&nbsp;'

    if vars['prev']:
        prev = (u'<a href="%(prev)s" '
                 'title="%(prev_title)s">&laquo;&nbsp;Previous</a>' % vars)
        prev_title = u'%(prev_title)s' % vars
    if vars['next']:
        next = (u'<a href="%(next)s" '
                 'title="%(next_title)s">Next&nbsp;&raquo;</a>' % vars)
        next_title = u'%(next_title)s' % vars
    if vars['root']:
        root = (u'<a href="%(root)s" '
                 'title="%(root_title)s">Start</a>' % vars)
        root_title = u'<strong>%(root_title)s</strong>' % vars
    if vars['parent']:
        up = (u'<a href="%(parent)s" '
               'title="%(parent_title)s">Up</a>' % vars)

    v = {'next': next, 'prev': prev, 'root': root, 'up': up,
         'next_title': next_title,
         'prev_title': prev_title,
         'root_title': root_title,
        }

    header = (u'<table width="100%%">\n'
               '<tr valign="top">'
               '<td width="20%%" align="left">%(prev)s</td>'
               '<td width="60%%" align="center">%(root_title)s</td>'
               '<td width="20%%" align="right">%(next)s</td>'
               '</tr>\n'
               '</table>\n'
               '<hr />\n' % v)

    footer = (u'<hr />\n'
               '<table width="100%%">\n'
               '<tr valign="top">'
               '<td width="40%%" align="left">%(prev)s</td>'
               '<td width="20%%" align="center">%(up)s</td>'
               '<td width="40%%" align="right">%(next)s</td>'
               '</tr>\n'
               '<tr valign="top">'
               '<td width="40%%" align="left">%(prev_title)s</td>'
               '<td width="20%%" align="center">%(root)s</td>'
               '<td width="40%%" align="right">%(next_title)s</td>'
               '</tr>\n'
               '</table>\n' % v)

    return header, footer

def encode(text):
    text = text.replace('&', '&amp;')
    text = text.replace('<', '&lt;')
    text = text.replace('>', '&gt;')
    text = text.replace('"', '&quot;')
    text = text.replace('@', '&#64;')
    text = text.replace(u'\u00a0', '&nbsp;')
    return text

def attval(text):
    text = encode(text)
    text = text.replace('\n', ' ')
    text = text.replace('\r', ' ')
    text = text.replace('\t', ' ')
    text = text.replace('\v', ' ')
    text = text.replace('\f', ' ')
    return text

def validate_basename(setting, value, option_parser, config_parser=None,
                      config_section=None):
    if os.sep in value:
        raise ValueError('"%s" cannot appear in value' % os.sep)

    stripped = value.strip()
    if not value or not stripped:
        raise ValueError('value must not be an empty '
                         '(or whitespace-only) string')
    return stripped


class ChunkerError(Exception):
    """Base class for exceptions thrown by the chunker."""
    pass


class DupFilenameError(ChunkerError):
    """Exception thrown for duplicate filenames."""
    pass


class UnsupportedWriterError(ChunkerError):
    """Exception thrown if the writer class passed to the chunker does not
    have a ``set_external_ids()`` method.
    """
    pass


class ChunkedHTMLWriter(writers.Writer):

    # Default filename for the root chunk
    default_chunker_root_filename = 'index.html'

    # Default filename for sections
    default_chunk_basename = '%i.html'

    config_section = 'html chunker'
    # XXX: Do I need to set dependencies?
    #config_section_dependencies = ('html4css1 writer', )

    settings_spec = (
        'HTML Chunker Specific Options',
        None,
        (('The maximum section depth for chunking.  The default is 1, i.e. '
          'top-level sections will be chunked.  Use 0 for "no limit".',
          ['--chunk-depth'],
          {'default': 1, 'metavar': '<level>',
           'validator': frontend.validate_nonnegative_int}),
         ('Specify the base filename for sections.  Use any of the following '
          'placeholders in the name: '
          '%%i (the ID value of the chunk node), '
          '%%n (the chunk number, zero-based, zero-padded), '
          '%%s (the section number, each component zero-padded), and '
          '%%%% (the %% character).  '
          'The default is "%s".' % default_chunk_basename,
          ['--chunk-basename'],
          {'default': default_chunk_basename, 'metavar': '<name>',
           'validator': validate_basename}),
         ('Specify which node ID to use for the $id placeholder in section '
          'filenames.  The value is a number (the index of the ID), where 0 '
          'means the auto-generated ID, 1 means the 1st user-defined, 2 means '
          'the 2nd one, etc.  You can also use negative indices: -1 means the '
          'last user-defined ID, -2 means the last before, etc.  '
          'If the ID with the specified index does not exist for a node, the '
          'first one (index 0) will be used.  The default is 0.',
          ['--chunker-select-id'],
          {'default': 0, 'metavar': '<num>', 'type': 'int'}),
         ('Specify the section number separator for the $sectnum placeholder '
          '(see the --config-sect-filename option).  The default is ".".',
          ['--chunker-sectnum-sep'],
          {'default': '.', 'metavar': '<string>'}),
         ('Print a message for each chunk being written.',
          ['--chunker-progress'],
          {'dest': 'chunker_progress', 'default': 1, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Do not print the chunked filename messages.  This is the default.',
          ['--chunker-no-progress'],
          {'dest': 'chunker_progress', 'default': 0, 'action': 'store_false',
           'validator': frontend.validate_boolean}),
    ),)

    def __init__(self, writer_class=None, nav_callback=None):
        """Arguments:

        * `writer_class`: The writer class that actually converts each
          chunk of the document tree to HTML (the default is
          ``html4css1.Writer``).
        * `nav_callback`: A function that is used to build the navigation bar
          for each chunk (if not specified, the default implementation is
          used).

        See the module documentation for details.
        """
        writers.Writer.__init__(self)
        if not writer_class:
            writer_class = html4css1.Writer
        if not callable(getattr(writer_class, 'set_external_ids', None)):
            raise UnsupportedWriterError('writer class %r does not have the '
                                         '"set_external_ids" method required '
                                         'for chunking' % writer_class)
        self.writer_class = writer_class
        self.supported = writer_class.supported
        self.settings_spec = writer_class.settings_spec + self.settings_spec

        self.nav_callback = nav_callback or default_navigation_callback

    def get_transforms(self):
        return self.writer_class().get_transforms()

    def write(self, document, destination):
        self.document = document
        self.language = languages.get_language(document.settings.language_code)

        settings = self.document.settings
        destdir = ''
        root_filename = None
        if settings._destination:
            destdir = os.path.dirname(settings._destination)
            root_filename = os.path.basename(settings._destination)

        chunker = HTMLChunker(document, self.writer_class, root_filename,
                              destination, self.nav_callback)

        chunks = chunker.chunk()
        number_of_chunks = len(chunks)
        self.output = chunker.convert_chunk(chunks[0])
        output = destination.write(self.output)

        for c in chunks:
            destpath = os.path.join(destdir, c.filename)
            if settings.chunker_progress and root_filename:
                n = ('%%%dd' % len(str(number_of_chunks))) % (c.number + 1)
                print 'Writing chunk %s of %d: %s'\
                      % (n, number_of_chunks, destpath)
            if not c.is_root():
                out = chunker.convert_chunk(c)
                f = io.FileOutput(destination=None,
                        destination_path=destpath,
                        encoding=settings.output_encoding,
                        error_handler=settings.output_encoding_error_handler)
                f.write(out)
                f.close()

        return output

    def translate(self):
        pass


class HTMLChunker:
    """This class creates chunked HTML output from a document tree."""

    def __init__(self, document, writer_class, root_filename, root_destination,
                 nav_callback):
        """Arguments:

        * `document`: The document tree.
        * `writer_class`: A writer class that will be instantiated for each
          chunk to create the HTML output for that chunk.
        * `root_filename`: The file name of the root chunk.
        * `root_destination`: The destination to write the root chunk to.  This
          is an io.{File,String}Output instance.
        * `nav_callback`: A function that will be called for each chunk to
          create the navigation bars.
        """
        self.document = document
        self.writer_class = writer_class
        self.root_filename = root_filename
        self.root_destination = root_destination
        self.nav_callback = nav_callback
        self.settings = document.settings

    def chunk(self):
        """Chunk the document and return the list of chunks.  The chunks can be
        converted to HTML after this call by passing them one by one to the
        ``convert_chunk()`` method.
        """
        self.sections = self.collect_sections()
        self.chunktree = self.build_chunktree()

        if self.chunktree.children:
            if isinstance(self.root_destination, io.FileOutput):
                if not self.root_filename:
                    raise ChunkerError('Cannot write chunked HTML to stdout.')
            else:
                raise ChunkerError('Cannot write chunked HTML to non-file '
                                   'destination.')
            self.relocate_system_messages()
        #end if self.chunktree.children

        self.generate_filenames()
        self.chunktree.detach_nodes()
        self.relocate_meta_nodes()

        # The chunks are detached now, so we can build a mapping of IDs to
        # chunks -- each ID points to the chunk it appears in.
        self.ids = {}
        for chunk in self.chunktree.walk():
            for node in chunk.node.traverse(nodes.Element):
                for i in node.get('ids', []):
                    self.ids[i] = chunk

        return self.chunktree.walk()

    def collect_sections(self):
        """Build a tree of the document + all its sections, and return it."""
        def add_sections(node, parent_section):
            for n in node:
                if isinstance(n, nodes.section):
                    section = parent_section.append(Section(n))
                    if len(n):
                        add_sections(n, section)

        section_tree = Section(self.document)
        add_sections(self.document, section_tree)
        return section_tree

    def relocate_system_messages(self):
        """Relocate the auto-generated system messages section to the root
        chunk (after the title/subtitle/docinfo node).  I think it is better to
        have it in the root chunk than at the end (i.e. in the last chunk,
        where nobody notices).
        """
        if not self.chunktree.children:
            return

        pos = self.document.first_child_not_matching_class((
                nodes.Titular, nodes.docinfo, nodes.Decorative,
                ))
        if pos is None:
            pos = 0

        for sect in self.sys_msgs_sections:
            sect.parent.remove(sect)
            self.document.insert(pos, sect)
            pos += 1

    def relocate_meta_nodes(self):
        """Relocate <meta> nodes to the root chunk.  Make sure the chunk nodes
        are detached before calling this method."""
        if not self.chunktree.children or not self.meta_nodes:
            return

        for meta, chunk in self.meta_nodes:
            if chunk is not chunk.root:
                meta.parent.remove(meta)
                chunk.root.node.append(meta)

    def build_chunktree(self):
        """Build the chunk tree by collecting chunkable section nodes up to
        ``self.settings.chunk_depth` depth (0 means unlimited), and return the
        root chunk.
        """
        maxdepth = self.settings.chunk_depth
        visitor = DocumentChunker(self.document, maxdepth)
        self.document.walkabout(visitor)

        for name in ('ischunk', 'meta_nodes', 'number_of_chunks',
                     'sys_msgs_sections'):
            setattr(self, name, getattr(visitor, name))

        visitor.chunktree.setup_navlinks()
        return visitor.chunktree


    def generate_filenames(self):
        """Generate filenames for the chunks."""
        chunktree = self.chunktree
        chunktree.filename = self.root_filename or ''
        chunktree.quoted_filename = quote_filename(chunktree.filename)

        if not chunktree.children:
            return

        chunk_filenames = set([chunktree.filename])

        idnum = self.settings.chunker_select_id
        sectnum_sep = self.settings.chunker_sectnum_sep

        sections = dict([(s.node, s) for s in self.sections.walk()])

        chunk_list = chunktree.walk()
        chunknum_digits = len(str(self.number_of_chunks))
        # Remove the root chunk, it already has a filename
        del chunk_list[0]

        def generate_filename(chunk, rx=re.compile('%([ins%])')):
            def replace(m):
                s = m.group(1)
                if s == 'i':
                    try:
                        # Use -idnum, because user-defined IDs are in reverse
                        # order.
                        return chunk.node['ids'][-idnum]
                    except IndexError:
                        try:
                            return chunk.node['ids'][0]
                        except IndexError:
                            # We *need* the ID.  Create one.
                            return self.document.set_id(chunk.node)
                if s == 'n':
                    return ('%%0%dd' % chunknum_digits) % chunk.number
                if s == 's':
                    sectnum = sections[chunk.node].get_sectnum_str()
                    return sectnum_sep.join(sectnum)
                if s == '%':
                    return '%'
                return s
            return rx.sub(replace, self.settings.chunk_basename)

        for chunk in chunk_list:
            filename = generate_filename(chunk)
            if filename in chunk_filenames:
                raise DupFilenameError('Filename "%s" is already used for '
                                       'a chunk.' % filename)
            chunk.filename = filename
            chunk.quoted_filename = quote_filename(chunk.filename)
            chunk_filenames.add(filename)

    def convert_chunk(self, chunk):
        """Convert the chunk to HTML and return the HTML output."""
        doctree = self.create_subdocument(chunk)
        writer = self.writer_class()
        writer.set_external_ids(self.collect_external_ids(chunk))
        writer.write(doctree, io.NullOutput())
        writer.assemble_parts()
        return self.assemble_html_output(chunk, writer.parts)

    def collect_external_ids(self, chunk):
        """Resolve external references in the chunk, and return a mapping of
        IDs to URIs ('filename#id' or just 'filename').
        """
        external_ids = {}
        for node in chunk.node.traverse(nodes.Element):
            ids = node.get('backrefs', [])
            if node.hasattr('refid'):
                ids.append(node['refid'])
            for refid in ids:
                refchunk = self.ids[refid]
                if refchunk is not chunk:
                    if self.document.ids[refid] in self.ischunk:
                        uri = refchunk.quoted_filename
                    else:
                        uri = '%s#%s' % (refchunk.quoted_filename, refid)
                    external_ids[refid] = uri

        return external_ids

    def create_subdocument(self, chunk):
        """If `chunk` wraps a section node, create and return a new document
        node with the section added, otherwise return the root node.
        """
        if isinstance(chunk.node, nodes.document):
            return chunk.node

        # Create a new document.
        doctree = utils.new_document(self.settings._source,
                                     self.document.settings)

        # Add the decoration (head and footer).
        if self.document.decoration and len(self.document.decoration):
            root_decor = self.document.decoration
            decor = doctree.get_decoration()
            # Note: We can't use root_decor.get_{header,footer}(), because
            # they create the header/footer if it is missing.  We don't want
            # that.
            if isinstance(root_decor[0], nodes.header):
                header = decor.get_header()
                for n in root_decor[0]:
                    header.append(n.deepcopy())
            if isinstance(root_decor[-1], nodes.footer):
                footer = decor.get_footer()
                for n in root_decor[-1]:
                    footer.append(n.deepcopy())

        ## Copy <meta> nodes.
        #for meta, dummy in self.meta_nodes:
        #    doctree.append(meta.deepcopy())

        # Now add the copy of the section, and set the document node's title
        # from the section title.
        doctree += chunk.node.deepcopy()
        if isinstance(doctree[-1][0], nodes.title):
            n = doctree[-1][0]
            # XXX: Skip generated section number?  (Should be configurable.)
            doctree['title'] = chunk.get_title()

        return doctree

    def assemble_html_output(self, chunk, parts):
        """Assemble and return the HTML output for `chunk` from the HTML
        document parts in `parts`.
        """
        if chunk.is_root() and not chunk.children:
            return parts['whole']

        nav_header, nav_footer = self.create_navigation(chunk, parts)
        out = []

        # <?xml?> + <!DOCTYPE> + <html> + <head>
        out.append(parts['head_prefix'])

        # head content, less the stylesheet
        #out.append(parts['html_head'] % self.settings.output_encoding)

        # We want the same meta tags for each chunk, so we will copy them from
        # the root chunk.
        if chunk.is_root():
            meta = self.root_meta = parts['meta']
        else:
            meta = self.root_meta

        out.append(meta)

        def add_link(rel, chunk):
            out.append(u'<link rel="%s" href="%s" title="%s" />\n'
                       % (rel, chunk.quoted_filename,
                          attval(chunk.get_title())))

        if not chunk.is_root():
            add_link('start', chunk.root)
        if chunk.next:
            add_link('next', chunk.next)
        if chunk.prev:
            add_link('prev', chunk.prev)

        # Title + stylesheet.  parts['title'] might contain inline markup, so
        # will get the title this way.
        out.append('<title>%s</title>\n' % encode(chunk.get_title()))
        out.append(parts['stylesheet'])

        if nav_header:
            # body_prefix is </head><body> + header
            # + <div class="document" ...>
            #
            # We want the navigation header right before
            # <div class="document" ...>
            #
            # This is a silly hack.  It would be much better if the HTML
            # writer provided parts['document_start'] or something for the
            # <div class="document" ...> tag, and parts['document_end'] for
            # the closing </div>.
            #
            sep = '<div class="document"'
            before, after = parts['body_prefix'].split(sep)
            out.append(before)
            out.append(u'<div class="navheader">\n')
            out.append(nav_header)
            out.append(u'</div>\n')
            out.append(sep)
            out.append(after)
        else:
            out.append(parts['body_prefix'])

        out.append(parts['body_pre_docinfo'])
        out.append(parts['docinfo'])
        out.append(parts['fragment'])

        if nav_footer:
            # body_suffix is </div> + footer + </body> + </html>
            # We want the navigation footer before the footer, or </body>.
            #
            # Again, this is a silly hack.
            #
            try:
                sep = '<div class="footer"'
                before, after = parts['body_suffix'].split(sep)
            except ValueError:
                sep = '</body>'
                before, after = parts['body_suffix'].split(sep)

            out.append(before)
            out.append(u'<div class="navfooter">\n')
            out.append(nav_footer)
            out.append(u'</div>\n')
            out.append(sep)
            out.append(after)
        else:
            out.append(parts['body_suffix'])
        
        return u''.join(out)

    def create_navigation(self, chunk, parts):
        vars = dict([(v, u'') for v in [
            'next', 'next_title_raw',
            'prev', 'prev_title_raw',
            'parent', 'parent_title_raw',
            'root', 'root_title_raw',
            ]])

        if chunk.next:
            vars['next'] = chunk.next.quoted_filename
            vars['next_title_raw'] = chunk.next.get_title()
        if chunk.prev:
            vars['prev'] = chunk.prev.quoted_filename
            vars['prev_title_raw'] = chunk.prev.get_title()
        if chunk.parent:
            vars['parent'] = chunk.parent.quoted_filename
            vars['parent_title_raw'] = chunk.parent.get_title()
        if chunk.root and chunk is not chunk.root:
            vars['root'] = chunk.root.quoted_filename
            vars['root_title_raw'] = chunk.root.get_title()

        for name in ('next_title', 'prev_title', 'parent_title', 'root_title'):
            vars[name] = attval(vars['%s_raw' % name])

        header, footer = self.nav_callback(vars)
        if type(header) is not unicode:
            raise ChunkerError('Bad navigation header: not a Unicode string')
        if type(footer) is not unicode:
            raise ChunkerError('Bad navigation footer: not a Unicode string')
        
        return header, footer


class NodeWrapper:
    """A structural node of the document tree.  The root node wraps a
    ``nodes.document``, and each child node wraps a ``nodes.section``.
    """

    def __init__(self, node):
        assert isinstance(node, (nodes.section, nodes.document))
        self.node = node
        self.children = []
        self.root = self
        self.parent = None

    def append(self, section):
        """Add a section (wrapping a ``nodes.section``) to the children of this
        chunk.
        """
        assert isinstance(section.node, nodes.section)
        section.parent = self
        section.root = self.root
        self.children.append(section)
        return section

    def walk(self, topdown=True):
        """Generate all the sections in the tree by walking the tree either
        top down or bottom up.
        """
        sects = []
        if topdown:
            sects.append(self)

        for child in self.children:
            sects.extend(child.walk(topdown))

        if not topdown:
            sects.append(self)

        return sects

    def is_root(self):
        """Return whether this is the root."""
        return self is self.root

    def __nonzero__(self):
        return True

    def __len__(self):
        return len(self.children)

    def __getitem__(self, key):
        if isinstance(key, int):
            return self.children[key]
        else:
            raise TypeError('element index must be an integer')


class Chunk(NodeWrapper):
    """A chunk of the document tree."""

    def __init__(self, node):
        NodeWrapper.__init__(self, node)
        self.prev = None
        self.next = None
        self.number = 0

    def setup_navlinks(self):
        """Walk the chunk tree top down and set up previous/next links for HTML
        navigation.  As a convenience, this method also sets the ``number``
        attribute of each chunk to the navigation index of the chunk.
        """
        prev = None
        for n, chunk in enumerate(self.root.walk()):
            chunk.number = n
            chunk.prev = prev
            if prev:
                prev.next = chunk
            else:
                chunk.next = None
            prev = chunk

    def detach_nodes(self):
        """Detach chunk nodes from their parent, splitting the document tree
        into pieces.
        """
        for chunk in self.root.walk():
            if chunk.node.parent:
                chunk.node.parent.remove(chunk.node)
                chunk.node.parent = None

    def get_title(self, skip_sectnum=False):
        if isinstance(self.node, nodes.document):
            return self.node.get('title', '')
        if len(self.node) and isinstance(self.node[0], nodes.title):
            title_node = self.node[0]
            if not len(title_node):
                return u''
            if not isinstance(title_node[0], nodes.generated):
                return title_node.astext()
            if skip_sectnum:
                return title_node[1].astext()
            return re.sub(u'[\u00a0]{2,}', u'\u00a0', title_node.astext())
        return u''


class Section(NodeWrapper):
    """Convenience class for numbering sections."""

    def get_sectnum(self):
        """Return the section number as a list of integers.  Note that this
        has nothing to do with reST's section numbering -- it's just a count
        of the sections at each level.  For the root, return an empty list.
        For example::

            <document>                  []
                <section>               [1]
                <section>               [2]
                    <section>           [2, 1]
                        <section>       [2, 2]
        """
        if self.parent:
            sectnum = self.parent.get_sectnum()
            sectnum.append(self.parent.children.index(self) + 1)
            return sectnum
        else:
            return []

    def get_sectnum_str(self):
        """Same as ``get_sectnum()``, but the return value is a list of
        strings, where each string is padded with zeros as necessary.
        """
        if self.parent:
            n = self.parent.children.index(self) + 1
            sectnum = self.parent.get_sectnum_str()
            sectnum.append(('%%0%dd' % len(str(len(self.parent)))) % n)
            return sectnum
        else:
            return []

    def get_index(self):
        """Return the section index (1-based), i.e. the index of this section
        inside its parent.  Always return 0 for the root.
        """
        if self.parent:
            return self.parent.children.index(self) + 1
        else:
            return 0

    def get_index_str(self):
        """Return the section index (1-based), i.e. the index of this section
        inside its parent.  The return value is a string (optionally
        zero-padded); it is always '0' for the root.
        """
        if self.parent:
            i = self.parent.children.index(self) + 1
            return ('%%0%dd' % len(str(len(self.parent)))) % i
        else:
            return '0'


class DocumentChunker(nodes.GenericNodeVisitor):
    """A node visitor that builds a chunk tree from chunkable nodes (right
    now the document node and section nodes), and collects various useful
    data for chunking.
    """

    def __init__(self, document, maxdepth=0):
        nodes.GenericNodeVisitor.__init__(self, document)
        self.maxdepth = maxdepth
        self.chunktree = Chunk(document)
        self.chunks = [self.chunktree]
        self.number_of_chunks = 1
        self.ischunk = set([document])
        self.section_level = 0
        self.sys_msgs_sections = []
        self.meta_nodes = []

    def is_sysmsg_section(self, node):
        return (node.hasattr('classes')
                and 'system-messages' in node['classes'])

    def visit_section(self, node):
        if self.is_sysmsg_section(node):
            self.sys_msgs_sections.append(node)
            raise nodes.SkipNode
        if (self.section_level + 1) > self.maxdepth > 0:
            raise nodes.SkipNode

        #if not self.chunk_first_section:
        #    me = node.parent.index(node)
        #    first = node.parent.first_child_matching_class(nodes.section)
        #    if first is not None:
        #        if first == me:
        #            raise nodes.SkipDeparture
        #        s = node.parent[first]

        self.section_level += 1

        # Chunk this section.
        self.ischunk.add(node)
        chunk = Chunk(node)
        self.chunks[-1].append(chunk)
        self.chunks.append(chunk)
        self.number_of_chunks += 1

    def depart_section(self, node):
        self.chunks.pop()
        self.section_level -= 1

    def visit_meta(self, node):
        # Collect the meta node and the chunk it belongs to.
        self.meta_nodes.append((node, self.chunks[-1]))

    def do_nothing(self, node):
        pass

    default_visit = do_nothing
    default_departure = do_nothing
    unknown_visit = do_nothing
    unknown_departure = do_nothing


def main():
    try:
        import locale
        locale.setlocale(locale.LC_ALL, '')
    except ImportError:
        pass
    desc = default_description + (
        '  '
        'For chunked HTML output, <destination> is the filename of the '
        'root chunk.  The other chunks are written to the same directory '
        'as the root chunk, but their names are generated from the value '
        'given to the --chunk-basename option.  It is an error to use '
        'stdout as <destination> if there is more than one chunk to write.'
        )
    publish_cmdline(writer=ChunkedHTMLWriter(), description=desc)


if __name__ == '__main__':
    main()
