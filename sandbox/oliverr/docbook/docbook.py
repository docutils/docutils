#! /usr/bin/env python

"""
:Author: Ollie Rutherfurd
:Contact: oliver@rutherfurd.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

DocBook document tree Writer.

This Writer converts a reST document tree to a subset
of DocBook.

.. Note:: This is an unfinished work in progress.

Document Types
==============

This writer can create 3 types of DocBook documents:

1. "article" *(default)*
2. "book"
3. "chapter"

.. Note:: When creating a "book" document, all first-level
    sections are output as "chapter" elements instead
    of "section" as in "article" and "chapter".

Mappings
========

Option List
-----------

As there is no direct equivlent for a listing of program options
in DocBook_, as defined in reST_, a table containing the
option list contents is generated.

Field List
----------

Like `Option List`_, there is not direct equivlent for
a Field List in DocBook, so this is done using a
"variablelist".

.. NOTE:: It might be better to switch Definition List
    to glossary or something similar, so Field List
    and Definition List are generating the same type
    of output.

Todo
====

- Bibliography element
- Footnotes are totally broken.
- Inline images
- Plenty of other  things -- most of them are 
  flagged with ``TODO: ...`` comments.

"""

__docformat__ = 'reStructuredText'

import sys
import string
import re
from types import ListType
from docutils import writers, nodes, languages

class Writer(writers.Writer):

    cmdline_options = (
        'DocBook-Specific Options',
        None,
        (('Set DocBook document type. '
            'Choices are "article", "book", and "chapter". '
            'Default is "article".',
            ['--doctype'],
            {'default': 'article', 
             'metavar': '<name>',
             'type': 'choice', 
             'choices': ('article', 'boot', 'chapter',)
            }
         ),
        )
    )

    output = None
    """Final translated form of `document`."""

    def translate(self):
        visitor = DocBookTranslator(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.astext()

    def record(self):
        self.recordfile(self.output, self.destination)

class DocBookTranslator(nodes.NodeVisitor):

    # QUESTION: should encoding be variable?
    XML_DECL = '<?xml version="1.0" encoding="UTF-8"?>\n'

    DOCTYPE_DECL = """<!DOCTYPE %s PUBLIC "-//OASIS//DTD DocBook XML V4.1.2//EN"
                    "http://www.oasis-open.org/docbook/xml/4.0/docbookx.dtd">\n"""

    def __init__(self, document):
        nodes.NodeVisitor.__init__(self, document)
        self.language = languages.get_language(document.options.language_code)
        # doctype may be: article, book, or chapter
        self.doctype = document.options.doctype
        if self.doctype not in ('article','book','chapter'):
            raise ValueError('doctype must be one of: %s, %s, %s (got %s)' \
                % ('article','book','chapter', self.doctype,))
        self.doc_header = [
            self.XML_DECL,   # @@@ % output_encoding
            self.DOCTYPE_DECL % (self.doctype,),
            '<%s>\n' % (self.doctype,),
        ]
        self.doc_footer = [
            '</%s>\n' % (self.doctype,)
        ]
        self.biblio = {}
        self.body = []
        self.section = 0
        self.context = []
        self.colnames = []

    # def getDocInfo(self):
        # """Creates elements for all document attributes to go
        # into `{doctype}info` element. It's just temporary
        # solution.
        # """
        # 
        # # FIXME: this method won't work when setting multiple
        # # elements for a sub-element
        # def createNodes(path,text):
            # nodes = path.split('/')
            # elements = ['\n<%s>' % name for name in nodes]
            # elements.append(text)
            # nodes.reverse()
            # elements.extend(['</%s>\n' % name for name in nodes])
            # return ''.join(elements)
        # buff = ['<%sinfo>\n' % (self.doctype,)]
        # for (path,text) in self.docinfo.items():
            # buff.append(createNodes(path,text))
        # return ''.join(buff) + '</%sinfo>\n' % (self.doctype,)

    def astext(self):
        # TODO: include {doctype}info elements
        return ''.join(self.doc_header
                    #+ [self.getDocInfo()]
                    + self.body
                    + self.doc_footer)

    def encode(self, text):
        """Encode special characters in `text` & return."""
        # @@@ A codec to do these and all other HTML entities would be nice.
        text = text.replace("&", "&amp;")
        text = text.replace("<", "&lt;")
        text = text.replace('"', "&quot;")
        text = text.replace(">", "&gt;")
        return text

    def attval(self, text,
               transtable=string.maketrans('\n\r\t\v\f', '     ')):
        """Cleanse, encode, and return attribute value text."""
        return self.encode(text.translate(transtable))

    def starttag(self, node, tagname, suffix='\n', infix='', **attributes):
        """
        Construct and return a start tag given a node (id & class attributes
        are extracted), tag name, and optional attributes.
        """
        atts = {}
        for (name, value) in attributes.items():
            atts[name.lower()] = value

        for att in ('id',):             # node attribute overrides
            if node.has_key(att):
                atts[att] = node[att]

        attlist = atts.items()
        attlist.sort()
        parts = [tagname.lower()]
        for name, value in attlist:
            if value is None:           # boolean attribute
                # According to the HTML spec, ``<element boolean>`` is good,
                # ``<element boolean="boolean">`` is bad.
                # (But the XHTML (XML) spec says the opposite.  <sigh>)
                parts.append(name.lower())
            elif isinstance(value, ListType):
                values = [str(v) for v in value]
                parts.append('%s="%s"' % (name.lower(),
                                          self.attval(' '.join(values))))
            else:
                parts.append('%s="%s"' % (name.lower(),
                                          self.attval(str(value))))
        return '<%s%s>%s' % (' '.join(parts), infix, suffix)

    def emptytag(self, node, tagname, suffix='\n', **attributes):
        """Construct and return an XML-compatible empty tag."""
        return self.starttag(node, tagname, suffix, infix=' /', **attributes)

    def visit_Text(self, node):
        self.body.append(self.encode(node.astext()))

    def depart_Text(self, node):
        pass

    def visit_attention(self, node):
        self.body.append(self.starttag(node, 'note'))
        self.body.append('\n<title>%s</title>\n' % (self.language.labels[node.tagname],))

    def depart_attention(self, node):
        self.body.append('</note>\n')

    # TODO: author (map to {doctype}info/author/othername)
    visit_author = depart_author = lambda self,node: None

    # TODO: authors (use authorgroup)
    visit_authors = depart_authors = lambda self,node: None

    def visit_block_quote(self, node):
        self.body.append(self.starttag(node, 'blockquote'))

    def depart_block_quote(self, node):
        self.body.append('</blockquote>\n')

    def visit_bullet_list(self, node):
        self.body.append(self.starttag(node, 'itemizedlist'))

    def depart_bullet_list(self, node):
        self.body.append('</itemizedlist>\n')

    def visit_caption(self, node):
        # NOTE: ideally, this should probably be stuffed into
        # the mediaobject as a "caption" element
        self.body.append(self.starttag(node, 'para'))

    def depart_caption(self, node):
        self.body.append('</para>')

    def visit_caution(self, node):
        self.body.append(self.starttag(node, 'caution'))
        self.body.append('\n<title>%s</title>\n' % (self.language.labels[node.tagname],))

    def depart_caution(self, node):
        self.body.append('</caution>\n')

    # QUESTION: how should citation be handled?
    # TODO: citation
    visit_citation = depart_citation = lambda self, node: None

    # TODO: citation_reference
    visit_citation_reference = depart_citation_reference = lambda self, node: None

    def visit_classifier(self, node):
        self.body.append(' : ')
        self.body.append(self.starttag(node, 'type'))

    def depart_classifier(self, node):
        self.body.append('</type>\n')

    def visit_colspec(self, node):
        self.colnames.append('col_%d' % (len(self.colnames) + 1,))
        atts = {'colname': self.colnames[-1]}
        self.body.append(self.emptytag(node, 'colspec', **atts))

    def depart_colspec(self, node):
        pass

    def visit_comment(self, node, sub=re.compile('-(?=-)').sub):
        """Escape double-dashes in comment text."""
        self.body.append('<!-- %s -->\n' % sub('- ', node.astext()))
        raise nodes.SkipNode

    def visit_contact(self, node):
        # TODO: map to {doctype}info/author/email
        # (if other needed parts of author are present)
        raise nodes.SkipNode

    # TODO: copyright
    visit_copyright = depart_copyright = lambda self,node: None

    def visit_danger(self, node):
        self.body.append(self.starttag(node, 'caution'))
        self.body.append('\n<title>%s</title>\n' % (self.language.labels[node.tagname],))

    def depart_danger(self, node):
        self.body.append('</caution>\n')

    def visit_date(self, node):
        # TODO: include date in {doctype}info/date
        raise nodes.SkipNode

    def visit_decoration(self, node):
        pass

    def depart_decoration(self, node):
        pass

    def visit_definition(self, node):
        # "term" is not closed
        self.body.append('</term>\n')
        self.body.append(self.starttag(node, 'listitem'))

    def depart_definition(self, node):
        self.body.append('</listitem>\n')

    def visit_definition_list(self, node):
        self.body.append(self.starttag(node, 'variablelist'))

    def depart_definition_list(self, node):
        self.body.append('</variablelist>\n')

    def visit_definition_list_item(self, node):
        self.body.append(self.starttag(node, 'varlistentry'))

    def depart_definition_list_item(self, node):
        self.body.append('</varlistentry>\n')

    def visit_description(self, node):
        self.body.append(self.starttag(node, 'entry'))

    def depart_description(self, node):
        self.body.append('</entry>\n')

    def visit_docinfo(self, node):
        pass

    def depart_docinfo(self, node):
        pass

    def visit_doctest_block(self, node):
        self.body.append('<informalexample>\n')
        self.body.append(self.starttag(node, 'programlisting'))

    def depart_doctest_block(self, node):
        self.body.append('</programlisting>\n')
        self.body.append('</informalexample>\n')

    def visit_document(self, node):
        pass

    def depart_document(self, node):
        pass

    def visit_emphasis(self, node):
        self.body.append(self.starttag(node, 'emphasis'))

    def depart_emphasis(self, node):
        self.body.append('</emphasis>\n')

    def visit_entry(self, node):
        tagname = 'entry'
        atts = {}
        if node.has_key('morerows'):
            atts['morerows'] = node['morerows']
        if node.has_key('morecols'):
            atts['namest'] = self.colnames[self.entry_level]
            atts['nameend'] = self.colnames[self.entry_level + node['morecols']]
        self.entry_level += 1   # for tracking what namest and nameend are
        self.body.append(self.starttag(node, tagname, **atts))

    def depart_entry(self, node):
        self.body.append('</entry>\n')

    def visit_enumerated_list(self, node):
        # TODO: need to specify "mark" type used for list items
        self.body.append(self.starttag(node, 'orderedlist'))

    def depart_enumerated_list(self, node):
        self.body.append('</orderedlist>\n')

    def visit_error(self, node):
        self.body.append(self.starttag(node, 'caution'))
        self.body.append('\n<title>%s</title>\n' % (self.language.labels[node.tagname],))

    def depart_error(self, node):
        self.body.append('</caution>\n')

    # TODO: wrap with some element (filename used in DocBook example)
    def visit_field(self, node):
        self.body.append(self.starttag(node, 'varlistentry'))

    def depart_field(self, node):
        self.body.append('</varlistentry>\n')

    # TODO: see if this should be wrapped with some element
    def visit_field_argument(self, node):
        self.body.append(' ')

    def depart_field_argument(self, node):
        pass

    def visit_field_body(self, node):
        # NOTE: this requires that a field body always
        #   be present, which looks like the case
        #   (from docutils.dtd)
        self.body.append(self.context.pop())
        self.body.append(self.starttag(node, 'listitem'))

    def depart_field_body(self, node):
        self.body.append('</listitem>\n')

    def visit_field_list(self, node):
        self.body.append(self.starttag(node, 'variablelist'))

    def depart_field_list(self, node):
        self.body.append('</variablelist>\n')

    def visit_field_name(self, node):
        self.body.append(self.starttag(node, 'term'))
        # popped by visit_field_body, so "field_argument" is
        # content within "term"
        self.context.append('</term>\n')

    def depart_field_name(self, node):
        pass
        #self.body.append('</term>\n')

    def visit_figure(self, node):
        self.body.append(self.starttag(node, 'informalfigure'))
        self.body.append('<blockquote>')

    def depart_figure(self, node):
        self.body.append('</blockquote>')
        self.body.append('</informalfigure>\n')

    # TODO: footer (this is where 'generated by docutils' arrives)
    # if that's all that will be there, it could map to "colophon"
    def visit_footer(self, node):
        raise nodes.SkipChildren

    def depart_footer(self, node):
        pass


    def visit_footnote(self, node):
        atts = {'id': node['id']}
        if isinstance(node[0], nodes.label):
            # FIXME: this fails with the second auto-sequence
            # symbol (after "#")
            #atts['label'] = node[0].astext()
            pass
        self.body.append(self.starttag(node, 'footnote', **atts))

    def depart_footnote(self, node):
        self.body.append('</footnote>\n')

    def visit_footnote_reference(self, node):
        if node.has_key('refid'):
            linkend = node['refid']
        else:
            linkend = self.document.nameids[node['refname']]
        self.body.append(self.emptytag(node, 'footnoteref', linkend=linkend))
        # so footnote reference doesn't show up twice, like: [1] 1
        raise nodes.SkipNode

    # TODO: header

    def visit_hint(self, node):
        self.body.append(self.starttag(node, 'note'))
        self.body.append('\n<title>%s</title>\n' % (self.language.labels[node.tagname],))

    def depart_hint(self, node):
        self.body.append('</note>\n')

    def visit_image(self, node):
        atts = node.attributes.copy()
        atts['fileref'] = atts['uri']
        alt = None
        del atts['uri']
        if atts.has_key('alt'):
            alt = atts['alt']
            del atts['alt']
        if atts.has_key('height'):
            atts['depth'] = atts['height']
            del atts['height']
        # NOTE: using win32 port of xsltproc and docbook-stylesheets-1.51.1
        # I'm getting the following error when transforming:
        # Error C:\home\igor\src\gnome-xml\xpath.c:8023: Undefined 
        # namespace prefix xmlXPathCompiledEval: evaluation failed
        # When I switched to version 1.49 of the docbook-stylesheets
        # I didn't have this problem.
        self.body.append('<mediaobject>\n')
        self.body.append('<imageobject>\n')
        self.body.append(self.emptytag(node, 'imagedata', **atts))
        self.body.append('</imageobject>\n')
        if alt:
            self.body.append('<textobject><phrase>' \
                '%s</phrase></textobject>\n' % alt)
        self.body.append('</mediaobject>\n')

    def depart_image(self, node):
        pass

    def visit_important(self, node):
        self.body.append(self.starttag(node, 'important'))

    def depart_important(self, node):
        self.body.append('</important>')

    # @@@ Incomplete, pending a proper implementation on the
    # Parser/Reader end.
    def visit_interpreted(self, node):
        self.body.append('<constant>\n')

    def depart_interpreted(self, node):
        self.body.append('</constant>\n')

    def visit_label(self, node):
        # NOTE: getting label for "footnote" in ``visit_footnote``
        if isinstance(node.parent, nodes.footnote):
            raise nodes.SkipNode
        # TODO: handle citation label
        elif isinstance(node.parent, nodes.citation):
            raise nodes.SkipNode

    def depart_label(self, node):
        pass

    def visit_legend(self, node):
        pass

    def depart_legend(self, node):
        pass

    def visit_list_item(self, node):
        self.body.append(self.starttag(node, 'listitem'))

    def depart_list_item(self, node):
        self.body.append('</listitem>\n')

    def visit_literal(self, node):
         self.body.append('<literal>')

    def depart_literal(self, node):
        self.body.append('</literal>')

    def visit_literal_block(self, node):
        self.body.append(self.starttag(node, 'programlisting'))

    def depart_literal_block(self, node):
        self.body.append('</programlisting>\n')

    def visit_note(self, node):
        self.body.append(self.starttag(node, 'note'))
        self.body.append('\n<title>%s</title>\n' % (self.language.labels[node.tagname],))

    def depart_note(self, node):
        self.body.append('</note>\n')

    def visit_option(self, node):
        self.body.append(self.starttag(node, 'command'))
        if self.context[-1]:
            self.body.append(', ')

    def depart_option(self, node):
        self.context[-1] += 1
        self.body.append('</command>')

    def visit_option_argument(self, node):
        self.body.append(node.get('delimiter', ' '))
        self.body.append(self.starttag(node, 'replaceable', ''))

    def depart_option_argument(self, node):
        self.body.append('</replaceable>')

    def visit_option_group(self, node):
        self.body.append(self.starttag(node, 'entry'))
        self.context.append(0)

    def depart_option_group(self, node):
        self.context.pop()
        self.body.append('</entry>\n')

    def visit_option_list(self, node):
        self.body.append(self.starttag(node, 'informaltable', frame='all'))
        self.body.append('<tgroup cols="2">\n')
        self.body.append('<colspec colname="option_col"/>\n')
        self.body.append('<colspec colname="description_col"/>\n')
        self.body.append('<thead>\n')
        self.body.append('<row>\n')
        # FIXME: shouldn't hardcode everything...
        self.body.append('<entry align="center">Option</entry>\n')
        self.body.append('<entry align="center">Description</entry>\n')
        self.body.append('</row>\n')
        self.body.append('</thead>\n')
        self.body.append('<tbody>\n')

    def depart_option_list(self, node):
        self.body.append('</tbody>')
        self.body.append('</tgroup>\n')
        self.body.append('</informaltable>\n')

    def visit_option_list_item(self, node):
        self.body.append(self.starttag(node, 'row'))

    def depart_option_list_item(self, node):
        self.body.append('</row>\n')

    def visit_option_string(self, node):
        pass

    def depart_option_string(self, node):
        pass

    def visit_organization(self, node):
        self.biblio['orgname'] = node.astext()
        raise nodes.SkipNode

    def visit_paragraph(self, node):
        self.body.append(self.starttag(node, 'para', ''))

    def depart_paragraph(self, node):
        self.body.append('</para>\n')

    # TODO: problematic
    visit_problematic = depart_problematic = lambda self, node: None

    def visit_raw(self, node):
        if node.has_key('format') and node['format'] == 'docbook':
            self.body.append(node.astext())
        raise node.SkipNode

    def visit_reference(self, node):
        atts = {}
        if node.has_key('refuri'):
            atts['url'] = node['refuri']
            self.context.append('ulink')
        elif node.has_key('refid'):
            atts['linkend'] = node['refid']
            self.context.append('link')
        elif node.has_key('refname'):
            atts['linkend'] = self.document.nameids[node['refname']]
            self.context.append('link')
        self.body.append(self.starttag(node, self.context[-1], '', **atts))

    def depart_reference(self, node):
        self.body.append('</%s>' % (self.context.pop(),))

    # TODO: revision
    visit_revision = depart_revision = lambda self,node: None

    def visit_row(self, node):
        self.entry_level = 0
        self.body.append(self.starttag(node, 'row'))

    def depart_row(self, node):
        self.body.append('</row>\n')

    def visit_section(self, node):
        if self.section == 0 and self.doctype == 'book':
            self.body.append(self.starttag(node, 'chapter'))
        else:
            self.body.append(self.starttag(node, 'section'))
        self.section += 1

    def depart_section(self, node):
        self.section -= 1
        if self.section == 0 and self.doctype == 'book':
            self.body.append('</chapter>\n')
        else:
            self.body.append('</section>\n')

    # TODO: status
    visit_status = depart_status = lambda self,node: None

    def visit_strong(self, node):
        self.body.append(self.starttag(node, 'emphasis', role='strong'))

    def depart_strong(self, node):
        self.body.append('</emphasis>\n')

    def visit_substitution_definition(self, node):
        raise nodes.SkipNode

    def visit_substitution_reference(self, node):
        self.unimplemented_visit(node)

    def visit_subtitle(self, node):
        self.body.append(self.starttag(node, 'subtitle'))

    def depart_subtitle(self, node):
        self.body.append('</subtitle>\n')

    # TODO: system_message
    visit_system_message = depart_system_message = lambda self, node: None

    def visit_table(self, node):
        self.body.append(
            self.starttag(node, 'informaltable', frame='all')
        )

    def depart_table(self, node):
        self.body.append('</informaltable>\n')

    # TODO: target
    visit_target = depart_target = lambda self,node: None

    def visit_tbody(self, node):
        self.body.append(self.starttag(node, 'tbody'))

    def depart_tbody(self, node):
        self.body.append('</tbody>\n')

    def visit_term(self, node):
        self.body.append(self.starttag(node, 'term'))
        self.body.append('<varname>')

    def depart_term(self, node):
        # Leave the end tag "term" to ``visit_definition()``,
        # in case there's a classifier.
        self.body.append('</varname>')

    def visit_tgroup(self, node):
        self.colnames = []
        atts = {'cols': node['cols']}
        self.body.append(self.starttag(node, 'tgroup', **atts))

    def depart_tgroup(self, node):
        self.body.append('</tgroup>\n')

    def visit_thead(self, node):
        self.body.append(self.starttag(node, 'thead'))

    def depart_thead(self, node):
        self.body.append('</thead>\n')

    def visit_tip(self, node):
        self.body.append(self.starttag(node, 'tip'))

    def depart_tip(self, node):
        self.body.append('</tip>\n')

    def visit_title(self, node):
        self.body.append(self.starttag(node, 'title'))

    def depart_title(self, node):
        self.body.append('</title>\n')

    def visit_topic(self, node):
        # NOTE: Table of Contents are handled by DocBook
        if node.get('class') == 'contents':
            raise nodes.SkipChildren
        elif node.get('class') == 'abstract':
            self.body.append(self.starttag(node, 'abstract'))
            self.context.append('abstract')
        else:
            print node
            print `node`
            print dir(node)
            self.unimplemented_visit(node)

    def depart_topic(self, node):
        if len(self.context):
            self.body.append('</%s>\n' % (self.context.pop(),))

    # QUESTION: what to do for "transition"?
    def visit_transition(self, node):
        pass

    def depart_transition(self, node):
        pass

    def visit_version(self, node):
        # TODO: include in {doctype}info/edition?
        raise nodes.SkipNode

    def visit_warning(self, node):
        self.body.append(self.starttag(node, 'warning'))

    def depart_warning(self, node):
        self.body.append('</warning>\n')

    def unimplemented_visit(self, node):
        raise NotImplementedError('visiting unimplemented node type: %s'
                % node.__class__.__name__)

# :collapseFolds=1:folding=indent:indentSize=4:lineSeparator=\n:noTabs=true:tabSize=4:
