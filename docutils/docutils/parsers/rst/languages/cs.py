# Author: Marek Blaha
# Contact: mb@dat.cz
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

# New language mappings are welcome.  Before doing a new translation, please
# read <http://docutils.sf.net/docs/howto/i18n.html>.  Two files must be
# translated for each language: one in docutils/languages, the other in
# docutils/parsers/rst/languages.

"""
Czech-language mappings for language-dependent features of
reStructuredText.
"""

__docformat__ = 'reStructuredText'


directives = {
      # language-dependent: fixed
      u'pozor': 'attention',
      u'caution': 'caution',       # jak rozlisit caution a warning?
      u'nebezpe\u010D\u00ED': 'danger',
      u'chyba': 'error',
      u'rada': 'hint',
      u'd\u016Fle\u017Eit\u00E9': 'important',
      u'pozn\u00E1mka': 'note',
      u'tip': 'tip',
      u'varov\u00E1n\u00ED': 'warning',
      u'admonition': 'admonition',
      u'sidebar': 'sidebar',
      u't\u00E9ma': 'topic',
      u'line-block': 'line-block',
      u'parsed-literal': 'parsed-literal',
      u'odd\u00EDl': 'rubric',
      u'moto': 'epigraph',
      u'highlights': 'highlights',
      u'pull-quote': 'pull-quote',
      u'compound (translation required)': 'compound',
      #'questions': 'questions',
      #'qa': 'questions',
      #'faq': 'questions',
      u'table (translation required)': 'table',
      u'csv-table (translation required)': 'csv-table',
      u'meta': 'meta',
      #'imagemap': 'imagemap',
      u'image': 'image',   # obrazek
      u'figure': 'figure', # a tady?
      u'include': 'include',
      u'raw': 'raw',
      u'replace': 'replace',
      u'unicode': 'unicode',
      u't\u0159\u00EDda': 'class',
      u'role (translation required)': 'role',
      u'obsah': 'contents',
      u'sectnum': 'sectnum',
      u'section-numbering': 'sectnum',
      #'footnotes': 'footnotes',
      #'citations': 'citations',
      u'target-notes': 'target-notes',
      u'restructuredtext-test-directive': 'restructuredtext-test-directive'}
"""Czech name to registered (in directives/__init__.py) directive name
mapping."""

roles = {
    # language-dependent: fixed
    u'abbreviation (translation required)': 'abbreviation',
    u'ab (translation required)': 'abbreviation',
    u'acronym (translation required)': 'acronym',
    u'ac (translation required)': 'acronym',
    u'index (translation required)': 'index',
    u'i (translation required)': 'index',
    u'subscript (translation required)': 'subscript',
    u'sub (translation required)': 'subscript',
    u'superscript (translation required)': 'superscript',
    u'sup (translation required)': 'superscript',
    u'title-reference (translation required)': 'title-reference',
    u'title (translation required)': 'title-reference',
    u't (translation required)': 'title-reference',
    u'pep-reference (translation required)': 'pep-reference',
    u'pep (translation required)': 'pep-reference',
    u'rfc-reference (translation required)': 'rfc-reference',
    u'rfc (translation required)': 'rfc-reference',
    u'emphasis (translation required)': 'emphasis',
    u'strong (translation required)': 'strong',
    u'literal (translation required)': 'literal',
    u'named-reference (translation required)': 'named-reference',
    u'anonymous-reference (translation required)': 'anonymous-reference',
    u'footnote-reference (translation required)': 'footnote-reference',
    u'citation-reference (translation required)': 'citation-reference',
    u'substitution-reference (translation required)': 'substitution-reference',
    u'target (translation required)': 'target',
    u'uri-reference (translation required)': 'uri-reference',
    u'uri (translation required)': 'uri-reference',
    u'url (translation required)': 'uri-reference',
    u'raw (translation required)': 'raw',}
"""Mapping of Czech role names to canonical role names for interpreted text.
"""
