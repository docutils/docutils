# Authors: David Goodger; William Dode
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

# New language mappings are welcome.  Before doing a new translation, please
# read <http://docutils.sf.net/spec/howto/i18n.html>.  Two files must be
# translated for each language: one in docutils/languages, the other in
# docutils/parsers/rst/languages.

"""
French-language mappings for language-dependent features of
reStructuredText.
"""

__docformat__ = 'reStructuredText'


directives = {
      u'attention': 'attention',
      u'pr\u00E9caution': 'caution',
      u'danger': 'danger',
      u'erreur': 'error',
      u'conseil': 'hint',
      u'important': 'important',
      u'note': 'note',
      u'astuce': 'tip',
      u'avertissement': 'warning',
      u'admonition': 'admonition',
      u'encadr\u00E9': 'sidebar',
      u'sujet': 'topic',
      u'bloc-textuel': 'line-block',
      u'bloc-interpr\u00E9t\u00E9': 'parsed-literal',
      u'code-interpr\u00E9t\u00E9': 'parsed-literal',
      u'intertitre': 'rubric',
      u'exergue': 'epigraph',
      u'\u00E9pigraphe': 'epigraph',
      u'chapeau': 'highlights',
      u'accroche': 'pull-quote',
      #u'questions': 'questions',
      #u'qr': 'questions',
      #u'faq': 'questions',
      u'meta': 'meta',
      #u'imagemap (translation required)': 'imagemap',
      u'image': 'image',
      u'figure': 'figure',
      u'inclure': 'include',
      u'brut': 'raw',
      u'remplacer': 'replace',
      u'unicode': 'unicode',
      u'classe': 'class',
      u'sommaire': 'contents',
      u'table-des-mati\u00E8res': 'contents',
      u'sectnum': 'sectnum',
      u'section-num\u00E9rot\u00E9e': 'sectnum',
      u'liens': 'target-notes',
      #u'footnotes (translation required)': 'footnotes',
      #u'citations (translation required)': 'citations',
      }
"""French name to registered (in directives/__init__.py) directive name
mapping."""

roles = {
      u'abbreviation (translation required)': 'abbreviation',
      u'acronym (translation required)': 'acronym',
      u'index (translation required)': 'index',
      u'subscript (translation required)': 'subscript',
      u'superscript (translation required)': 'superscript',
      u'title-reference (translation required)': 'title-reference',
      u'pep-reference (translation required)': 'pep-reference',
      u'rfc-reference (translation required)': 'rfc-reference',
      u'emphasis (translation required)': 'emphasis',
      u'strong (translation required)': 'strong',
      u'literal (translation required)': 'literal',
      u'named-reference (translation required)': 'named-reference',
      u'anonymous-reference (translation required)': 'anonymous-reference',
      u'footnote-reference (translation required)': 'footnote-reference',
      u'citation-reference (translation required)': 'citation-reference',
      u'substitution-reference (translation required)': 'substitution-reference',
      u'target (translation required)': 'target',
      u'uri-reference (translation required)': 'uri-reference',}
"""Mapping of French role names to canonical role names for interpreted text.
"""
