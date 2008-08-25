# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision: 4229 $
# Date: $Date: 2005-12-23 00:46:16 +0100 (Fri, 23 Dec 2005) $
# Copyright: This module has been placed in the public domain.

# New language mappings are welcome.  Before doing a new translation, please
# read <http://docutils.sf.net/docs/howto/i18n.html>.  Two files must be
# translated for each language: one in docutils/languages, the other in
# docutils/parsers/rst/languages.

"""
Galician-language mappings for language-dependent features of
reStructuredText.
"""

__docformat__ = 'reStructuredText'


directives = {
      # language-dependent: fixed
      'atenci\xf3n': 'attention',
      'advertencia': 'caution',
      'perigo': 'danger',
      'erro': 'error',
      'pista': 'hint',
      'importante': 'important',
      'nota': 'note',
      'consello': 'tip',
      'aviso': 'warning',
      'admonici\xf3n': 'admonition',
      'barra lateral': 'sidebar',
      't\xf3pico': 'topic',
      'bloque-li\xf1a': 'line-block',
      'literal-analizado': 'parsed-literal',
      'r\xfabrica': 'rubric',
      'ep\xedgrafe': 'epigraph',
      'realzados': 'highlights',
      'coller-citaci\xf3n': 'pull-quote',
      'compor': 'compound',
      'recipiente': 'container',
      #'questions': 'questions',
      't\xe1boa': 'table',
      't\xe1boa-csv': 'csv-table',
      't\xe1boa-listaxe': 'list-table',
      #'qa': 'questions',
      #'faq': 'questions',
      'meta': 'meta',
      #'imagemap': 'imagemap',
      'imaxe': 'image',
      'figura': 'figure',
      'inclu\xedr': 'include',
      'cru': 'raw',
      'substitu\xedr': 'replace',
      'unicode': 'unicode',
      'data': 'date',
      'clase': 'class',
      'regra': 'role',
      'regra-predeterminada': 'default-role',
      't\xedtulo': 'title',
      'contido': 'contents',
      'seccnum': 'sectnum',
      'secci\xf3n-numerar': 'sectnum',
      'cabeceira': 'header',
      'p\xe9 de p\xe1xina': 'footer',
      #'footnotes': 'footnotes',
      #'citations': 'citations',
      'notas-destino': 'target-notes',
      'texto restruturado-proba-directiva': 'restructuredtext-test-directive'}
"""Galician name to registered (in directives/__init__.py) directive name
mapping."""

roles = {
    # language-dependent: fixed
    'abreviatura': 'abbreviation',
    'ab': 'abbreviation',
    'acr\xf3nimo': 'acronym',
    'ac': 'acronym',
    '\xedndice': 'index',
    'i': 'index',
    'sub\xedndice': 'subscript',
    'sub': 'subscript',
    'super\xedndice': 'superscript',
    'sup': 'superscript',
    'referencia t\xedtulo': 'title-reference',
    't\xedtulo': 'title-reference',
    't': 'title-reference',
    'referencia-pep': 'pep-reference',
    'pep': 'pep-reference',
    'referencia-rfc': 'rfc-reference',
    'rfc': 'rfc-reference',
    '\xe9nfase': 'emphasis',
    'forte': 'strong',
    'literal': 'literal',
    'referencia-nome': 'named-reference',
    'referencia-an\xf3nimo': 'anonymous-reference',
    'referencia-nota ao p\xe9': 'footnote-reference',
    'referencia-citaci\xf3n': 'citation-reference',
    'referencia-substituci\xf3n': 'substitution-reference',
    'destino': 'target',
    'referencia-uri': 'uri-reference',
    'uri': 'uri-reference',
    'url': 'uri-reference',
    'cru': 'raw',}
"""Mapping of Galician role names to canonical role names for interpreted text.
"""

 	  	 
