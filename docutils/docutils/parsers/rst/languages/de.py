# -*- coding: iso-8859-1 -*-
# Author: Engelbert Gruber
# Contact: grubert@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
German-language mappings for language-dependent features of
reStructuredText.
"""

__docformat__ = 'reStructuredText'


directives = {
      'achtung': 'attention',
      'vorsicht': 'caution',
      'gefahr': 'danger',
      'fehler': 'error',
      'hinweis': 'hint',
      'wichtig': 'important',
      'notiz': 'note',
      'tip': 'tip',
      'warnung': 'warning',
      'admonition (translation required)': 'admonition',
      'sidebar (translation required)': 'sidebar', # kasten ?
      'topic': 'topic',  # Überbegriff, Thema ?
      'line-block': 'line-block',
      'parsed-literal': 'parsed-literal',
      'rubrik': 'rubric',
      'epigraph (translation required)': 'epigraph',
      'highlights (translation required)': 'highlights',
      'pull-quote (translation required)': 'pull-quote', # kasten too ?
      #'questions': 'questions',
      #'qa': 'questions',
      #'faq': 'questions',
      'meta': 'meta',
      #'imagemap': 'imagemap',
      'bild': 'image',
      'abbildung': 'figure',
      'raw': 'raw',         # unbearbeitet
      'include': 'include', # einfügen, "füge ein" would be more like a command.
                            # einfügung would be the noun. 
      'replace': 'replace', # ersetzen, ersetze
      'unicode': 'unicode',
      'class (translation required)': 'class',
      'inhalt': 'contents',
      'sectnum': 'sectnum',
      'section-numbering': 'sectnum',
      'target-notes': 'target-notes',
      #'footnotes': 'footnotes',
      #'citations': 'citations',
      'restructuredtext-test-directive': 'restructuredtext-test-directive'}
"""English name to registered (in directives/__init__.py) directive name
mapping."""
