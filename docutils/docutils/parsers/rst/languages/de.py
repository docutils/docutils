# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
German-language mappings for language-dependent features of
reStructuredText.
"""

__docformat__ = 'reStructuredText'


directives = {
      'Achtung': 'attention',
      'Vorsicht': 'caution',
      'Gefahr': 'danger',
      'Fehler': 'error',
      'Hinweis': 'hint',
      'Wichtig': 'important',
      'Notiz': 'note',
      'Tip': 'tip',
      'Warnung': 'warning',
      'Topic': 'topic',  # Inhalt, Thema or Überbegriff
      'line-block': 'line-block',
      'parsed-literal': 'parsed-literal',
      #'questions': 'questions',
      #'qa': 'questions',
      #'faq': 'questions',
      'meta': 'meta',
      #'imagemap': 'imagemap',
      'Bild': 'image',
      'figure': 'figure', # also Bild ?
      #'raw': 'raw',
      'Inhalt': 'contents',
      'sectnum': 'sectnum',
      'section-numbering': 'sectnum',
      'target-notes': 'target-notes',
      #'footnotes': 'footnotes',
      #'citations': 'citations',
      'restructuredtext-test-directive': 'restructuredtext-test-directive'}
"""English name to registered (in directives/__init__.py) directive name
mapping."""
