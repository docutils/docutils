#! /usr/bin/env python

"""
:Author:    Adam Chodorowski
:Contact:   chodorowski@users.sourceforge.net
:Revision:  $Revision$
:Date:      $Date$
:Copyright: This module has been placed in the public domain.

Swedish language mappings for language-dependent features of reStructuredText.
"""

__docformat__ = 'reStructuredText'


directives = {
      'observera': 'attention',
      'varning': 'caution',
      'fara': 'danger',
      'fel': 'error',
      'vägledning': 'hint',
      'viktigt': 'important',
      'notera': 'note',
      'tips': 'tip',
      'varning': 'warning',
      'frågor': 'questions',
      'frågor-och-svar': 'questions', # NOTE: A bit long, but recommended by
      'vanliga-frågor': 'questions',  # NOTE: http://www.nada.kth.se/dataterm/
      'meta': 'meta',
      #'bildkarta': 'imagemap', # FIXME: Translation might be to literal.
      'bild': 'image',
      'figur': 'figure',
      #'rå': 'raw',             # FIXME: Translation might be to literal.
      'innehåll': 'contents',
      #'fotnoter': 'footnotes',
      #'citeringar': 'citations',
      #'ämne': 'topic',
      'restructuredtext-test-directive': 'restructuredtext-test-directive'}
"""Swedish name to registered (in directives/__init__.py) directive name mapping."""
