# Author:    Adam Chodorowski
# Contact:   chodorowski@users.sourceforge.net
# Revision:  $Revision$
# Date:      $Date$
# Copyright: This module has been placed in the public domain.

# New language mappings are welcome.  Before doing a new translation, please
# read <http://docutils.sf.net/spec/howto/i18n.html>.  Two files must be
# translated for each language: one in docutils/languages, the other in
# docutils/parsers/rst/languages.

"""
Swedish language mappings for language-dependent features of reStructuredText.
"""

__docformat__ = 'reStructuredText'


directives = {
      u'observera': 'attention',
      u'caution (translation required)': 'caution',
      u'fara': 'danger',
      u'fel': 'error',
      u'v\u00e4gledning': 'hint',
      u'viktigt': 'important',
      u'notera': 'note',
      u'tips': 'tip',
      u'varning': 'warning',
      u'admonition (translation required)': 'admonition',
      u'sidebar (translation required)': 'sidebar',
      u'\u00e4mne': 'topic',
      u'line-block (translation required)': 'line-block',
      u'parsed-literal (translation required)': 'parsed-literal',
      u'mellanrubrik': 'rubric',
      u'epigraph (translation required)': 'epigraph',
      u'highlights (translation required)': 'highlights',
      u'pull-quote (translation required)': 'pull-quote',
      # u'fr\u00e5gor': 'questions',
      # NOTE: A bit long, but recommended by http://www.nada.kth.se/dataterm/:
      # u'fr\u00e5gor-och-svar': 'questions',
      # u'vanliga-fr\u00e5gor': 'questions',  
      u'meta': 'meta',
      # u'bildkarta': 'imagemap',   # FIXME: Translation might be too literal.
      u'bild': 'image',
      u'figur': 'figure',
      u'inkludera': 'include',   
      u'r\u00e5': 'raw',            # FIXME: Translation might be too literal.
      u'ers\u00e4tt': 'replace', 
      u'unicode': 'unicode',
      u'class (translation required)': 'class',
      u'inneh\u00e5ll': 'contents',
      u'sektionsnumrering': 'sectnum',
      u'target-notes (translation required)': 'target-notes',
      # u'fotnoter': 'footnotes',
      # u'citeringar': 'citations',
      }
"""Swedish name to registered (in directives/__init__.py) directive name
mapping."""
