# Authors: David Goodger; William Dode
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

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
      u'admonestation': 'admonition',
      u'encadr\u00E9': 'sidebar',
      u'sujet': 'topic',
      u'bloc-textuel': 'line-block',
      u'bloc-interpr\u00E9t\u00E9': 'parsed-literal',
      u'code-interpr\u00E9t\u00E9': 'parsed-literal',
      u'inter-titre': 'rubric',
      u'exergue': 'epigraph',
      u'chapeau': 'highlights',
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
