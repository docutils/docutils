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
      u'attention': u'attention',
      u'pr\u00E9caution': u'caution',
      u'danger': u'danger',
      u'erreur': u'error',
      u'conseil': u'hint',
      u'important': u'important',
      u'note': u'note',
      u'astuce': u'tip',
      u'avertissement': u'warning',
      u'encadr\u00E9': u'sidebar',
      u'sujet': u'topic',
      u'bloc-textuel': u'line-block',
      u'bloc-interpr\u00E9t\u00E9': u'parsed-literal',
      u'code-interpr\u00E9t\u00E9': u'parsed-literal',
      #u'questions': u'questions',
      #u'qr': u'questions',
      #u'faq': u'questions',
      u'meta': u'meta',
      #u'imagemap (translation required)': u'imagemap',
      u'image': u'image',
      u'figure': u'figure',
      u'inclure': u'include',
      u'brut': u'raw',
      u'remplacer': u'replace',
      u'sommaire': u'contents',
      u'table-des-mati\u00E8res': u'contents',
      u'sectnum': u'sectnum',
      u'section-num\u00E9rot\u00E9e': u'sectnum',
      u'liens': u'target-notes',
      #u'footnotes (translation required)': u'footnotes',
      #u'citations (translation required)': u'citations',
      }
"""French name to registered (in directives/__init__.py) directive name
mapping."""
