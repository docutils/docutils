# :Author: Miroslav Vasko
# :Contact: zemiak@zoznam.sk
# :Revision: $Revision$
# :Date: $Date$
# :Copyright: This module has been placed in the public domain.

"""
Slovak-language mappings for language-dependent features of Docutils.
"""

__docformat__ = 'reStructuredText'

labels = {
      'author': u'Autor',
      'authors': u'Autori',
      'organization': u'Organiz\u00E1cia',
      'address': u'Adresa',
      'contact': u'Kontakt',
      'version': u'Verzia',
      'revision': u'Rev\u00EDzia',
      'status': u'Stav',
      'date': u'D\u00E1tum',
      'copyright': u'Copyright',
      'dedication': u'Venovanie',
      'abstract': u'Abstraktne',
      'attention': u'Pozor!',
      'caution': u'Opatrne!',
      'danger': u'!NEBEZPE\u010cENSTVO!',
      'error': u'Chyba',
      'hint': u'Rada',
      'important': u'D\u00F4le\u017Eit\u00E9',
      'note': u'Pozn\u00E1mka',
      'tip': u'Tip',
      'warning': u'Varovanie',
      'contents': u'Obsah'}
"""Mapping of node class name to label text."""

bibliographic_fields = {
      u'autor': 'author',
      u'autori': 'authors',
      u'organiz\u00E1cia': 'organization',
      u'adresa': 'address',
      u'kontakt': 'contact',
      u'verzia': 'version',
      u'rev\u00EDzia': 'revision',
      u'stav': 'status',
      u'd\u00E1tum': 'date',
      u'copyright': 'copyright',
      u'venovanie': 'dedication',
      u'abstraktne': 'abstract'}
"""Slovak (lowcased) to canonical name mapping for bibliographic fields."""

author_separators = [';', ',']
"""List of separator strings for the 'Authors' bibliographic field. Tried in
order."""
