"""
:Author: Miroslav Va¹ko
:Contact: zemiak@zoznam.sk
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Slovak-language mappings for language-dependent features of Docutils.
"""

__docformat__ = 'reStructuredText'


from docutils import nodes


labels = {
      'author': 'Autor',
      'authors': 'Autori',
      'organization': 'Organizácia',
      'address': 'Adresa',
      'contact': 'Kontakt',
      'version': 'Verzia',
      'revision': 'Revízia',
      'status': 'Stav',
      'date': 'Dátum',
      'copyright': 'Copyright',
      'dedication': 'Venovanie',
      'abstract': 'Abstraktne',
      'attention': 'Pozor!',
      'caution': 'Opatrne!',
      'danger': '!NEBEZPEÈENSTVO!',
      'error': 'Chyba',
      'hint': 'Rada',
      'important': 'Dôle¾ité',
      'note': 'Poznámka',
      'tip': 'Tip',
      'warning': 'Varovanie',
      'contents': 'Obsah'}
"""Mapping of node class name to label text."""

bibliographic_fields = {
      'author': nodes.author,
      'authors': nodes.authors,
      'organization': nodes.organization,
      'address': nodes.address,
      'contact': nodes.contact,
      'version': nodes.version,
      'revision': nodes.revision,
      'status': nodes.status,
      'date': nodes.date,
      'copyright': nodes.copyright,
      'dedication': nodes.topic,
      'abstract': nodes.topic}
"""Field name (lowcased) to node class name mapping for bibliographic fields
(field_list)."""

author_separators = [';', ',']
"""List of separator strings for the 'Authors' bibliographic field. Tried in
order."""
