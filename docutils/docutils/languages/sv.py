#! /usr/bin/env python

"""
:Author:    Adam Chodorowski
:Contact:   chodorowski@users.sourceforge.net
:Revision:  $Revision$
:Date:      $Date$
:Copyright: This module has been placed in the public domain.

Swedish language mappings for language-dependent features of Docutils.
"""

__docformat__ = 'reStructuredText'


from docutils import nodes


labels = {
    'author': 'Författare',
    'authors': 'Författare',
    'organization': 'Organisation',
    'contact': 'Kontakt',
    'version': 'Version',
    'revision': 'Revision',
    'status': 'Status',
    'date': 'Datum',
    'copyright': 'Copyright',
    'abstract': 'Sammanfattning',
    'attention': 'Observera!',
    'caution': 'Varning!',
    'danger': 'FARA!',
    'error': 'Fel',
    'hint': 'Vägledning',
    'important': 'Viktigt',
    'note': 'Notera',
    'tip': 'Tips',
    'warning': 'Varning',
    'contents': 'Innehåll'}
"""Mapping of node class name to label text."""

bibliographic_fields = {
    'författare': nodes.authors,
    'organisation': nodes.organization,
    'kontakt': nodes.contact,
    'version': nodes.version,
    'revision': nodes.revision,
    'status': nodes.status,
    'datum': nodes.date,
    'copyright': nodes.copyright,
    'sammanfattning': nodes.topic}
"""Field name (lowcased) to node class name mapping for bibliographic fields
(field_list)."""

author_separators = [';', ',']
"""List of separator strings for the 'Authors' bibliographic field. Tried in
order."""
