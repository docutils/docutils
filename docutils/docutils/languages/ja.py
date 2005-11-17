# Author: Hisashi Morita
# Contact: hisashim@kt.rim.or.jp
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

# New language mappings are welcome.  Before doing a new translation, please
# read <http://docutils.sf.net/docs/howto/i18n.html>.  Two files must be
# translated for each language: one in docutils/languages, the other in
# docutils/parsers/rst/languages.

"""
Japanese-language mappings for language-dependent features of Docutils.
"""

__docformat__ = 'reStructuredText'

labels = {
      # fixed: language-dependent
      'author': u'\u8457\u8005',
      'authors': u'\u8457\u8005',
      'organization': u'\u7d44\u7e54',
      'address': u'\u4f4f\u6240',
      'contact': u'\u9023\u7d61\u5148',
      'version': u'\u30d0\u30fc\u30b8\u30e7\u30f3',
      'revision': u'\u30ea\u30d3\u30b8\u30e7\u30f3',
      'status': u'30b9\u30c6\u30fc\u30bf\u30b9',
      'date': u'\u65e5\u4ed8',
      'copyright': u'\u8457\u4f5c\u6a29',
      'dedication': u'\u732e\u8f9e',
      'abstract': u'\u6982\u8981',
      'attention': u'\u6ce8\u76ee\u0021',
      'caution': u'\u6ce8\u610f\u0021',
      'danger': u'\u0021\u5371\u967a\u0021',
      'error': u'\u30a8\u30e9\u30fc',
      'hint': u'\u30d2\u30f3\u30c8',
      'important': u'\u91cd\u8981',
      'note': u'\u5099\u8003',
      'tip': u'\u0054\u0069\u0070',
      'warning': u'\ufeff\u8b66\u544a',
      'contents': u'\u76ee\u6b21'}
"""Mapping of node class name to label text."""

bibliographic_fields = {
      # language-dependent: fixed
      'author (translation required)': 'author',
      'authors (translation required)': 'authors',
      'organization (translation required)': 'organization',
      'address (translation required)': 'address',
      'contact (translation required)': 'contact',
      'version (translation required)': 'version',
      'revision (translation required)': 'revision',
      'status (translation required)': 'status',
      'date (translation required)': 'date',
      'copyright (translation required)': 'copyright',
      'dedication (translation required)': 'dedication',
      'abstract (translation required)': 'abstract'}
"""Japanese (lowcased) to canonical name mapping for bibliographic fields."""

author_separators = [';', ',']
"""List of separator strings for the 'Authors' bibliographic field. Tried in
order."""
