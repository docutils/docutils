# -*- coding: utf-8 -*-
# $Id$
# Author: Dmytro Kazanzhy <dkazanzhy@gmail.com>
# Copyright: This module has been placed in the public domain.

# New language mappings are welcome.  Before doing a new translation, please
# read <http://docutils.sf.net/docs/howto/i18n.html>.  Two files must be
# translated for each language: one in docutils/languages, the other in
# docutils/parsers/rst/languages.

"""
Ukrainian-language mappings for language-dependent features of Docutils.
"""

__docformat__ = 'reStructuredText'

labels = {
      u'abstract': u'Анотація',
      u'address': u'Адреса',
      u'attention': u'Увага!',
      u'author': u'Автор',
      u'authors': u'Автори',
      u'caution': u'Обережно!',
      u'contact': u'Контакт',
      u'contents': u'Зміст',
      u'copyright': u'Права копіювання',
      u'danger': u'НЕБЕЗПЕЧНО!',
      u'date': u'Дата',
      u'dedication': u'Посвячення',
      u'error': u'Помилка',
      u'hint': u'Порада',
      u'important': u'Важливо',
      u'note': u'Примітка',
      u'organization': u'Організація',
      u'revision': u'Редакція',
      u'status': u'Статус',
      u'tip': u'Підказка',
      u'version': u'Версія',
      u'warning': u'Попередження'}
"""Mapping of node class name to label text."""

bibliographic_fields = {
      u'анотація': u'abstract',
      u'адреса': u'address',
      u'автор': u'author',
      u'автори': u'authors',
      u'контакт': u'contact',
      u'права копіювання': u'copyright',
      u'дата': u'date',
      u'посвячення': u'dedication',
      u'організація': u'organization',
      u'редакція': u'revision',
      u'статус': u'status',
      u'версія': u'version'}
"""Ukrainian (lowcased) to canonical name mapping for bibliographic fields."""

author_separators =  [';', ',']
"""List of separator strings for the 'Authors' bibliographic field. Tried in
order."""
