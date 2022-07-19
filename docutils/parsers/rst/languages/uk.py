# -*- coding: utf-8 -*-
# $Id$
# Author: Dmytro Kazanzhy <dkazanzhy@gmail.com>
# Copyright: This module has been placed in the public domain.

# New language mappings are welcome.  Before doing a new translation, please
# read <http://docutils.sf.net/docs/howto/i18n.html>.  Two files must be
# translated for each language: one in docutils/languages, the other in
# docutils/parsers/rst/languages.

"""
Ukrainian-language mappings for language-dependent features of
reStructuredText.
"""

__docformat__ = 'reStructuredText'

directives = {
 u'блок-строк': u'line-block',
 u'мета': u'meta',
 u'математика': 'math',
 u'оброблений-літерал': u'parsed-literal',
 u'виділена-цитата': u'pull-quote',
 u'код': 'code',
 u'складений абзац': 'compound',
 u'контейнер': 'container',
 u'таблиця': 'table',
 u'таблиця-csv': 'csv-table',
 u'таблиця-списків': 'list-table',
 u'сирий': u'raw',
 u'заміна': u'replace',
 u'тестова-директива-restructuredtext': u'restructuredtext-test-directive',
 u'цільові-виноски': u'target-notes',
 u'юнікод': u'unicode',
 u'дата': u'date',
 u'бічна-панель': u'sidebar',
 u'важливо': u'important',
 u'включати': u'include',
 u'увага': u'attention',
 u'виділення': u'highlights',
 u'зауваження': u'admonition',
 u'зображення': u'image',
 u'клас': u'class',
 u'роль': 'role',
 u'роль-за-замовчуванням': 'default-role',
 u'заголовок': 'title',
 u'номер-розділу': u'sectnum',
 u'нумерація-розділів': u'sectnum',
 u'небезпечно': u'danger',
 u'обережно': u'caution',
 u'помилка': u'error',
 u'підказка': u'tip',
 u'попередження': u'warning',
 u'примітка': u'note',
 u'малюнок': u'figure',
 u'рубрика': u'rubric',
 u'порада': u'hint',
 u'зміст': u'contents',
 u'тема': u'topic',
 u'епіграф': u'epigraph',
 u'верхній колонтитул': 'header',
 u'нижній колонтитул': 'footer',}
"""Ukrainian name to registered (in directives/__init__.py) directive name
mapping."""

roles = {
 u'акронім': 'acronym',
 u'код': 'code',
 u'анонімне-посилання': 'anonymous-reference',
 u'буквально': 'literal',
 u'математика': 'math',
 u'верхній-індекс': 'superscript',
 u'наголос': 'emphasis',
 u'іменоване-посилання': 'named-reference',
 u'індекс': 'index',
 u'нижній-індекс': 'subscript',
 u'жирне-накреслення': 'strong',
 u'скорочення': 'abbreviation',
 u'посилання-заміна': 'substitution-reference',
 u'посилання-на-pep': 'pep-reference',
 u'посилання-на-rfc': 'rfc-reference',
 u'посилання-на-uri': 'uri-reference',
 u'посилання-на-заголовок': 'title-reference',
 u'посилання-на-зноску': 'footnote-reference',
 u'посилання-на-цитату': 'citation-reference',
 u'ціль': 'target',
 u'сирий': 'raw',}
"""Mapping of Ukrainian role names to canonical role names for interpreted text.
"""
