# -*- coding: utf-8 -*-
# $Id$
# Author: Roman Suzi <rnd@onego.ru>
# Copyright: This module has been placed in the public domain.

# New language mappings are welcome.  Before doing a new translation, please
# read <http://docutils.sf.net/docs/howto/i18n.html>.  Two files must be
# translated for each language: one in docutils/languages, the other in
# docutils/parsers/rst/languages.

"""
Russian-language mappings for language-dependent features of
reStructuredText.
"""

__docformat__ = 'reStructuredText'

directives = {
 u'блок-строк': u'line-block',
 u'meta': u'meta',
 u'math (translation required)': 'math',
 u'обработанный-литерал': u'parsed-literal',
 u'выделенная-цитата': u'pull-quote',
 u'code (translation required)': 'code',
 u'compound (translation required)': 'compound',
 u'container (translation required)': 'container',
 u'table (translation required)': 'table',
 u'csv-table (translation required)': 'csv-table',
 u'list-table (translation required)': 'list-table',
 u'сырой': u'raw',
 u'замена': u'replace',
 u'тестовая-директива-restructuredtext': u'restructuredtext-test-directive',
 u'целевые-сноски': u'target-notes',
 u'unicode': u'unicode',
 u'дата': u'date',
 u'боковая-полоса': u'sidebar',
 u'важно': u'important',
 u'включать': u'include',
 u'внимание': u'attention',
 u'выделение': u'highlights',
 u'замечание': u'admonition',
 u'изображение': u'image',
 u'класс': u'class',
 u'role (translation required)': 'role',
 u'default-role (translation required)': 'default-role',
 u'title (translation required)': 'title',
 u'номер-раздела': u'sectnum',
 u'нумерация-разделов': u'sectnum',
 u'опасно': u'danger',
 u'осторожно': u'caution',
 u'ошибка': u'error',
 u'подсказка': u'tip',
 u'предупреждение': u'warning',
 u'примечание': u'note',
 u'рисунок': u'figure',
 u'рубрика': u'rubric',
 u'совет': u'hint',
 u'содержание': u'contents',
 u'тема': u'topic',
 u'эпиграф': u'epigraph',
 u'header (translation required)': 'header',
 u'footer (translation required)': 'footer',}
"""Russian name to registered (in directives/__init__.py) directive name
mapping."""

roles = {
 u'акроним': 'acronym',
 u'code (translation required)': 'code',
 u'анонимная-ссылка': 'anonymous-reference',
 u'буквально': 'literal',
 u'math (translation required)': 'math',
 u'верхний-индекс': 'superscript',
 u'выделение': 'emphasis',
 u'именованная-ссылка': 'named-reference',
 u'индекс': 'index',
 u'нижний-индекс': 'subscript',
 u'сильное-выделение': 'strong',
 u'сокращение': 'abbreviation',
 u'ссылка-замена': 'substitution-reference',
 u'ссылка-на-pep': 'pep-reference',
 u'ссылка-на-rfc': 'rfc-reference',
 u'ссылка-на-uri': 'uri-reference',
 u'ссылка-на-заглавие': 'title-reference',
 u'ссылка-на-сноску': 'footnote-reference',
 u'цитатная-ссылка': 'citation-reference',
 u'цель': 'target',
 u'raw (translation required)': 'raw',}
"""Mapping of Russian role names to canonical role names for interpreted text.
"""
