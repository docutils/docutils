# -*- coding: utf-8 -*-
# Author: Panjunyong
# Contact: panjy@zopechina.com
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

# New language mappings are welcome.  Before doing a new translation, please
# read <http://docutils.sf.net/docs/howto/i18n.html>.  Two files must be
# translated for each language: one in docutils/languages, the other in
# docutils/parsers/rst/languages.

"""
Simplified Chinese language mappings for language-dependent features of
reStructuredText.
"""

__docformat__ = 'reStructuredText'


directives = {
      # language-dependent: fixed
      '注意': 'attention',
      '小心': 'caution',
      '危险': 'danger',
      '错误': 'error',
      '提示': 'hint',
      '重要': 'important',
      '注解': 'note',
      '技巧': 'tip',
      '警告': 'warning',
      '忠告': 'admonition',
      '侧框': 'sidebar',
      '主题': 'topic',
      'line-block (translation required)': 'line-block',
      'parsed-literal (translation required)': 'parsed-literal',
      '醒目': 'rubric',
      '铭文': 'epigraph',
      '要点': 'highlights',
      'pull-quote (translation required)': 'pull-quote',
      '复合': 'compound',
      '容器': 'container',
      #'questions (translation required)': 'questions',
      '表格': 'table',
      'csv表格': 'csv-table',
      '列表表格': 'list-table',
      #'qa (translation required)': 'questions',
      #'faq (translation required)': 'questions',
      '元数据': 'meta',
      #'imagemap (translation required)': 'imagemap',
      '图象': 'image',
      '图例': 'figure',
      '包含': 'include',
      '原文': 'raw',
      '代替': 'replace',
      '统一码': 'unicode',
      '类型': 'class',
      '角色': 'role',
      '默认角色': 'default-role',
      '标题': 'title',
      '目录': 'contents',
      '章节序号': 'sectnum',
      '题头': 'header',
      '页脚': 'footer',
      #'footnotes (translation required)': 'footnotes',
      #'citations (translation required)': 'citations',
      'target-notes (translation required)': 'target-notes',
      'restructuredtext-test-directive': 'restructuredtext-test-directive'}
"""Traditional Chinese name to registered (in directives/__init__.py)
directive name mapping."""

roles = {
    # language-dependent: fixed
    '缩写': 'abbreviation',
    '简称': 'acronym',
    'index (translation required)': 'index',
    'i (translation required)': 'index',
    '下标': 'subscript',
    '上标': 'superscript',
    'title-reference (translation required)': 'title-reference',
    'title (translation required)': 'title-reference',
    't (translation required)': 'title-reference',
    'pep-reference (translation required)': 'pep-reference',
    'pep (translation required)': 'pep-reference',
    'rfc-reference (translation required)': 'rfc-reference',
    'rfc (translation required)': 'rfc-reference',
    '强调': 'emphasis',
    '加粗': 'strong',
    '字面': 'literal',
    'named-reference (translation required)': 'named-reference',
    'anonymous-reference (translation required)': 'anonymous-reference',
    'footnote-reference (translation required)': 'footnote-reference',
    'citation-reference (translation required)': 'citation-reference',
    'substitution-reference (translation required)': 'substitution-reference',
    'target (translation required)': 'target',
    'uri-reference (translation required)': 'uri-reference',
    'uri (translation required)': 'uri-reference',
    'url (translation required)': 'uri-reference',
    'raw (translation required)': 'raw',}
"""Mapping of Traditional Chinese role names to canonical role names for
interpreted text."""

# Decode UTF-8 strings.  (We cannot use unicode literals directly for
# Python 2.1 compatibility.)
for mapping in directives, roles:
    for key, value in mapping.items():
        del mapping[key]
        mapping[unicode(key, 'utf8')] = unicode(value, 'utf8')
