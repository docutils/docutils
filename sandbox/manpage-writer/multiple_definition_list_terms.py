#!/usr/bin/env python3
# :Copyright: © 2023 Günter Milde.

#             Released without warranty under the terms of the
#             GNU General Public License (v. 2 or later)

# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause
#
# Test definition list items with extra terms  with Docutils writers
# =================================================================

# `Feature-Request #60`__ asks for the legitimation of definition list
# items with more than one term for two reasons:
# 
# * it is legal and supported in HTML,
# * the custom directive `"glossary"`_ may generate definition list items
#   with multiple terms.
#
# __ https://sourceforge.net/p/docutils/feature-requests/60/
# __ https://www.sphinx-doc.org/en/master/
#            usage/restructuredtext/directives.html#glossary
# 
# In Docutils <= 0.20, there is no rST syntax construct that can generate a 
# definition list item with more than one term.
#
# To allow testing and improving writer support, this skript generates a
# document tree programatically and exports it with the writers defined in
# Docutils to temporary files.

import difflib
import os.path

from docutils.core import publish_doctree, publish_from_doctree
from docutils.nodes import term, classifier

sample = """\
term 1
  definition 1
term 2a
  definition 2
term 3a : classifier 3a : classifier 3aa
  definition 3
"""
expected = """\
<document source="<string>">
    <definition_list>
        <definition_list_item>
            <term>
                term 1
            <definition>
                <paragraph>
                    definition 1
        <definition_list_item>
            <term>
                term 2a
            <term>
                term 2b
            <definition>
                <paragraph>
                    definition 2
        <definition_list_item>
            <term>
                term 3a
            <classifier>
                classifier 3a
            <classifier>
                classifier 3aa
            <term>
                term 3b
            <classifier>
                classifier 3b
            <definition>
                <paragraph>
                    definition 3
"""

settings = {'_disable_config': True,
            'datestamp': False,
            'generator': False,
            'embed_stylesheet': False,
            'indents': True,  # XML writer
            'use_latex_citations': True,
            'legacy_column_widths': False,            
            }


doctree = publish_doctree(sample, settings_overrides=settings)

# print(doctree)

# Insert extra <term> node:
# print(doctree[0][1].children)
doctree[0][1].insert(1, term('test', 'term 2b'))
# Insert extra <term> and <classifier> nodes:
# print(doctree[0][2].children)
doctree[0][2].insert(3, term('test', 'term 3b'))
doctree[0][2].insert(4, classifier('test', 'classifier 3b'))

output = publish_from_doctree(doctree, writer_name='pseudoxml',
                              settings_overrides=settings)
assert output.decode() == expected


extensions = {'html': 'html',
              'html4': 'html',
              'html5': 'html',
              'latex': 'tex',
              'xetex': 'tex',
              'manpage': 'man',
              'odt': 'odt',
              'xml': 'xml',
              'pseudoxml': 'txt',
              }

for format in extensions.keys():
    output = publish_from_doctree(doctree, writer_name=format,
                                  settings_overrides=settings)

    filename = f'termtest-{format}.{extensions[format]}'
    outf = f'output/{filename}'
    with open(outf, 'bw') as outfile:
        outfile.write(output)
        print(f'output written to {outf}')
    expf = f'expected/{filename}'
    if os.path.exists(expf):
        d = difflib.unified_diff(open(expf).readlines(), 
                                 open(outf).readlines(),
                                 fromfile=expf, tofile=outf)
        print("".join(list(d)))


# output = publish_from_doctree(doctree, writer_name='odf')
# with open('/tmp/termtest.troff', 'bw') as outfile:
#     outfile.write(output)
