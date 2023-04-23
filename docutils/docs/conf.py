#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
import time

sys.path.insert(0, os.path.abspath('.'))
sys.path.insert(0, os.path.abspath('..'))

# -----------------------------------------------------------------------------

extensions = [
  'sphinx.ext.autodoc',
  'sphinx.ext.extlinks', 
  'sphinx.ext.napoleon', 
  'sphinx.ext.intersphinx',
  'sphinx.ext.viewcode'
]

needs_sphinx = '6.1'

project = 'docutils'
# pylint: disable-next=C0209
project_copyright = '2021 - %s' % time.strftime('%Y')

root_doc = 'index'
source_suffix = '.rst'
include_patterns = ['*.rst']
templates_path = ['_templates']
exclude_patterns = ['_build', *templates_path]

add_module_names = False
python_use_unqualified_type_names = True

# -----------------------------------------------------------------------------
# Options for HTML output
# -----------------------------------------------------------------------------

html_static_path = ['_static']
html_last_updated_fmt = '%b %d, %Y'
html_split_index = True
html_search_language = 'en'
html_domain_indices = True
html_theme = 'sphinx_rtd_theme'
html_theme_options = {
    'display_version': True,
    'style_external_links': False,
    'navigation_depth': -1,
    'includehidden': False
}

# -----------------------------------------------------------------------------
# Built-in Sphinx options
# -----------------------------------------------------------------------------

nitpick_ignore_regex = [
    (r'py:.*', r'builtins\..*'),
    (r'py:.*', r'docutils\..*'),
]
suppress_warnings = ['toc']

# -----------------------------------------------------------------------------
# Options for sphinx.ext.autodoc
# -----------------------------------------------------------------------------

autodoc_default_options = {
    'ignore-module-all': True,  # force 'bysource' sorting
    'show-inheritance': True,
    'special-members': False,  # handled by Napoleon
    'inherited-members': False,
    'undoc-public-members': True
}
autodoc_member_order = 'bysource'
autodoc_typehints = 'both'
autodoc_typehints_description_target = 'documented'

# -----------------------------------------------------------------------------
# Options for sphinx.ext.autosummary
# -----------------------------------------------------------------------------

autosummary_generate = False

# -----------------------------------------------------------------------------
# Options for sphinx.ext.extlinks
# -----------------------------------------------------------------------------

extlinks_detect_hardcoded_links = True
DOCUTILS_URL = 'https://docutils.sourceforge.io/docs/ref'
extlinks = {
    'dudtd': (f'{DOCUTILS_URL}/doctree.html#%s', '%s'),
    'duref': (f'{DOCUTILS_URL}/rst/restructuredtext.html#%s', '%s'),
    'durole': (f'{DOCUTILS_URL}/rst/roles.html#%s', '%s'),
    'dudir': (f'{DOCUTILS_URL}/rst/directives.html#%s', '%s')
}

# -----------------------------------------------------------------------------
# Options for sphinx.ext.intersphinx
# -----------------------------------------------------------------------------

intersphinx_mapping = {
    'python': ('https://docs.python.org/3', None)
}

# -----------------------------------------------------------------------------
# Options for sphinx.ext.napoleon
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# This extension is used for parsing inner sections and include documented
# members independently of whether they are public, private or special.
# -----------------------------------------------------------------------------

napoleon_include_init_with_doc = True
napoleon_include_private_with_doc = True
napoleon_include_special_with_doc = True
