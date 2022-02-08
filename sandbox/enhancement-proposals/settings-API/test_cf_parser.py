#!/usr/bin/env python3
# :Copyright: © 2022 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

# :Id: $Id:  $

# test_cf_parser.py: test the new ConfigParser class
# ============================================================

import configparser
from cf_parser import ConfigParser

# Setup
# =====

# an example selection of active sections (similar to "rst2latex.py")::

active_sections = ('general',
                   'readers', 'standalone reader',
                   'parsers', 'rst parser',
                   'general', # duplicate!
                   'writers', 'latex writers', 'latex2e writer')

# all Docutils settings whose settings specification entry contains
# ``'validator': validate_encoding_and_error_handler``::

encoding_settings = ('input_encoding', 'output_encoding', 'error_encoding')

# all Docutils settings whose settings specification entry contains
# ``'action': 'append'``::

appending_settings = {# setting name, list separator, # validator
    'smartquotes_locales': ', ',        # validate_smartquotes_locales
    'table_style': ',',                 # validate_comma_separated_list
    'strip_classes': ',',               # validate_strip_class
    'strip_elements_with_classes': ',', # validate_strip_class
    'expose_internals': ':',            # validate_colon_separated_string_list
    }

# some settings have list values that overwrite earlier list values
# instead of appending to them::
#
# list values: stylesheet
#              stylesheet_path
#              stylesheet_dirs
#              

# all Docutils settings whose settings specification entry contains
# ``'overrides': <value>``
overriding_settings = {
    'footnote_references': 'trim_footnote_reference_space',
    'stylesheet': 'stylesheet_path',
    'stylesheet_path': 'stylesheet',
    'theme': 'theme_url',
    'theme_url': 'theme',
    }


# Docutils configuration file parser instance
cfp = ConfigParser(active_sections=active_sections,
                   encoding_settings=encoding_settings,
                   appending_settings=appending_settings,
                   overriding_settings=overriding_settings)
cfp.read('test.conf')


# Original configuration file parser instance
cfp_o = configparser.ConfigParser()
cfp_o.read('test.conf')



# Mapping interface
# =================

# configparser.ConfigParser instances provide a  `mapping interface`
# with some adaptions.

# Keys are normalized to lowercase.
# Docutils adds normalization of '-' to '_'::

assert dict(cfp_o['general']) == {'generator': 'off',
                                'smart-quotes': 'yes'}
assert dict(cfp['general']) == {'generator': 'off',
                                'smart_quotes': 'yes'}

# Iterating over an instance returns section names
# in  the order they were added to the parser::

# for s in cfp:
#     print(s)

# This can be used in functions that expect a sequence::

# print(tuple(cfp_o))
assert tuple(cfp_o)[:3] == ('DEFAULT', 'general', 'html4css1 writer')

# The Docutils ConfigParser adds an ACTIVE section:

# print(tuple(cfp)[:3])
assert tuple(cfp)[:3] == ('DEFAULT', 'ACTIVE', 'general')

# emty section:
assert dict(cfp_o['xml writer']) == {}


# the ConfigParser instance is mutable::

cfp_o['DEFAULT'].update(cfp_o['general'])
cfp_o['DEFAULT'].update(cfp_o['latex writers'])

# print(dict(cfp_o['DEFAULT']))
assert dict(cfp_o['DEFAULT']) == {'generator': 'on',
                                  'smart-quotes': 'yes',
                                  'strip-elements-with-classes': 'no-latex',
                                  'stylesheet-path': 'silly/example.sty',
                                  }
# delete all DEFAULT values
cfp_o['DEFAULT'].clear()


# Differences to dict of dicts

# Defaults show up in all sections::

# 'pseudoxml writer' section has no entries in "test.conf":
# print(dict(cfp_o['pseudoxml writer']))
assert dict(cfp_o['xml writer']) == dict(cfp_o['DEFAULT'])

# cfp_o.pop('DEFAULT') # ValueError: Cannot remove the default section.

# The second value for `ConfigParser().get()` is NOT the default value::

# cfp_o.get('silly', {}) # configparser.NoSectionError: No section: 'silly'



# Docutils setting handlers
# =========================

# encoding settings may have an error handler after a colon::

# print(dict(cfp_o['pseudoxml writer']))
assert dict(cfp_o['pseudoxml writer']) == {'output_encoding':
                                               'ascii:backslashreplace',
                                           }
# print(dict(cfp['pseudoxml writer']))
assert dict(cfp['pseudoxml writer']) == {'output_encoding': 'ascii',
                                         'output_encoding_error_handler':
                                             'backslashreplace',
                                         }

# some settings override other settings::

# print(cfp['latex2e writer']['stylesheet'])
assert 'stylesheet_path' not in cfp['latex2e writer']
# print(cfp_o['latex2e writer']['stylesheet_path'])


# Collect active settings
# =======================

# settings from "active" sections are collected in section ACTIVE::


# print(tuple(s for s in cfp.active_sections if s in cfp))
assert tuple(s for s in cfp.active_sections if s in cfp) == (
           'general', 'latex writers', 'latex2e writer')

# print(f"assert len(cfp['ACTIVE']) == {len(cfp['ACTIVE'])}")
# for key, value in cfp['ACTIVE'].items():
#     print(f"assert cfp['ACTIVE']['{key}'] == '{value}'")
assert len(cfp['ACTIVE']) == 8
# from [general]
assert cfp['ACTIVE']['generator'] == 'on' # overridden from [latex writers]
assert cfp['ACTIVE']['smart_quotes'] == 'yes'
# from [latex writers]
assert cfp['ACTIVE']['strip_elements_with_classes'] == 'no-latex'
# from [latex2e writer]
assert cfp['ACTIVE']['stylesheet'] == 'microtype, enumitem'
assert cfp['ACTIVE']['hyperref_options'] == 'citecolor=black'
assert cfp['ACTIVE']['use_latex_citations'] == 'yes'
assert cfp['ACTIVE']['sectnum_xform'] == 'no'
assert cfp['ACTIVE']['use_latex_abstract'] == 'yes'


# Overlay additional files:
# reading returns a list of successfully read files
read_ok = cfp.read(('test2.conf', 'test3.conf', 'test-nicht-da.conf'))
# print(read_ok)
assert read_ok == ['test2.conf', 'test3.conf']

active_and_present = tuple(s for s in cfp.active_sections if s in cfp)
#print(active_and_present) # includes now also "writers" section

# print(f"assert len(cfp['ACTIVE']) == {len(cfp['ACTIVE'])}")
# for key, value in cfp['ACTIVE'].items():
#     print(f"assert cfp['ACTIVE']['{key}'] == '{value}'")

assert len(cfp['ACTIVE']) == 12
assert cfp['ACTIVE']['generator'] == 'on' # overridden in test2.conf
assert cfp['ACTIVE']['smart_quotes'] == 'yes'
assert cfp['ACTIVE']['stylesheet'] == 'microtype, enumitem'
assert cfp['ACTIVE']['smart_quotes'] == 'yes'
assert cfp['ACTIVE']['strip_elements_with_classes'] == 'no-latex'
assert cfp['ACTIVE']['stylesheet'] == 'microtype, enumitem'
assert cfp['ACTIVE']['hyperref_options'] == 'citecolor=black'
assert cfp['ACTIVE']['use_latex_citations'] == 'yes'
assert cfp['ACTIVE']['sectnum_xform'] == 'no'
assert cfp['ACTIVE']['use_latex_abstract'] == 'yes'
# new in test2.conf
assert cfp['ACTIVE']['language_code'] == 'ru' # overridden in test3.conf
# new in test3.conf
assert cfp['ACTIVE']['expose_internals'] == 'line:source'
assert cfp['ACTIVE']['trim_footnote_reference_space'] == 'yes'
# new in test2.conf but why in last position? because of a deleted entry?
assert cfp['ACTIVE']['footnote_references'] == 'superscript'


# Collect appending list settings
# ===============================

cfp.active_sections = ('general', 
                       'parsers', 'rst parser',
                       'writers', 'html4css1 writer', 'pep_html writer')
cfp.clear()
# print(dict(cfp))
assert dict(cfp) == {'DEFAULT': cfp['DEFAULT'],
                     'ACTIVE': cfp['ACTIVE']}

# Read test samples with list settings:
read_ok = cfp.read(('config_list.txt', 'config_list_2.txt'))
# print(tuple(cfp))

# print(f"assert len(cfp['ACTIVE']) == {len(cfp['ACTIVE'])}")
# for key, value in cfp['ACTIVE'].items():
#     print(f"assert cfp['ACTIVE']['{key}'] == {repr(value)}")

assert len(cfp['ACTIVE']) == 5
assert cfp['ACTIVE']['expose_internals'] == 'a:b:c:d:e:f'
assert cfp['ACTIVE']['strip_classes'] == 'spam,pan,\nfun,parrot,ham,  eggs'
assert cfp['ACTIVE']['strip_elements_with_classes'] == ('sugar, flour,milk,'
                                                        'safran,eggs,salt')
assert cfp['ACTIVE']['smartquotes_locales'] == ('de: «»‹›, nl: „”’’,\n'
                                                'cs: »«›‹, fr: « : »:‹ : ›')
assert cfp['ACTIVE']['stylesheet'] == 'style2.css,\nstyle3.css'
