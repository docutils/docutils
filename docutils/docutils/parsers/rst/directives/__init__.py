#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

This package contains directive implementation modules.

The interface for directive functions is as follows::

    def directivefn(match, type_name, data, state, state_machine, attributes)

Where:

- ``match`` is a regular expression match object which matched the first line
  of the directive. ``match.group(1)`` gives the directive name.
- ``type_name`` is the directive type or name.
- ``data`` contains the remainder of the first line of the directive after the
  "::".
- ``state`` is the state which called the directive function.
- ``state_machine`` is the state machine which controls the state which called
  the directive function.
- ``attributes`` is a dictionary of extra attributes which may be added to the
  element the directive produces.  Currently, only an "alt" attribute is
  passed by substitution definitions (value: the substitution name), which may
  be used by an embedded image directive.

Directive functions return a tuple of two values:

- a list of nodes which will be inserted into the document tree at the point
  where the directive was encountered (can be an empty list), and
- a boolean: true iff the directive block finished at a blank line.
"""

__docformat__ = 'reStructuredText'

from docutils.parsers.rst.languages import en as _fallback_language_module


_directive_registry = {
      'attention': ('admonitions', 'attention'),
      'caution': ('admonitions', 'caution'),
      'danger': ('admonitions', 'danger'),
      'error': ('admonitions', 'error'),
      'important': ('admonitions', 'important'),
      'note': ('admonitions', 'note'),
      'tip': ('admonitions', 'tip'),
      'hint': ('admonitions', 'hint'),
      'warning': ('admonitions', 'warning'),
      'topic': ('body', 'topic'),
      'line-block': ('body', 'line_block'),
      'parsed-literal': ('body', 'parsed_literal'),
      #'questions': ('body', 'question_list'),
      'image': ('images', 'image'),
      'figure': ('images', 'figure'),
      'contents': ('parts', 'contents'),
      'sectnum': ('parts', 'sectnum'),
      #'footnotes': ('parts', 'footnotes'),
      #'citations': ('parts', 'citations'),
      'target-notes': ('references', 'target_notes'),
      'meta': ('html', 'meta'),
      #'imagemap': ('html', 'imagemap'),
      #'raw': ('misc', 'raw'),
      'restructuredtext-test-directive': ('misc', 'directive_test_function'),}
"""Mapping of directive name to (module name, function name). The directive
'name' is canonical & must be lowercase; language-dependent names are defined
in the language package."""

_modules = {}
"""Cache of imported directive modules."""

_directives = {}
"""Cache of imported directive functions."""

def directive(directive_name, language_module):
    """
    Locate and return a directive function from its language-dependent name.
    If not found in the current language, check English.
    """
    normname = directive_name.lower()
    if _directives.has_key(normname):
        return _directives[normname]
    try:
        canonicalname = language_module.directives[normname]
    except (KeyError, AttributeError):
        try:
            # Try English as a fallback:
            canonicalname = _fallback_language_module.directives[normname]
        except KeyError:
            # The canonical name should be an English name, but just in case:
            canonicalname = normname
    try:
        modulename, functionname = _directive_registry[canonicalname]
    except KeyError:
        return None
    if _modules.has_key(modulename):
        module = _modules[modulename]
    else:
        try:
            module = __import__(modulename, globals(), locals())
        except ImportError:
            return None
    try:
        function = getattr(module, functionname)
    except AttributeError:
        return None
    return function
