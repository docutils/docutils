#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

This package contains directive implementation modules.

The interface for directive functions is as follows::

    def directivefn(match, type_name, data, state, state_machine,
                    option_presets):

Where:

- ``match`` is a regular expression match object which matched the first line
  of the directive. ``match.group(1)`` gives the directive name.
- ``type_name`` is the directive type or name.
- ``data`` contains the remainder of the first line of the directive after the
  "::".
- ``state`` is the state which called the directive function.
- ``state_machine`` is the state machine which controls the state which called
  the directive function.
- ``option_presets`` is a dictionary of preset options which may be added to
  the element the directive produces.  Currently, only an "alt" option is
  passed by substitution definitions (value: the substitution name), which may
  be used by an embedded image directive.

Directive functions return a tuple of two values:

- a list of nodes which will be inserted into the document tree at the point
  where the directive was encountered (can be an empty list), and
- a boolean: true iff the directive block finished at a blank line.
"""

__docformat__ = 'reStructuredText'

from docutils.parsers.rst.languages import en as _fallback_language_module
import docutils


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
      'raw': ('include', 'raw'),
      'include': ('include', 'include'),
      'replace': ('include', 'replace'),
      'restructuredtext-test-directive': ('misc', 'directive_test_function'),}
"""Mapping of directive name to (module name, function name). The directive
'name' is canonical & must be lowercase; language-dependent names are defined
in the language package."""

_modules = {}
"""Cache of imported directive modules."""

_directives = {}
"""Cache of imported directive functions."""



class DirectiveParseError(docutils.ApplicationError):
  def __init__(self, error, unparsed):
    Exception.__init__(self, error, unparsed)
    self.error = error
    self.unparsed = unparsed


#def parse_directive(match, type_name, data, state, state_machine, attributes,
#  attribute_spec={}):

def parse_directive(match, type_name, state, state_machine,
                    option_presets, arguments=None,
                    option_spec={}, content=None):
    """
    Parameters:

    - `match`, `type_name`, state`, `state_machine`, and
      `option_presets`: See `docutils.parsers.rst.directives.__init__`.
    - `arguments`: A 2-tuple of the number of ``(required,
      optional)`` whitespace-separated arguments to parse, or
      ``None`` if no arguments (same as ``(0, 0)``).  If an
      argument may contain whitespace (multiple words), specify
      only one argument (either required or optional); the client
      code must do any context-sensitive parsing.
    - `option_spec`: A dictionary, mapping known option names to
      conversion functions such as `int` or `float`.  ``None`` or
      an empty dict implies no options to parse.
    - `content`: A boolean; true if content is allowed.  Client
      code must handle the case where content is required but not
      supplied (an empty content list will be returned).

    Returns a 4-tuple: list of arguments, dict of options, list of
    strings (content block), and a boolean (blank finish).

    Or raises `DirectiveParseError` with arguments: node (system
    message), boolean (blank finish).
    """
    lineno = state_machine.abs_line_number()
    line_offset = state_machine.line_offset
    datablock, indent, offset, blank_finish = \
          state_machine.get_first_known_indented(match.end(), until_blank=1)
    blocktext = '\n'.join(state_machine.input_lines[
          line_offset : line_offset + len(datablock) + 1])
    for i in range(len(datablock)):
        if datablock[i][:1] == ':':
            attlines = datablock[i:]
            datablock = datablock[:i]
            break
    else:
        attlines = []
    attoffset = line_offset + i
    reference = ''.join([line.strip() for line in datablock])
    if attlines:
        success, data, blank_finish = state.parse_extension_attributes(
              attribute_spec, attlines, blank_finish)
        if success:                     # data is a dict of attributes
            attributes.update(data)
        else:                           # data is an error string
            error = state_machine.reporter.error(
                  'Error in "%s" directive attributes at line %s:\n%s.'
                  % (match.group(1), lineno, data), '',
                  nodes.literal_block(blocktext, blocktext))
            raise CheapException(error, blank_finish)

    return datablock, blocktext, blank_finish

def openAny(path):
    try:
      # is it a file?
      return open(path)
    except :
      try:
        # is it a url?
        return urlopen(path)
      except (URLError, ValueError):
        # treat as a string
        return StringIO(path)

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

def flag(argument):
    if argument and argument.strip():
        raise ValueError('no argument is allowed; "%s" supplied' % argument)
    else:
        return None

def unchanged(argument):
    return argument  # unchanged!

def format_values(values):
    return '%s, or "%s"' % (', '.join(['"%s"' % s for s in values[:-1]]),
                            values[-1])

def choice(argument, values):
    try:
        value = argument.lower().strip()
    except AttributeError:
        raise TypeError('must supply an argument; choose from %s'
                        % format_values(values))
    if value in values:
        return value
    else:
        raise ValueError('"%s" unknown; choose from %s'
                         % (argument, format_values(values)))
