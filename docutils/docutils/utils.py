#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Miscellaneous utilities for the documentation utilities.
"""

__docformat__ = 'reStructuredText'

import sys
import os
import os.path
from types import StringType, UnicodeType
from docutils import ApplicationError, DataError
from docutils import frontend, nodes


class SystemMessage(ApplicationError):

    def __init__(self, system_message):
        Exception.__init__(self, system_message.astext())


class Reporter:

    """
    Info/warning/error reporter and ``system_message`` element generator.

    Five levels of system messages are defined, along with corresponding
    methods: `debug()`, `info()`, `warning()`, `error()`, and `severe()`.

    There is typically one Reporter object per process.  A Reporter object is
    instantiated with thresholds for reporting (generating warnings) and
    halting processing (raising exceptions), a switch to turn debug output on
    or off, and an I/O stream for warnings.  These are stored in the default
    reporting category, '' (zero-length string).

    Multiple reporting categories [#]_ may be set, each with its own reporting
    and halting thresholds, debugging switch, and warning stream
    (collectively a `ConditionSet`).  Categories are hierarchical dotted-name
    strings that look like attribute references: 'spam', 'spam.eggs',
    'neeeow.wum.ping'.  The 'spam' category is the ancestor of
    'spam.bacon.eggs'.  Unset categories inherit stored conditions from their
    closest ancestor category that has been set.

    When a system message is generated, the stored conditions from its
    category (or ancestor if unset) are retrieved.  The system message level
    is compared to the thresholds stored in the category, and a warning or
    error is generated as appropriate.  Debug messages are produced iff the
    stored debug switch is on.  Message output is sent to the stored warning
    stream.

    The default category is '' (empty string).  By convention, Writers should
    retrieve reporting conditions from the 'writer' category (which, unless
    explicitly set, defaults to the conditions of the default category).

    .. [#] The concept of "categories" was inspired by the log4j project:
       http://jakarta.apache.org/log4j/.
    """

    levels = 'DEBUG INFO WARNING ERROR SEVERE'.split()
    """List of names for system message levels, indexed by level."""

    def __init__(self, report_level, halt_level, stream=None, debug=0):
        """
        Initialize the `ConditionSet` forthe `Reporter`'s default category.

        :Parameters:

            - `report_level`: The level at or above which warning output will
              be sent to `stream`.
            - `halt_level`: The level at or above which `SystemMessage`
              exceptions will be raised, halting execution.
            - `debug`: Show debug (level=0) system messages?
            - `stream`: Where warning output is sent.  Can be file-like (has a
              ``.write`` method), a string (file name, opened for writing), or
              `None` (implies `sys.stderr`; default).
        """
        if stream is None:
            stream = sys.stderr
        elif type(stream) in (StringType, UnicodeType):
            raise NotImplementedError('This should open a file for writing.')

        self.categories = {'': ConditionSet(debug, report_level, halt_level,
                                            stream)}
        """Mapping of category names to conditions. Default category is ''."""

    def set_conditions(self, category, report_level, halt_level,
                       stream=None, debug=0):
        if stream is None:
            stream = sys.stderr
        self.categories[category] = ConditionSet(debug, report_level,
                                                 halt_level, stream)

    def unset_conditions(self, category):
        if category and self.categories.has_key(category):
            del self.categories[category]

    __delitem__ = unset_conditions

    def get_conditions(self, category):
        while not self.categories.has_key(category):
            category = category[:category.rfind('.') + 1][:-1]
        return self.categories[category]

    __getitem__ = get_conditions

    def system_message(self, level, comment=None, category='',
                       *children, **attributes):
        """
        Return a system_message object.

        Raise an exception or generate a warning if appropriate.
        """
        msg = nodes.system_message(comment, level=level,
                                   type=self.levels[level],
                                   *children, **attributes)
        debug, report_level, halt_level, stream = self[category].astuple()
        if level >= report_level or debug and level == 0:
            if category:
                print >>stream, 'Reporter "%s":' % category, msg.astext()
            else:
                print >>stream, 'Reporter:', msg.astext()
        if level >= halt_level:
            raise SystemMessage(msg)
        return msg

    def debug(self, comment=None, category='', *children, **attributes):
        """
        Level-0, "DEBUG": an internal reporting issue. Typically, there is no
        effect on the processing. Level-0 system messages are handled
        separately from the others.
        """
        return self.system_message(
              0, comment, category, *children, **attributes)

    def info(self, comment=None, category='', *children, **attributes):
        """
        Level-1, "INFO": a minor issue that can be ignored. Typically there is
        no effect on processing, and level-1 system messages are not reported.
        """
        return self.system_message(
              1, comment, category, *children, **attributes)

    def warning(self, comment=None, category='', *children, **attributes):
        """
        Level-2, "WARNING": an issue that should be addressed. If ignored,
        there may be unpredictable problems with the output.
        """
        return self.system_message(
              2, comment, category, *children, **attributes)

    def error(self, comment=None, category='', *children, **attributes):
        """
        Level-3, "ERROR": an error that should be addressed. If ignored, the
        output will contain errors.
        """
        return self.system_message(
              3, comment, category, *children, **attributes)

    def severe(self, comment=None, category='', *children, **attributes):
        """
        Level-4, "SEVERE": a severe error that must be addressed. If ignored,
        the output will contain severe errors. Typically level-4 system
        messages are turned into exceptions which halt processing.
        """
        return self.system_message(
              4, comment, category, *children, **attributes)


class ConditionSet:

    """
    A set of two thresholds (`report_level` & `halt_level`), a switch
    (`debug`), and an I/O stream (`stream`), corresponding to one `Reporter`
    category.
    """

    def __init__(self, debug, report_level, halt_level, stream):
        self.debug = debug
        self.report_level = report_level
        self.halt_level = halt_level
        self.stream = stream

    def astuple(self):
        return (self.debug, self.report_level, self.halt_level,
                self.stream)


class ExtensionAttributeError(DataError): pass
class BadAttributeError(ExtensionAttributeError): pass
class BadAttributeDataError(ExtensionAttributeError): pass
class DuplicateAttributeError(ExtensionAttributeError): pass


def extract_extension_attributes(field_list, attribute_spec):
    """
    Return a dictionary mapping extension attribute names to converted values.

    :Parameters:
        - `field_list`: A flat field list without field arguments, where each
          field body consists of a single paragraph only.
        - `attribute_spec`: Dictionary mapping known attribute names to a
          conversion function such as `int` or `float`.

    :Exceptions:
        - `KeyError` for unknown attribute names.
        - `ValueError` for invalid attribute values (raised by the conversion
           function).
        - `DuplicateAttributeError` for duplicate attributes.
        - `BadAttributeError` for invalid fields.
        - `BadAttributeDataError` for invalid attribute data (missing name,
          missing data, bad quotes, etc.).
    """
    attlist = extract_attributes(field_list)
    attdict = assemble_attribute_dict(attlist, attribute_spec)
    return attdict

def extract_attributes(field_list):
    """
    Return a list of attribute (name, value) pairs from field names & bodies.

    :Parameter:
        `field_list`: A flat field list without field arguments, where each
        field body consists of a single paragraph only.

    :Exceptions:
        - `BadAttributeError` for invalid fields.
        - `BadAttributeDataError` for invalid attribute data (missing name,
          missing data, bad quotes, etc.).
    """
    attlist = []
    for field in field_list:
        if len(field) != 2:
            raise BadAttributeError(
                  'extension attribute field may not contain field arguments')
        name = str(field[0].astext().lower())
        body = field[1]
        if len(body) == 0:
            data = None
        elif len(body) > 1 or not isinstance(body[0], nodes.paragraph) \
              or len(body[0]) != 1 or not isinstance(body[0][0], nodes.Text):
            raise BadAttributeDataError(
                  'extension attribute field body may contain\n'
                  'a single paragraph only (attribute "%s")' % name)
        else:
            data = body[0][0].astext()
        attlist.append((name, data))
    return attlist

def assemble_attribute_dict(attlist, attspec):
    """
    Return a mapping of attribute names to values.

    :Parameters:
        - `attlist`: A list of (name, value) pairs (the output of
          `extract_attributes()`).
        - `attspec`: Dictionary mapping known attribute names to a
          conversion function such as `int` or `float`.

    :Exceptions:
        - `KeyError` for unknown attribute names.
        - `DuplicateAttributeError` for duplicate attributes.
        - `ValueError` for invalid attribute values (raised by conversion
           function).
    """
    attributes = {}
    for name, value in attlist:
        convertor = attspec[name]       # raises KeyError if unknown
        if attributes.has_key(name):
            raise DuplicateAttributeError('duplicate attribute "%s"' % name)
        try:
            attributes[name] = convertor(value)
        except (ValueError, TypeError), detail:
            raise detail.__class__('(attribute: "%s"; value: %r)\n%s'
                                   % (name, value, detail))
    return attributes


class NameValueError(DataError): pass


def extract_name_value(line):
    """
    Return a list of (name, value) from a line of the form "name=value ...".

    :Exception:
        `NameValueError` for invalid input (missing name, missing data, bad
        quotes, etc.).
    """
    attlist = []
    while line:
        equals = line.find('=')
        if equals == -1:
            raise NameValueError('missing "="')
        attname = line[:equals].strip()
        if equals == 0 or not attname:
            raise NameValueError(
                  'missing attribute name before "="')
        line = line[equals+1:].lstrip()
        if not line:
            raise NameValueError(
                  'missing value after "%s="' % attname)
        if line[0] in '\'"':
            endquote = line.find(line[0], 1)
            if endquote == -1:
                raise NameValueError(
                      'attribute "%s" missing end quote (%s)'
                      % (attname, line[0]))
            if len(line) > endquote + 1 and line[endquote + 1].strip():
                raise NameValueError(
                      'attribute "%s" end quote (%s) not followed by '
                      'whitespace' % (attname, line[0]))
            data = line[1:endquote]
            line = line[endquote+1:].lstrip()
        else:
            space = line.find(' ')
            if space == -1:
                data = line
                line = ''
            else:
                data = line[:space]
                line = line[space+1:].lstrip()
        attlist.append((attname.lower(), data))
    return attlist

def normalize_name(name):
    """Return a case- and whitespace-normalized name."""
    return ' '.join(name.lower().split())

def new_document(options=None):
    if options is None:
        options = frontend.OptionParser().get_default_values()
    reporter = Reporter(options.report_level, options.halt_level,
                        options.warning_stream, options.debug)
    document = nodes.document(options=options, reporter=reporter)
    return document

def clean_rcs_keywords(paragraph, keyword_substitutions):
    if len(paragraph) == 1 and isinstance(paragraph[0], nodes.Text):
        textnode = paragraph[0]
        for pattern, substitution in keyword_substitutions:
            match = pattern.match(textnode.data)
            if match:
                textnode.data = pattern.sub(substitution, textnode.data)
                return

def relative_uri(source, target):
    """
    Build and return a URI to `target`, relative to `source`.

    If there is no common prefix, return the absolute path to `target`.
    """
    source_parts = os.path.abspath(source).split(os.sep)
    target_parts = os.path.abspath(target).split(os.sep)
    if source_parts[:2] != target_parts[:2]:
        # Nothing in common between paths.  Return absolute path.
        return '/'.join(target_parts)
    source_parts.reverse()
    target_parts.reverse()
    while source_parts[-1] == target_parts[-1]:
        # Remove path components in common:
        source_parts.pop()
        target_parts.pop()
    target_parts.reverse()
    parts = ['..'] * (len(source_parts) - 1) + target_parts
    return '/'.join(parts)
