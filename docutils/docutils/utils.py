#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Miscellaneous utilities for the documentation utilities.
"""

import sys, re
import nodes


class SystemMessage(Exception):

    def __init__(self, system_message):
        Exception.__init__(self, system_message.astext())


class Reporter:

    """
    Info/warning/error reporter and ``system_message`` element generator.

    Five levels of system messages are defined, along with corresponding
    methods: `debug()`, `info()`, `warning()`, `error()`, and `severe()`.

    There is typically one Reporter object per process. A Reporter object is
    instantiated with thresholds for generating warnings and errors (raising
    exceptions), a switch to turn debug output on or off, and an I/O stream
    for warnings. These are stored in the default reporting category, ''
    (zero-length string).

    Multiple reporting categories [#]_ may be set, each with its own warning
    and error thresholds, debugging switch, and warning stream (collectively a
    `ConditionSet`). Categories are hierarchically-named strings that look
    like attribute references: 'spam', 'spam.eggs', 'neeeow.wum.ping'. The
    'spam' category is the ancestor of 'spam.bacon.eggs'. Unset categories
    inherit stored conditions from their closest ancestor category that has
    been set.

    When a system message is generated, the stored conditions from its
    category (or ancestor if unset) are retrieved. The system message level is
    compared to the thresholds stored in the category, and a warning or error
    is generated as appropriate. Debug messages are produced iff the stored
    debug switch is on. Message output is sent to the stored warning stream.

    The default category is '' (empty string). By convention, Writers should
    retrieve reporting conditions from the 'writer' category (which, unless
    explicitly set, defaults to the conditions of the default category).

    .. [#] The concept of "categories" was inspired by the log4j project:
       http://jakarta.apache.org/log4j/.
    """

    levels = 'DEBUG INFO WARNING ERROR SEVERE'.split()
    """List of names for system message levels, indexed by level."""

    def __init__(self, warninglevel, errorlevel, stream=None, debug=0):
        """
        Initialize the `ConditionSet` forthe `Reporter`'s default category.

        :Parameters:

            - `warninglevel`: The level at or above which warning output will
              be sent to `stream`.
            - `errorlevel`: The level at or above which `SystemMessage`
              exceptions will be raised.
            - `debug`: Show debug (level=0) system messages?
            - `stream`: Where warning output is sent (`None` implies
              `sys.stderr`).
        """

        if stream is None:
            stream = sys.stderr

        self.categories = {'': ConditionSet(debug, warninglevel, errorlevel,
                                            stream)}
        """Mapping of category names to conditions. Default category is ''."""

    def setconditions(self, category, warninglevel, errorlevel,
                      stream=None, debug=0):
        if stream is None:
            stream = sys.stderr
        self.categories[category] = ConditionSet(debug, warninglevel,
                                                 errorlevel, stream)

    def unsetconditions(self, category):
        if category and self.categories.has_key(category):
            del self.categories[category]

    __delitem__ = unsetconditions

    def getconditions(self, category):
        while not self.categories.has_key(category):
            category = category[:category.rfind('.') + 1][:-1]
        return self.categories[category]

    __getitem__ = getconditions

    def system_message(self, level, comment=None, category='',
                       *children, **attributes):
        """
        Return a system_message object.

        Raise an exception or generate a warning if appropriate.
        """
        msg = nodes.system_message(comment, level=level,
                                   type=self.levels[level],
                                   *children, **attributes)
        debug, warninglevel, errorlevel, stream = self[category].astuple()
        if level >= warninglevel or debug and level == 0:
            if category:
                print >>stream, 'Reporter "%s":' % category, msg.astext()
            else:
                print >>stream, 'Reporter:', msg.astext()
        if level >= errorlevel:
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
    A set of thresholds, switches, and streams corresponding to one `Reporter`
    category.
    """

    def __init__(self, debug, warninglevel, errorlevel, stream):
        self.debug = debug
        self.warninglevel = warninglevel
        self.errorlevel = errorlevel
        self.stream = stream

    def astuple(self):
        return (self.debug, self.warninglevel, self.errorlevel,
                self.stream)


class ExtensionAttributeError(Exception): pass
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
        name = field[0].astext().lower()
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
            raise detail.__class__('(attribute "%s", value "%r") %s'
                                   % (name, value, detail))
    return attributes


class NameValueError(Exception): pass


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


def normname(name):
    """Return a case- and whitespace-normalized name."""
    return ' '.join(name.lower().split())

def id(string):
    """
    Convert `string` into an identifier and return it.

    Docutils identifiers will conform to the regular expression
    ``[a-z][-a-z0-9]*``. For CSS compatibility, identifiers (the "class" and
    "id" attributes) should have no underscores, colons, or periods. Hyphens
    may be used.

    - The `HTML 4.01 spec`_ defines identifiers based on SGML tokens:

          ID and NAME tokens must begin with a letter ([A-Za-z]) and may be
          followed by any number of letters, digits ([0-9]), hyphens ("-"),
          underscores ("_"), colons (":"), and periods (".").

    - However the `CSS1 spec`_ defines identifiers based on the "name" token,
      a tighter interpretation ("flex" tokenizer notation; "latin1" and
      "escape" 8-bit characters have been replaced with entities)::

          unicode     \\[0-9a-f]{1,4}
          latin1      [&iexcl;-&yuml;]
          escape      {unicode}|\\[ -~&iexcl;-&yuml;]
          nmchar      [-a-z0-9]|{latin1}|{escape}
          name        {nmchar}+

    The CSS1 "nmchar" rule does not include underscores ("_"), colons (":"),
    or periods ("."), therefore "class" and "id" attributes should not contain
    these characters. They should be replaced with hyphens ("-"). Combined
    with HTML's requirements (the first character must be a letter; no
    "unicode", "latin1", or "escape" characters), this results in the
    ``[a-z][-a-z0-9]*`` pattern.

    .. _HTML 4.01 spec: http://www.w3.org/TR/html401
    .. _CSS1 spec: http://www.w3.org/TR/REC-CSS1
    """
    id = non_id_chars.sub('-', normname(string))
    id = non_id_at_ends.sub('', id)
    return str(id)

non_id_chars = re.compile('[^a-z0-9]+')
non_id_at_ends = re.compile('^[-0-9]+|-+$')

def newdocument(languagecode='en', warninglevel=2, errorlevel=4,
                stream=None, debug=0):
    reporter = Reporter(warninglevel, errorlevel, stream, debug)
    document = nodes.document(languagecode=languagecode, reporter=reporter)
    return document
