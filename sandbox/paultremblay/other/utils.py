# $Id: utils.py 7073 2011-07-07 06:49:19Z milde $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Miscellaneous utilities for the documentation utilities.
"""

__docformat__ = 'reStructuredText'

import sys
import os
import os.path
import warnings
import unicodedata
from docutils import ApplicationError, DataError
from docutils import nodes
from docutils.error_reporting import ErrorOutput, SafeString


class SystemMessage(ApplicationError):

    def __init__(self, system_message, level):
        Exception.__init__(self, system_message.astext())
        self.level = level


class SystemMessagePropagation(ApplicationError): pass


class Reporter:

    """
    Info/warning/error reporter and ``system_message`` element generator.

    Five levels of system messages are defined, along with corresponding
    methods: `debug()`, `info()`, `warning()`, `error()`, and `severe()`.

    There is typically one Reporter object per process.  A Reporter object is
    instantiated with thresholds for reporting (generating warnings) and
    halting processing (raising exceptions), a switch to turn debug output on
    or off, and an I/O stream for warnings.  These are stored as instance
    attributes.

    When a system message is generated, its level is compared to the stored
    thresholds, and a warning or error is generated as appropriate.  Debug
    messages are produced if the stored debug switch is on, independently of
    other thresholds.  Message output is sent to the stored warning stream if
    not set to ''.

    The Reporter class also employs a modified form of the "Observer" pattern
    [GoF95]_ to track system messages generated.  The `attach_observer` method
    should be called before parsing, with a bound method or function which
    accepts system messages.  The observer can be removed with
    `detach_observer`, and another added in its place.

    .. [GoF95] Gamma, Helm, Johnson, Vlissides. *Design Patterns: Elements of
       Reusable Object-Oriented Software*. Addison-Wesley, Reading, MA, USA,
       1995.
    """

    levels = 'DEBUG INFO WARNING ERROR SEVERE'.split()
    """List of names for system message levels, indexed by level."""

    # system message level constants:
    (DEBUG_LEVEL,
     INFO_LEVEL,
     WARNING_LEVEL,
     ERROR_LEVEL,
     SEVERE_LEVEL) = range(5)

    def __init__(self, source, report_level, halt_level, stream=None,
                 debug=0, encoding=None, error_handler='backslashreplace'):
        """
        :Parameters:
            - `source`: The path to or description of the source data.
            - `report_level`: The level at or above which warning output will
              be sent to `stream`.
            - `halt_level`: The level at or above which `SystemMessage`
              exceptions will be raised, halting execution.
            - `debug`: Show debug (level=0) system messages?
            - `stream`: Where warning output is sent.  Can be file-like (has a
              ``.write`` method), a string (file name, opened for writing),
              '' (empty string) or `False` (for discarding all stream messages)
              or `None` (implies `sys.stderr`; default).
            - `encoding`: The output encoding.
            - `error_handler`: The error handler for stderr output encoding.
        """

        self.source = source
        """The path to or description of the source data."""

        self.error_handler = error_handler
        """The character encoding error handler."""

        self.debug_flag = debug
        """Show debug (level=0) system messages?"""

        self.report_level = report_level
        """The level at or above which warning output will be sent
        to `self.stream`."""

        self.halt_level = halt_level
        """The level at or above which `SystemMessage` exceptions
        will be raised, halting execution."""

        if not isinstance(stream, ErrorOutput):
            stream = ErrorOutput(stream, encoding, error_handler)

        self.stream = stream
        """Where warning output is sent."""

        self.encoding = encoding or getattr(stream, 'encoding', 'ascii')
        """The output character encoding."""

        self.observers = []
        """List of bound methods or functions to call with each system_message
        created."""

        self.max_level = -1
        """The highest level system message generated so far."""

    def set_conditions(self, category, report_level, halt_level,
                       stream=None, debug=0):
        warnings.warn('docutils.utils.Reporter.set_conditions deprecated; '
                      'set attributes via configuration settings or directly',
                      DeprecationWarning, stacklevel=2)
        self.report_level = report_level
        self.halt_level = halt_level
        if not isinstance(stream, ErrorOutput):
            stream = ErrorOutput(stream, self.encoding, self.error_handler)
        self.stream = stream
        self.debug_flag = debug

    def attach_observer(self, observer):
        """
        The `observer` parameter is a function or bound method which takes one
        argument, a `nodes.system_message` instance.
        """
        self.observers.append(observer)

    def detach_observer(self, observer):
        self.observers.remove(observer)

    def notify_observers(self, message):
        for observer in self.observers:
            observer(message)

    def system_message(self, level, message, *children, **kwargs):
        """
        Return a system_message object.

        Raise an exception or generate a warning if appropriate.
        """
        # `message` can be a `string`, `unicode`, or `Exception` instance.
        if isinstance(message, Exception):
            message = SafeString(message)

        attributes = kwargs.copy()
        if 'base_node' in kwargs:
            source, line = get_source_line(kwargs['base_node'])
            del attributes['base_node']
            if source is not None:
                attributes.setdefault('source', source)
            if line is not None:
                attributes.setdefault('line', line)
                # assert source is not None, "node has line- but no source-argument"
        if not 'source' in attributes: # 'line' is absolute line number
            try: # look up (source, line-in-source)
                source, line = self.locator(attributes.get('line'))
                # print "locator lookup", kwargs.get('line'), "->", source, line
            except AttributeError:
                source, line = None, None
            if source is not None:
                attributes['source'] = source
            if line is not None:
                attributes['line'] = line
        # assert attributes['line'] is not None, (message, kwargs)
        # assert attributes['source'] is not None, (message, kwargs)
        attributes.setdefault('source', self.source)

        msg = nodes.system_message(message, level=level,
                                   type=self.levels[level],
                                   *children, **attributes)
        if self.stream and (level >= self.report_level
                            or self.debug_flag and level == self.DEBUG_LEVEL
                            or level >= self.halt_level):
            self.stream.write(msg.astext() + '\n')
        if level >= self.halt_level:
            raise SystemMessage(msg, level)
        if level > self.DEBUG_LEVEL or self.debug_flag:
            self.notify_observers(msg)
        self.max_level = max(level, self.max_level)
        return msg

    def debug(self, *args, **kwargs):
        """
        Level-0, "DEBUG": an internal reporting issue. Typically, there is no
        effect on the processing. Level-0 system messages are handled
        separately from the others.
        """
        if self.debug_flag:
            return self.system_message(self.DEBUG_LEVEL, *args, **kwargs)

    def info(self, *args, **kwargs):
        """
        Level-1, "INFO": a minor issue that can be ignored. Typically there is
        no effect on processing, and level-1 system messages are not reported.
        """
        return self.system_message(self.INFO_LEVEL, *args, **kwargs)

    def warning(self, *args, **kwargs):
        """
        Level-2, "WARNING": an issue that should be addressed. If ignored,
        there may be unpredictable problems with the output.
        """
        return self.system_message(self.WARNING_LEVEL, *args, **kwargs)

    def error(self, *args, **kwargs):
        """
        Level-3, "ERROR": an error that should be addressed. If ignored, the
        output will contain errors.
        """
        return self.system_message(self.ERROR_LEVEL, *args, **kwargs)

    def severe(self, *args, **kwargs):
        """
        Level-4, "SEVERE": a severe error that must be addressed. If ignored,
        the output will contain severe errors. Typically level-4 system
        messages are turned into exceptions which halt processing.
        """
        return self.system_message(self.SEVERE_LEVEL, *args, **kwargs)


class ExtensionOptionError(DataError): pass
class BadOptionError(ExtensionOptionError): pass
class BadOptionDataError(ExtensionOptionError): pass
class DuplicateOptionError(ExtensionOptionError): pass


def extract_extension_options(field_list, options_spec):
    """
    Return a dictionary mapping extension option names to converted values.

    :Parameters:
        - `field_list`: A flat field list without field arguments, where each
          field body consists of a single paragraph only.
        - `options_spec`: Dictionary mapping known option names to a
          conversion function such as `int` or `float`.

    :Exceptions:
        - `KeyError` for unknown option names.
        - `ValueError` for invalid option values (raised by the conversion
           function).
        - `TypeError` for invalid option value types (raised by conversion
           function).
        - `DuplicateOptionError` for duplicate options.
        - `BadOptionError` for invalid fields.
        - `BadOptionDataError` for invalid option data (missing name,
          missing data, bad quotes, etc.).
    """
    option_list = extract_options(field_list)
    option_dict = assemble_option_dict(option_list, options_spec)
    return option_dict

def extract_options(field_list):
    """
    Return a list of option (name, value) pairs from field names & bodies.

    :Parameter:
        `field_list`: A flat field list, where each field name is a single
        word and each field body consists of a single paragraph only.

    :Exceptions:
        - `BadOptionError` for invalid fields.
        - `BadOptionDataError` for invalid option data (missing name,
          missing data, bad quotes, etc.).
    """
    option_list = []
    for field in field_list:
        if len(field[0].astext().split()) != 1:
            raise BadOptionError(
                'extension option field name may not contain multiple words')
        name = str(field[0].astext().lower())
        body = field[1]
        if len(body) == 0:
            data = None
        elif len(body) > 1 or not isinstance(body[0], nodes.paragraph) \
              or len(body[0]) != 1 or not isinstance(body[0][0], nodes.Text):
            raise BadOptionDataError(
                  'extension option field body may contain\n'
                  'a single paragraph only (option "%s")' % name)
        else:
            data = body[0][0].astext()
        option_list.append((name, data))
    return option_list

def assemble_option_dict(option_list, options_spec):
    """
    Return a mapping of option names to values.

    :Parameters:
        - `option_list`: A list of (name, value) pairs (the output of
          `extract_options()`).
        - `options_spec`: Dictionary mapping known option names to a
          conversion function such as `int` or `float`.

    :Exceptions:
        - `KeyError` for unknown option names.
        - `DuplicateOptionError` for duplicate options.
        - `ValueError` for invalid option values (raised by conversion
           function).
        - `TypeError` for invalid option value types (raised by conversion
           function).
    """
    options = {}
    for name, value in option_list:
        convertor = options_spec[name]  # raises KeyError if unknown
        if convertor is None:
            raise KeyError(name)        # or if explicitly disabled
        if name in options:
            raise DuplicateOptionError('duplicate option "%s"' % name)
        try:
            options[name] = convertor(value)
        except (ValueError, TypeError), detail:
            raise detail.__class__('(option: "%s"; value: %r)\n%s'
                                   % (name, value, ' '.join(detail.args)))
    return options


class NameValueError(DataError): pass


def decode_path(path):
    """
    Ensure `path` is Unicode. Return `nodes.reprunicode` object.

    Decode file/path string in a failsave manner if not already done.
    """
    # see also http://article.gmane.org/gmane.text.docutils.user/2905
    if isinstance(path, unicode):
        return path
    try:
        path = path.decode(sys.getfilesystemencoding(), 'strict')
    except AttributeError: # default value None has no decode method
        return nodes.reprunicode(path)
    except UnicodeDecodeError:
        try:
            path = path.decode('utf-8', 'strict')
        except UnicodeDecodeError:
            path = path.decode('ascii', 'replace')
    return nodes.reprunicode(path)


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

def new_reporter(source_path, settings):
    """
    Return a new Reporter object.

    :Parameters:
        `source` : string
            The path to or description of the source text of the document.
        `settings` : optparse.Values object
            Runtime settings.
    """
    reporter = Reporter(
        source_path, settings.report_level, settings.halt_level,
        stream=settings.warning_stream, debug=settings.debug,
        encoding=settings.error_encoding,
        error_handler=settings.error_encoding_error_handler)
    return reporter

def new_document(source_path, settings=None):
    """
    Return a new empty document object.

    :Parameters:
        `source_path` : string
            The path to or description of the source text of the document.
        `settings` : optparse.Values object
            Runtime settings.  If none are provided, a default core set will
            be used.  If you will use the document object with any Docutils
            components, you must provide their default settings as well.  For
            example, if parsing, at least provide the parser settings,
            obtainable as follows::

                settings = docutils.frontend.OptionParser(
                    components=(docutils.parsers.rst.Parser,)
                    ).get_default_values()
    """
    from docutils import frontend
    if settings is None:
        settings = frontend.OptionParser().get_default_values()
    source_path = decode_path(source_path)
    reporter = new_reporter(source_path, settings)
    document = nodes.document(settings, reporter, source=source_path)
    document.note_source(source_path, -1)
    return document

def clean_rcs_keywords(paragraph, keyword_substitutions):
    if len(paragraph) == 1 and isinstance(paragraph[0], nodes.Text):
        textnode = paragraph[0]
        for pattern, substitution in keyword_substitutions:
            match = pattern.search(textnode)
            if match:
                paragraph[0] = nodes.Text(pattern.sub(substitution, textnode))
                return

def relative_path(source, target):
    """
    Build and return a path to `target`, relative to `source` (both files).

    If there is no common prefix, return the absolute path to `target`.
    """
    source_parts = os.path.abspath(source or 'dummy_file').split(os.sep)
    target_parts = os.path.abspath(target).split(os.sep)
    # Check first 2 parts because '/dir'.split('/') == ['', 'dir']:
    if source_parts[:2] != target_parts[:2]:
        # Nothing in common between paths.
        # Return absolute path, using '/' for URLs:
        return '/'.join(target_parts)
    source_parts.reverse()
    target_parts.reverse()
    while (source_parts and target_parts
           and source_parts[-1] == target_parts[-1]):
        # Remove path components in common:
        source_parts.pop()
        target_parts.pop()
    target_parts.reverse()
    parts = ['..'] * (len(source_parts) - 1) + target_parts
    return '/'.join(parts)

def get_stylesheet_reference(settings, relative_to=None):
    """
    Retrieve a stylesheet reference from the settings object.

    Deprecated. Use get_stylesheet_reference_list() instead to
    enable specification of multiple stylesheets as a comma-separated
    list.
    """
    if settings.stylesheet_path:
        assert not settings.stylesheet, (
            'stylesheet and stylesheet_path are mutually exclusive.')
        if relative_to == None:
            relative_to = settings._destination
        return relative_path(relative_to, settings.stylesheet_path)
    else:
        return settings.stylesheet

# Return 'stylesheet' or 'stylesheet_path' arguments as list.
#
# The original settings arguments are kept unchanged: you can test
# with e.g. ``if settings.stylesheet_path:``
#
# Differences to ``get_stylesheet_reference``:
# * return value is a list
# * no re-writing of the path (and therefore no optional argument)
#   (if required, use ``utils.relative_path(source, target)``
#   in the calling script)
def get_stylesheet_list(settings):
    """
    Retrieve list of stylesheet references from the settings object.
    """
    assert not (settings.stylesheet and settings.stylesheet_path), (
            'stylesheet and stylesheet_path are mutually exclusive.')
    if settings.stylesheet_path:
        sheets = settings.stylesheet_path.split(",")
    elif settings.stylesheet:
        sheets = settings.stylesheet.split(",")
    else:
        sheets = []
    # strip whitespace (frequently occuring in config files)
    return [sheet.strip(u' \t\n') for sheet in sheets]

def get_trim_footnote_ref_space(settings):
    """
    Return whether or not to trim footnote space.

    If trim_footnote_reference_space is not None, return it.

    If trim_footnote_reference_space is None, return False unless the
    footnote reference style is 'superscript'.
    """
    if settings.trim_footnote_reference_space is None:
        return hasattr(settings, 'footnote_references') and \
               settings.footnote_references == 'superscript'
    else:
        return settings.trim_footnote_reference_space

def get_source_line(node):
    """
    Return the "source" and "line" attributes from the `node` given or from
    its closest ancestor.
    """
    while node:
        if node.source or node.line:
            return node.source, node.line
        node = node.parent
    return None, None

def escape2null(text):
    """Return a string with escape-backslashes converted to nulls."""
    parts = []
    start = 0
    while 1:
        found = text.find('\\', start)
        if found == -1:
            parts.append(text[start:])
            return ''.join(parts)
        parts.append(text[start:found])
        parts.append('\x00' + text[found+1:found+2])
        start = found + 2               # skip character after escape

def unescape(text, restore_backslashes=0):
    """
    Return a string with nulls removed or restored to backslashes.
    Backslash-escaped spaces are also removed.
    """
    if restore_backslashes:
        return text.replace('\x00', '\\')
    else:
        for sep in ['\x00 ', '\x00\n', '\x00']:
            text = ''.join(text.split(sep))
        return text

east_asian_widths = {'W': 2,   # Wide
                     'F': 2,   # Full-width (wide)
                     'Na': 1,  # Narrow
                     'H': 1,   # Half-width (narrow)
                     'N': 1,   # Neutral (not East Asian, treated as narrow)
                     'A': 1}   # Ambiguous (s/b wide in East Asian context,
                               # narrow otherwise, but that doesn't work)
"""Mapping of result codes from `unicodedata.east_asian_widt()` to character
column widths."""

def column_width(text):
    """Return the column width of text.

    Correct ``len(text)`` for wide East Asian and combining Unicode chars.
    """
    if isinstance(text, str) and sys.version_info < (3,0):
        return len(text)
    combining_correction = sum([-1 for c in text
                                if unicodedata.combining(c)])
    try:
        width = sum([east_asian_widths[unicodedata.east_asian_width(c)]
                     for c in text])
    except AttributeError:  # east_asian_width() New in version 2.4.
        width = len(text)
    return width + combining_correction

def uniq(L):
     r = []
     for item in L:
         if not item in r:
             r.append(item)
     return r

# by Li Daobing http://code.activestate.com/recipes/190465/
# since Python 2.6 there is also itertools.combinations()
def unique_combinations(items, n):
    """Return r-length tuples, in sorted order, no repeated elements"""
    if n==0: yield []
    else:
        for i in xrange(len(items)-n+1):
            for cc in unique_combinations(items[i+1:],n-1):
                yield [items[i]]+cc

def normalize_language_tag(tag):
    """Return a list of normalized combinations for a `BCP 47` language tag.

    Example:

      >>> normalize_language_tag('de-AT-1901')
      ['de_at_1901', 'de_at', 'de_1901', 'de']
    """
    # normalize:
    tag = tag.lower().replace('-','_')
    # find all combinations of subtags
    taglist = []
    base_tag= tag.split('_')[:1]
    subtags = tag.split('_')[1:]
    # print base_tag, subtags
    for n in range(len(subtags), 0, -1):
        for tags in unique_combinations(subtags, n):
            # print tags
            taglist.append('_'.join(base_tag + tags))
    taglist += base_tag
    return taglist

class DependencyList:

    """
    List of dependencies, with file recording support.

    Note that the output file is not automatically closed.  You have
    to explicitly call the close() method.
    """

    def __init__(self, output_file=None, dependencies=[]):
        """
        Initialize the dependency list, automatically setting the
        output file to `output_file` (see `set_output()`) and adding
        all supplied dependencies.
        """
        self.set_output(output_file)
        for i in dependencies:
            self.add(i)

    def set_output(self, output_file):
        """
        Set the output file and clear the list of already added
        dependencies.

        `output_file` must be a string.  The specified file is
        immediately overwritten.

        If output_file is '-', the output will be written to stdout.
        If it is None, no file output is done when calling add().
        """
        self.list = []
        if output_file == '-':
            self.file = sys.stdout
        elif output_file:
            self.file = open(output_file, 'w')
        else:
            self.file = None

    def add(self, *filenames):
        """
        If the dependency `filename` has not already been added,
        append it to self.list and print it to self.file if self.file
        is not None.
        """
        for filename in filenames:
            if not filename in self.list:
                self.list.append(filename)
                if self.file is not None:
                    print >>self.file, filename

    def close(self):
        """
        Close the output file.
        """
        if self.file not in (sys.stdout, sys.stderr):
            self.file.close()
            self.file = None

    def __repr__(self):
        if self.file:
            output_file = self.file.name
        else:
            output_file = None
        return '%s(%r, %s)' % (self.__class__.__name__, output_file, self.list)

import xml.sax.handler
from xml.sax.handler import feature_namespaces
from StringIO import StringIO

class CopyTree(xml.sax.ContentHandler):
    """
    Needed class for the function  XmlStringToDocutilsNodes function. 
    Don't invoke this class directly.

    """
  
    def __init__(self, default_namespace = None, ns_dict = None):
          self.__characters = ''
          self.__current_node_list = []
          self.__tree = None
          self.__default_namespace = default_namespace
          self.__ns_prefix_dict = {
                  'http://www.w3.org/XML/1998/namespace': 'xml',
                  'http://www.w3.org/1998/Math/MathML': 'ml',
                  'http://www.w3.org/1999/xhtml': 'xhtml',
                  'http://www.w3.org/1999/XSL/Transform':'xsl',
                  }
          if ns_dict != None:
               self.__ns_prefix_dict.update(ns_dict)

          self.__ns_prefix_no_write_dict = {
                  'http://www.w3.org/XML/1998/namespace': 'xml',
                  }


    def characters (self, characters): 
        self.__characters += characters


    def startElementNS(self, name, qname, attrs):
        """
        Elements
        ========

        Get the information from the start of the element to write a docutis node Element.

        If the default_namespace is set and it is the first element, get the
        namespace from the first element. Write it as xmlns="http:/...." Do
        not write any other namespaces.  If the default_namespace is set but
        no namespace is found, raise an error.

        If there are no namespaces, just get the element's name, create an Element method, and
        set the tagname.


        If there is namespace:

          1. See if a convenient prefix exists. If so, write that prefix:
             <math xmlns="http://www.w3.org/XML/1998/namespace" => <ml:math
          
          2. If there is a namespace but no prefix,  use ns1 as the prefix
            <customElement xmlns="http://www.custom.org" => <ns1:customElement

          3. If the namespace needs to be decalred, then write it:
             <math xmlsn="http://www.w3.org/XML/1998/namespace" => <ml:math xmlsn:ml=http://www.w3.org/XML/1998/namespace"
             of
            <customElement xmlns="http://www.custom.org" => <ns1:customElement xmlns:ns1="http://www.custom.org"

          4. If the namespace does not need to be written, don't write it:
            (Don't think any examles exist for elements.)

        Attributes
        ==========

        The same strategy is followd for the attributes, with the exception of using the ns# for a default prefix.
        If no convenient prefix is found:

        1. If the namespace for the attribute matches the namespace for the element, use it (ns1)

        2. Otherwise, start with ns2, and use the next number (ns3) for the next prefix, and so on.

        """
        if len(self.__current_node_list) > 0:
            self.__write_text()
        ns = name[0] # for example, "http://www.w3.org/XML/1998/namespace"
        ns_prefix = self.__ns_prefix_dict.get(ns)
        el_name = name[1] # a string indicating the tag name, for example, "math"
        element = nodes.Element()
        element.tagname = el_name
        if len(self.__current_node_list) > 0:
            self.__current_node_list[-1].append(element)
            # if there is a namespace that does not match the root; and not an
            # implicit namespace, like XML, raise an error
            if ns and self.__default_namespace and ns != self.__default_namespace and not(self.__ns_prefix_no_write_dict.get(ns)):
                raise SystemError('default namespace "%s"  does not match root namespace "%s"' % (ns, self.__default_namespace)) 
        else:
            self.__tree = element
            if self.__default_namespace:
                if not ns:
                    raise SystemError('no default namespace found, yet default_namespace passed to function') 
                element['xmlns'] = ns
                self.__default_namespace = ns
        self.__current_node_list.append(element)
        if not self.__default_namespace:
            if ns and ns_prefix:
                element.tagname = '%s:%s' % (ns_prefix, el_name)
            elif ns:
                element.tagname = 'ns1:%s' % el_name

            if ns and self.__ns_prefix_no_write_dict.get(ns): # don't need to write certain namespaces, like xml
                pass
            elif ns and ns_prefix:
                element['xmlns:%s' % ns_prefix] = ns
            elif ns:
                element['xmlns:ns1'] = ns
        elif self.__ns_prefix_no_write_dict.get(ns):
            # unlikey to actually occurr, but just in case
            element.tagname = '%s:%s' % (ns_prefix, el_name)


        the_keys = attrs.keys()
        counter = 1
        for the_key in the_keys:
            counter +=1
            ns_att = the_key[0]
            att_name = the_key[1]
            value = attrs[the_key]
            ns_prefix = self.__ns_prefix_dict.get(ns_att)
            if not self.__default_namespace:# all cases for non-default space, including no namespace and xml namespace
                if ns_att and ns_att != ns:
                    if not(self.__ns_prefix_no_write_dict.get(ns_att)):
                        att = 'xmlns:ns%s' % counter
                        the_value = ns_att
                        element[att] = the_value
                if ns_att and ns_prefix:
                    att = '%s:%s' % (ns_prefix, att_name)
                    element[att] = value
                elif ns_att and ns_att == ns:
                    att = 'ns1:%s' % att_name
                    element[att] = value
                elif ns_att:
                    att = 'ns%s:%s' % (counter, att_name)
                    element[att] = value
                else:
                    element[att_name] = value
            else: # default namespace only write prefixes such as xml; otherwise just write attribute
                if ns_att and self.__ns_prefix_no_write_dict.get(ns_att):
                    att_name = '%s:%s' % (ns_prefix, att_name)
                    element[att_name] = value
                else:
                    element[att_name] = value

    def __write_text(self):
        text = self.__characters
        self.__current_node_list[-1].append(nodes.Text(text))
        self.__characters = ''

    def endElementNS(self, name, qname):
        self.__write_text()
        self.__current_node_list.pop()

    def get_tree(self):
        return self.__tree

    def endDocument(self):
        pass

def XmlStringToDocutilsNodes(xml_string, encoding='utf8', default_namespace = None, ns_dict = None):
    """
    Converts an XML String into a docutils node tree, and returns that tree.

    xml_string can either be a unicode object or a string (for Python < 3); or
    a string or a byte string (for pyton >=3.0).

    The encoding is the encoding for the xm_string.

    The default_namespace should be set to some boolean value, such as True or
    False. If set, default_namespace makes easier-to read XML by writing the
    namespace in only the first element:

    <ml:math xmlns:ml="http://www.w3.org/1998/Math/MathML>
      <ml:style xmlns:ml="http://www.w3.org/1998/Math/MathMl">
      </ml:style>
     </ml:math

     Becomes:

    <math xmlns="http://www.w3.org/1998/Math/MathML>
      <style >
      </style>
     </math

     An error is raised if no namespace is found for the first element, or a namespace is found
     for subequent elements that does not match.

    The ns_dict is a dictionary of namespaces mapped to a prefix. For example:

     {"http://www.tei-c.org/ns/1.0":'tei'}

     If any element is found with the namespace http://www.tei-c.org/ns/1.0,
     then the prefix "tei" is used.  Note that this dictionary only makes the
     XML look more readable, and is not needed to create valid XML with the
     correct namespaces. For example, if the parser finds an element with a
     namespace "http://www.tei-c.org/ns/1.0", and no dict is passed to this 
     function, the parser assigns its own prefix:

     <ns1:paragraph xmlns:ns1="http://www.tei-c.org/ns/1.0"


    """

    if sys.version_info < (3,):
        if type(xml_string) == type(unicode('x')):
            xml_string = xml_string.encode('utf8')
        elif type(xml_string) == type('x'):
            xml_string = xml_string.decode(encoding)
            xml_string = xml_string.encode('utf8')
    else:
        if type(xml_string) == type(b'x'):
            xml_string = xml_string.decode(encoding)
    read_obj = StringIO(xml_string)
    the_handle=CopyTree(ns_dict = ns_dict, default_namespace = default_namespace)
    parser = xml.sax.make_parser()
    parser.setFeature(feature_namespaces, 1)
    parser.setContentHandler(the_handle)
    parser.setFeature("http://xml.org/sax/features/external-general-entities", True)
    parser.parse(read_obj)             
    read_obj.close()
    docutils_tree = the_handle.get_tree()
    return docutils_tree

import xml.dom.minidom
import xml.sax.saxutils

    """
    takes a dom element as current_element

    """
 
def start_tag(local_name):
    sys.stdout.write('<%s>' % local_name)

def end_tag(local_name):
    sys.stdout.write('</%s>' % local_name)

dom = xml.dom.minidom.parse('test.xml')
out_doc = xml.dom.minidom.Document()
def copy_tree(current_element):
    elements = current_element.childNodes
    for element in elements:
        if element.nodeType == xml.dom.Node.ELEMENT_NODE:
            element_name = element.localName
        if element.attributes!= None:
            for attr in element.attributes.values():
                ns = attr.namespaceURI
                local_name = attr.localName
                name = attr.name
                value = attr.value
                prefix = attr.prefix
                new_att = out_doc.createAttribute(name )
            start_tag(element_name)
            copy_tree(element)
            end_tag(element_name)
        elif element.nodeType == xml.dom.Node.TEXT_NODE:
            parent = element.parentNode
            if parent.localName == 'math':
                sys.stdout.write(element.data)
            else:
                sys.stdout.write(element.data)
                


copy_tree(dom)
