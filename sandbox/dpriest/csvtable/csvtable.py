# Author: David Priest & David Goodger
# Contact: priest@sfu.ca
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Directive for CSV (comma-separated values) Tables.

Yet to do:

* Add exception handling for malformed CSV.
* Add a bunch of unit tests to the test suite, focusing on malformed CSV.
"""

import csv
import os.path
import operator
from docutils import nodes, statemachine, utils
from docutils.utils import SystemMessagePropagation
from docutils.transforms import references
from docutils.parsers.rst import directives

try:
    import urllib2
except ImportError:
    urllib2 = None

try:
    True
except NameError:                       # Python 2.2 & 2.1 compatibility
    True = not 0
    False = not 1


class DocutilsDialect(csv.Dialect):

    delimiter = ','
    quotechar = '"'
    doublequote = True
    skipinitialspace = True
    lineterminator = '\n'
    quoting = csv.QUOTE_MINIMAL

    def __init__(self, options):
        if options.has_key('delim'):
            self.delimiter = str(options['delim'])
        if options.has_key('quote'):
            self.quotechar = str(options['quote'])
        if options.has_key('escape'):
            self.doublequote = False
            self.escapechar = str(options['escape'])
        csv.Dialect.__init__(self)


class HeaderDialect(csv.Dialect):

    """CSV dialect to use for the "header" option data."""

    delimiter = ','
    quotechar = '"'
    escapechar = '\\'
    doublequote = False
    skipinitialspace = True
    lineterminator = '\n'
    quoting = csv.QUOTE_MINIMAL


def csv_table(name, arguments, options, content, lineno,
             content_offset, block_text, state, state_machine):
    
    title, messages = make_title(arguments, state, lineno)
    try:
        csv_data, source = get_csv_data(
            name, options, content, lineno, block_text, state, state_machine)
        table_head, max_header_cols = process_header_option(
            options, state_machine, lineno)
        rows, max_cols = parse_csv_data_into_rows(
            csv_data, DocutilsDialect(options), source, options)
        max_cols = max(max_cols, max_header_cols)
        header_rows = options.get('header-rows', 0) # default 0
        table_head.extend(rows[:header_rows])
        table_body = rows[header_rows:]
        if not table_body:
            error = state_machine.reporter.error(
                  '"%s" directive requires table body content.' % name,
                  nodes.literal_block(block_text, block_text), line=lineno)
            return [error]
        col_widths = get_col_widths(options, max_cols, lineno, state_machine)
        extend_short_rows_with_empty_cells(max_cols, (table_head, table_body))
    except SystemMessagePropagation, detail:
        return [detail.args[0]]
    table = (col_widths, table_head, table_body)
    table_node = state.build_table(table, content_offset)
    if options.has_key('class'):
        table_node.set_class(options['class'])
    if title:
        table_node.insert(0, title)
    return [table_node] + messages

def make_title(arguments, state, lineno):
    if arguments:
        title_text = arguments[0]
        text_nodes, messages = state.inline_text(title_text, lineno)
        title = nodes.title(title_text, '', *text_nodes)
    else:
        title = None
        messages = []
    return title, messages

def get_csv_data(name, options, content, lineno, block_text,
                 state, state_machine):
    if content:                         # CSV data is from directive content
        if options.has_key('file') or options.has_key('url'):
            error = state_machine.reporter.error(
                  '"%s" directive may not both specify an external file and '
                  'have content.' % name,
                  nodes.literal_block(block_text, block_text), line=lineno)
            raise SystemMessagePropagation(error)
        source = content.source(0)
        csv_data = content
    elif options.has_key('file'):       # CSV data is from an external file
        if options.has_key('url'):
            error = state_machine.reporter.error(
                  'The "file" and "url" options may not be simultaneously '
                  'specified for the "%s" directive.' % name,
                  nodes.literal_block(block_text, block_text), line=lineno)
            raise SystemMessagePropagation(error)
        source_dir = os.path.dirname(
            os.path.abspath(state.document.current_source))
        source = os.path.normpath(os.path.join(source_dir, options['file']))
        source = utils.relative_path(None, source)
        try:
            try:
                csv_file = open(source, 'rb')
                csv_data = csv_file.read().splitlines()
            except IOError, error:
                severe = state_machine.reporter.severe(
                      'Problems with "%s" directive path:\n%s.' % (name, error),
                      nodes.literal_block(block_text, block_text), line=lineno)
                raise SystemMessagePropagation(severe)
        finally:
            csv_file.close()
    elif options.has_key('url'):        # CSV data is from a URL
        if not urllib2:
            severe = state_machine.reporter.severe(
                  'Problems with the "%s" directive and its "url" option: '
                  'unable to access the required functionality (from the '
                  '"urllib2" module).' % name,
                  nodes.literal_block(block_text, block_text), line=lineno)
            raise SystemMessagePropagation(severe)
        source = options['url']
        try:
            csv_data = urllib2.urlopen(source).read().splitlines()
        except (urllib2.URLError, IOError, OSError), error:
            severe = state_machine.reporter.severe(
                  'Problems with "%s" directive URL "%s":\n%s.'
                  % (name, options['url'], error),
                  nodes.literal_block(block_text, block_text), line=lineno)
            raise SystemMessagePropagation(severe)
    else:
        error = state_machine.reporter.warning(
            'The "%s" directive requires content; none supplied.' % (name),
            nodes.literal_block(block_text, block_text), line=lineno)
        raise SystemMessagePropagation(error)
    return csv_data, source

def process_header_option(options, state_machine, lineno):
    source = state_machine.get_source(lineno - 1)
    table_head = []
    max_header_cols = 0
    if options.has_key('header'):       # separate table header in option
        rows, max_header_cols = parse_csv_data_into_rows(
            options['header'].split('\n'), HeaderDialect(), source, options)
        table_head.extend(rows)
    return table_head, max_header_cols

def parse_csv_data_into_rows(csv_data, dialect, source, options):
    csv_reader = csv.reader(csv_data, dialect=dialect)
    rows = []
    max_cols = 0
    for row in csv_reader:
        row_data = []
        for cell in row:
            cell_data = (0, 0, 0, statemachine.StringList(cell.splitlines(),
                                                          source=source))
            row_data.append(cell_data)
        rows.append(row_data)
        max_cols = max(max_cols, len(row))
    return rows, max_cols

def get_col_widths(options, max_cols, lineno, state_machine):
    if options.has_key('widths'):
        col_widths = options['widths']
        if len(col_widths) != max_cols:
            error = state_machine.reporter.error(
              '"%s" widths does not match number of columns in table (%s).'
              % (name, max_cols),
              nodes.literal_block(block_text, block_text), line=lineno)
            raise SystemMessagePropagation(error)
    else:
        col_widths = [100 / max_cols] * max_cols
    return col_widths

def extend_short_rows_with_empty_cells(columns, parts):
    for part in parts:
        for row in part:
            if len(row) < columns:
                row.extend([(0, 0, 0, [])] * (columns - len(row)))

def single_char_or_unicode(argument):
    if argument == 'tab' or argument == '\\t':
        char = '\t'
    elif argument == 'space':
        char = ' '
    else:
        char = directives.unicode_code(argument)
    if len(char) > 1:
        raise ValueError('must be a single character or Unicode code')
    return argument

def single_char_or_whitespace_or_unicode(argument):
    if argument == 'tab':
        char = '\t'
    elif argument == 'space':
        char = ' '
    else:
        char = directives.unicode_code(argument)
    if len(char) > 1:
        raise ValueError('must be a single character or Unicode code')
    return argument

def positive_int(argument):
    value = int(argument)
    if value < 1:
        raise ValueError('negative or zero value; must be positive')
    return value

def positive_int_list(argument):
    if ',' in argument:
        entries = argument.split(',')
    else:
        entries = argument.split()
    return [positive_int(entry) for entry in entries]

csv_table.arguments = (0, 1, 1)
csv_table.options = {'header-rows': directives.nonnegative_int,
                     'header': directives.unchanged,
                     'widths': positive_int_list,
                     'file': directives.path,
                     'url': directives.path,
                     'class': directives.class_option,
                     # field delimiter char
                     'delim': single_char_or_whitespace_or_unicode,
                     # text field quote/unquote char:
                     'quote': single_char_or_unicode,
                     # char used to escape delim & quote as-needed:
                     'escape': single_char_or_unicode,}
csv_table.content = 1

directives.register_directive('csvtable', csv_table)
