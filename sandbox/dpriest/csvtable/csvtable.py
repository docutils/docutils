# Author: David Priest & David Goodger
# Contact: priest@sfu.ca
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Directive for CSV (comma-separated values) Tables.


"""

import csv
import os.path
import operator
from docutils import nodes, statemachine, utils
from docutils.transforms import references
from docutils.parsers.rst import directives

try:
    import urllib2
except ImportError:
    urllib2 = None


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


def csvtable(name, arguments, options, content, lineno,
             content_offset, block_text, state, state_machine):


    if arguments:
        title_text = arguments[0]
        text_nodes, messages = state.inline_text(title_text, lineno)
        title = nodes.title(title_text, '', *text_nodes)
    else:
        title = None

    # point CSV parser to table content
    csv.register_dialect('docutils', DocutilsDialect(options))
    csv.list_dialects()


    if content:
        if options.has_key('file') or options.has_key('url'):
            error = state_machine.reporter.error(
                  '"%s" directive may not both specify an external file and '
                  'have content.' % name,
                  nodes.literal_block(block_text, block_text), line=lineno)
            return [error]
        else:
            # content is supplied inline
            source, offset = content.info(0)
            csvreader = csv.reader(content, dialect='docutils')
    elif options.has_key('file'):
        if options.has_key('url'):
            error = state_machine.reporter.error(
                  'The "file" and "url" options may not be simultaneously '
                  'specified for the "%s" directive.' % name,
                  nodes.literal_block(block_text, block_text), line=lineno)
            return [error]
        source_dir = os.path.dirname(os.path.abspath(state.document.current_source))
        path = os.path.normpath(os.path.join(source_dir, options['file']))
        path = utils.relative_path(None, path)
        source = path
        try:
            try:
                # content is supplied as external file
                raw_file = open(path, 'rb')
                csv_data = raw_file.read().splitlines()
                csvreader = csv.reader(csv_data, dialect='docutils')
            except IOError, error:
                severe = state_machine.reporter.severe(
                      'Problems with "%s" directive path:\n%s.' % (name, error),
                      nodes.literal_block(block_text, block_text), line=lineno)
                return [severe]
        finally:
            raw_file.close()
    elif options.has_key('url'):
        if not urllib2:
            severe = state_machine.reporter.severe(
                  'Problems with the "%s" directive and its "url" option: '
                  'unable to access the required functionality (from the '
                  '"urllib2" module).' % name,
                  nodes.literal_block(block_text, block_text), line=lineno)
            return [severe]
        try:
            # content is supplied as URL
            csv_data = urllib2.urlopen(options['url']).read().splitlines()
            csvreader = csv.reader(csv_data, dialect='docutils')
            source = options['url']
        except (urllib2.URLError, IOError, OSError), error:
            severe = state_machine.reporter.severe(
                  'Problems with "%s" directive URL "%s":\n%s.'
                  % (name, options['url'], error),
                  nodes.literal_block(block_text, block_text), line=lineno)
            return [severe]
    else:
        error = state_machine.reporter.warning(
            'The "%s" directive requires content; none supplied.' % (name),
            nodes.literal_block(block_text, block_text), line=lineno)
        return [error]

    # populate header from header-option.
    tablehead = []
    # optionable column headers
    if options.has_key('headers'):
        for i in csv.reader(options['headers'].split('\n'), skipinitialspace=True):
            rowdata = []
            for j in i:
                cell = statemachine.StringList((j,), source=source)
                celldata = (0,0,0,cell)
                rowdata.append(celldata)
            tablehead.append(rowdata)
    # if not overridden, use first row as headers
    if (not options.has_key('header-rows')) and (not options.has_key('headers')):
        options['header-rows'] = 1
    else:
        options.setdefault('header-rows', '0')
        options['header-rows'] = int(options['header-rows'])
    # populate header from header-rows option.
    for i in range(options['header-rows']):
        row = csvreader.next()
        rowdata = []
        for j in row:
            cell = statemachine.StringList(j.splitlines(), source=source)
            celldata = (0,0,0,cell)
            rowdata.append(celldata)
        tablehead.append(rowdata)
    # populate tbody
    tablebody = []
    for row in csvreader:
        rowdata = []
        for i in row:
            j = statemachine.StringList(i.splitlines(), source=source)
            celldata = (0,0,0,j)
            rowdata.append(celldata)
        tablebody.append(rowdata)
    if tablebody == []:
        error = state_machine.reporter.error(
              '"%s" directive requires table body content.' % name,
              nodes.literal_block(block_text, block_text), line=lineno)
        return [error]
    # calculate column widths
    maxcols = max(map(len, tablehead + tablebody))
    if options.has_key('widths'):
        if ',' in options['widths']:
            tablecolwidths = options['widths'].replace(' ','').split(',')
        else:
            tablecolwidths = filter(options['widths'].split(' '))
        if len(tablecolwidths) != maxcols:
            error = state_machine.reporter.error(
              '"%s" widths does not match number of columns in table.' % name,
              nodes.literal_block(block_text, block_text), line=lineno)
        tablecolwidths = map(int, tablecolwidths)
    else:
        tablecolwidths = [100/maxcols]*maxcols

    # convert raw list to DocUtils node tree
    table = (tablecolwidths, tablehead, tablebody)
    tableline = content_offset
    table_node = state.build_table(table, content_offset)

    if options.has_key('class'):
        table_node.set_class(options['class'])

    if title:
        table_node.insert(0, title)

    return [table_node]

def single_char(argument):
    if argument == 'tab' or argument == '\\t':
        argument = '\t'
    elif argument == 'space':
        argument = ' '
    elif argument == 'lf' or argument == '\\n':
        argument = '\n'
    elif argument == 'cr' or argument == '\\r':
        argument = '\r'
    elif argument == 'ff' or argument == '\\f':
        argument = '\f'
    elif argument == 'vt' or argument == '\\v':
        argument = '\v'
    elif argument[0:3] == 'hex':
        argument = chr(int(argument[4:6],16))
    if len(argument) > 1:
        raise ValueError('string value; must be a single character')
    return argument

def line_end(argument):
    return directives.choice(argument, ('cr', 'lf', 'crlf'))

def nonzero_int(argument):
    value = int(argument)
    if value < 1:
        raise ValueError('negative or zero value; must be positive')
    return value

def nonzero_int_list(argument):
    if argument.find(','):
        for i in argument.split(','):
            nonzero_int(i)
    else:
        # how utterly bizarre: this doesn't work! .split isn't splitting at all.
        for i in argument.split():
            nonzero_int(i)
    return argument

csvtable.arguments = (0, 1, 1)
csvtable.options = {'header-rows': directives.nonnegative_int,
                    'headers': directives.unchanged,
                    'widths': nonzero_int_list,
                    'file' : directives.path,
                    'class' : directives.class_option,
                    'delim' : single_char,              # field delim char
                    'quote' : single_char,              # text field quote/unquote char
                    'escape' : single_char,             # char used to escape delim & quote as-needed
                   }
csvtable.content = 1

directives.register_directive('csvtable', csvtable)
