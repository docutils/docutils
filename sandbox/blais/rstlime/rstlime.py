#!/usr/bin/env python
"""
Extract data entries like this from a set of reStructuredText documents like
this::

  [book]
    :title: Probability Theory
    :subtitle: The Logic of Science
    :authors: E.T. Jaynes, G. Larry Bretthorst
    :isbn: 978-0521592710

and store them in an existing database and/or infer a new database model. This
allows you to very easily embed data within a text file.

(The ideas for this project originate from Nabu (http://furius.ca/nabu/). It is
an attempt at providing the basic functionality without all the complications,
setup and customization that Nabu requires.)
"""
__author__ = 'Martin Blais <blais@furius.ca>'

## FIXME: TODO:
## How do we deal with subtypes?  e.g. :p/cell:
## What do we do when we have multiple values for the same key?
## Should we join the lines for values?
## Add document ids.


# stdlib imports
import sys, os, re, getpass
from types import ClassType
from StringIO import StringIO
from collections import defaultdict
from operator import attrgetter

# psyco imports
import psycopg2 as dbapi

# docutils imports
from docutils import core, nodes, io


#-------------------------------------------------------------------------------
# Parsing and values extraction code

# Note: this is generic, perhaps should be part of docutils.nodes.
def find_first(nodetype, node, document):
    """ Find the first node under 'node' that is of the same type as 'nodetype'."""

    assert isinstance(nodetype, ClassType)
    found = []

    class FindFirst(nodes.SparseNodeVisitor):
        def visit(self, node):
            found.append(node)
            return True # stop

    setattr(FindFirst, 'visit_%s' % nodetype.__name__,
            FindFirst.visit)

    vis = FindFirst(document)
    node.walk(vis)

    return found[0] if found else None

def get_file_entries(fn):
    """ Parse a file and extract entries from it. """
    text = open(fn).read()
    document = core.publish_doctree(
        open(fn),
        source_class=io.FileInput,
        ## source_path=fn,
        reader_name='standalone',
        parser_name='restructuredtext',
        settings_overrides={'report_level': 'error'},
        )

    # Extract the unique document id.
    docid = None
    docinfo = find_first(nodes.docinfo, document, document)
    if docinfo:
        fields = extract_fields(docinfo, document)
        docid = dict(fields).get('Id', None)
    if docid is None:
        docid = basename(fn)

    # Obtain all the data from the document.
    v = FindData(docid, document)
    document.walk(v)
    return docid, v.entries


class Entry(object):
    """ A data entry read from a file. """

    def __init__(self, source, table, values):
        self.source = source
        self.table = table
        self.values = values

    def __str__(self):
        s = StringIO()
        s.write('[%s]  (%s)\n' % (self.table, self.source))
        for x in self.values:
            s.write('  :%s: %s\n' % x)
        return s.getvalue()

class FindData(nodes.SparseNodeVisitor):
    """ A visitor that finds all the definition_list_item which match our
    desired tagging for format."""

    # Regexp for the definition tag.
    tagre = re.compile('\[([a-zA-Z0-9_]+)\]\s*$')

    def __init__(self, docid, *args):
        nodes.SparseNodeVisitor.__init__(self, *args)
        self.docid = docid
        self.entries = []

    def visit_term(self, node):
        if len(node.children) != 1:
            return
        mo = self.tagre.match(node.astext())
        if not mo:
            return

        table = str2table(mo.group(1))
        dlitem = node.parent
        if len(dlitem.children) != 2:
            return

        defn = dlitem.children[1]
        if not isinstance(defn, nodes.definition) or len(defn.children) != 1:
            return
        flist = defn.children[0]
        if not isinstance(flist, nodes.field_list):
            return

        fields = extract_fields(flist, self.document)
        e = Entry(self.docid, table, fields)
        self.entries.append(e)

        raise nodes.SkipNode()

def str2table(s):
    "Convert a string to a valid table name."
    return s.lower().replace(' ', '_')


def extract_fields(node, document):
    "Return a list of (key, value) pairs from all underlying field_list's."
    v = ExtractFields(document)
    node.walk(v)
    return list(v)

class ExtractFields(nodes.SparseNodeVisitor, list):
    """ A visitor for a field_list that extracts all the name/value pairs. """

    def visit_field_name(self, node):
        self.key = node.astext()

    def visit_field_body(self, node):
        self.append( (self.key, node.astext()) )
        self.key = None



#-------------------------------------------------------------------------------
# Table definition inference code.

# Note: this is generic utils code.
def seq2dict(seq, classify_fun):
    """Given a sequence of objects and a function to classify them, return a dict of
    (key, sublist of objects) whereby 'key' is computed by calling
    'classify_fun' on objects."""
    assert isinstance(seq, (list, tuple)), seq
    r = defaultdict(list)
    for e in seq:
        try:
            r[classify_fun(e)].append(e)
        except Exception:
            pass
    return r

def infer_tables(entries):
    """ Given a list of entries, infer some database models from it. """

    table_entries = seq2dict(entries, attrgetter('table'))
    return dict((table, infer_table(entries))
                for table, entries in table_entries.iteritems())

intre = re.compile('[0-9]+$')
floatre = re.compile('[0-9\.]+$')

def infer_table(entries):
    """ Given a list of entries from the same table, infer a table description.
    This returns a dict of 'table-name' to a sorted list of (column-name, type)
    pairs."""

    coldata = defaultdict(list)
    sortorder = defaultdict(int)
    for e in entries:
        for i, (key, value) in enumerate(e.values):
            coldata[key].append(value)
            sortorder[key] += i

    coldefs = {}
    for colname, values in coldata.iteritems():
        if all(intre.match(x) for x in values):
            ctype = int
        elif all(floatre.match(x) for x in values):
            ctype = float
        else:
            ctype = unicode
        coldefs[colname] = ctype

    return sorted(coldefs.items(), key=lambda x: sortorder[x[0]])

sqltypes = {int: 'INTEGER',
            float: 'FLOAT',
            unicode: 'TEXT'}

def sqlcol(colname):
    "Sanitize columns names for SQL."
    return colname.strip().lower().replace(' ', '_').replace('-', '_')

def table2sql(table, tabledef):
    """Generate SQL table definition code given the table name and columns
    definition."""
    lines = ['CREATE TABLE %s (' % table]
    for colname, ctype in tabledef:
        lines.append('  %s %s,' % (sqlcol(colname), sqltypes[ctype]))
    lines[-1] = lines[-1][:-1]

    lines.append(');')
    lines.append('')
    return os.linesep.join(lines)


#-------------------------------------------------------------------------------
# Database introspection.

def db_get_tables(conn):
    "List all the tables of the database."
    curs = conn.cursor()
    curs.execute("""
      SELECT table_name FROM information_schema.tables
        WHERE table_schema = 'public';
        """)
    return [x[0] for x in curs]

def db_get_table_columns(conn, table):
    "List all the columns of a table in the database."
    curs = conn.cursor()
    curs.execute("""
      SELECT column_name, data_type FROM information_schema.columns
        WHERE table_schema = 'public' AND
              table_name = %s
        """, (table,))
    return list(curs)

def db_get_model(conn):
    "Obtain the definition of database tables and columns."
    dbmodel = {}
    for table in db_get_tables(conn):
        dbmodel[table] = db_get_table_columns(conn, table)
    return dbmodel



#-------------------------------------------------------------------------------
# Filling up the database.

def store_entries(entries_list, dbmodel, conn):
    """ Given a list of entries to be stored, try to store as much data as
    possible in the given database model."""

    curs = conn.cursor()

    dbmodel = dict((k, dict(v)) for (k,v) in dbmodel.iteritems())
    for e in entries_list:
        try:
            cols = dbmodel[e.table]
        except KeyError:
            pass # Table for available.

        scols = []
        svalues = []
        colset = set()
        for cname, cvalue in e.values:
            if cname in colset:
                continue # Cannot store two of the same key.
            else:
                colset.add(cname)

            dtype = cols[sqlcol(cname)]
            if dtype == 'text':
                value = unicode(cvalue)
            else:
                raise NotImplementedError("Unsupported type.")

            scols.append(sqlcol(cname))
            svalues.append(value)

        if svalues:
            curs.execute("""
              INSERT INTO %s (%s) VALUES (%s)
              """ % (e.table,
                     ','.join(scols),
                     ','.join(['%s'] * len(svalues))),
                         svalues)

    conn.commit()


#-------------------------------------------------------------------------------
# Main program.

def parse_dburi(dburi):
    """ Parse the database connection URI. """

    user, passwd, host, dbname = [None] * 4
    mo = re.match('(db|postgres|postgresql)://(?:([^:@]+)'
                  '(?::([^:@]+))?@)?([a-z0-9]+)/([a-z0-9]+)/?$', dburi)
    if mo:
        user, passwd, host, dbname = mo.group(2, 3, 4, 5)
    elif re.match('[a-z]+', dburi):
        dbname = dburi
    else:
        parser.error("Invalid database connection string.")

    if user is None:
        user = getpass.getuser()
    if passwd is None:
        passwd = getpass.getpass()
    if host is None:
        host = 'localhost'
    r = (user, passwd, host, dbname)
    assert None not in r, r
    return r


def main():
    import optparse
    parser = optparse.OptionParser(__doc__.strip())

    parser.add_option('-c', '--infer-definition', action='store', metavar="FILE",
                      help="Infer the definition of tables from the data and "
                      "store in the given filename.")

    parser.add_option('-s', '--store', action='store', metavar="CONNSTR",
                      default="postgres://localhost/test",
                      help="If present, store the contents to a given database. "
                      "You must provide a database connection URI.")

    opts, args = parser.parse_args()

    if not args:
        parser.error("You must specific a list of filenames to process.")

    # Disable the conversion of system messages into text.
    nodes.system_message.astext = lambda *args: u''

    # Process each input file.
    entries_by_document = {}
    entries_list = []
    for fn in args:
        docid, entries = get_file_entries(fn)
        entries_by_document[docid] = entries
        entries_list.extend(entries)

    # Infer the definition of the database into CREATE TABLE statements.
    if opts.infer_definition:
        f = open(opts.infer_definition, 'w')
        defs = infer_tables(entries)
        for table, tabledef in defs.iteritems():
            f.write(table2sql(table, tabledef))
            f.write('\n')
## FIXME: we need to try to create the tables.
## FIXME: add an option to drop and recreate the tables.
            
    # Open a connection to the database.
    if opts.store:
        user, passwd, host, dbname = parse_dburi(opts.store)
        conn = dbapi.connect(host=host,
                             user=user,
                             password=passwd,
                             database=dbname)

        # Open the database and inspect the model.
        dbmodel = db_get_model(conn)
        store_entries(entries_list, dbmodel, conn)






if __name__ == '__main__':
    ## inspect_db()
    main()




