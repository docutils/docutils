# -*- coding: utf-8 -*-
"""
    sphinx.web.database
    ~~~~~~~~~~~~~~~~~~~

    The models of the MVC thing we don't really use. Currently
    just used for comments.

    The database connections are thread local. To set the connection
    for a thread use the `set_connection` function provided. The
    `connect` method automatically sets up new tables and returns a
    usable connection which is also set as the connection for the
    thread that called that function.

    :copyright: 2007 by Georg Brandl, Armin Ronacher.
    :license: Python license.
"""
import sqlite3
import time
from threading import local
from datetime import datetime
from .markdown import markdown


_thread_local = local()


def adapt_datetime(val):
    return time.mktime(val.timetuple())

def convert_datetime(val):
    return datetime.fromtimestamp(float(val))

sqlite3.register_adapter(datetime, adapt_datetime)
sqlite3.register_converter('datetime', convert_datetime)


def connect(path):
    """Connect and create tables if required. Also assigns
    the connection for the current thread."""
    con = sqlite3.connect(path, detect_types=sqlite3.PARSE_DECLTYPES)
    con.isolation_level = None

    # create tables that do not exist.
    for table in tables:
        try:
            con.execute('select * from %s;' % table)
        except sqlite3.OperationalError:
            con.execute(tables[table])

    set_connection(con)
    return con


def get_cursor():
    """Return a new cursor."""
    return _thread_local.connection.cursor()


def set_connection(con):
    """Call this after thread creation to make this connection
    the connection for this thread."""
    _thread_local.connection = con


#: tables that we use
tables = {
    'comments': '''
        create table comments (
            comment_id integer primary key,
            associated_page varchar(200),
            title varchar(120),
            author varchar(200),
            author_mail varchar(250),
            comment_body text,
            pub_date datetime
        );'''
}


class Comment(object):
    """
    Represents one comment.
    """

    def __init__(self, associated_page, title, author, author_mail,
                 comment_body, pub_date=None):
        self.comment_id = None
        self.associated_page = associated_page
        self.title = title
        if pub_date is None:
            pub_date = datetime.utcnow()
        print type(pub_date), repr(pub_date)
        self.pub_date = pub_date
        self.author = author
        self.author_mail = author_mail
        self.comment_body = comment_body

    @property
    def url(self):
        return '%s#comment-%s' % (self.associated_page[:-4], self.comment_id)

    @property
    def parsed_comment_body(self):
        return markdown(self.comment_body, safe_mode=True)

    def save(self):
        """
        Save the comment and use the cursor provided.
        """
        cur = get_cursor()
        args = (self.associated_page, self.title, self.author,
                self.author_mail, self.comment_body, self.pub_date)
        if self.comment_id is None:
            cur.execute('''insert into comments (associated_page, title,
                                                 author, author_mail,
                                                 comment_body, pub_date)
                                  values (?, ?, ?, ?, ?, ?)''', args)
            self.comment_id = cur.lastrowid
        else:
            args += (self.comment_id,)
            cur.execute('''update comments set associated_page=?,
                                  title=?, author=?,
                                  author_mail=?, comment_body=?,
                                  pub_date=? where comment_id = ?''', args)
        cur.close()

    @staticmethod
    def _make_comment(row):
        rv = Comment(*row[1:])
        rv.comment_id = row[0]
        return rv

    @staticmethod
    def get(comment_id):
        cur = get_cursor()
        cur.execute('select * from comments where comment_id = ?', (comment_id,))
        row = cur.fetchone()
        if row is None:
            raise ValueError('comment not found')
        try:
            return Comment._make_comment(row)
        finally:
            cur.close()

    @staticmethod
    def get_for_page(associated_page):
        cur = get_cursor()
        cur.execute('select * from comments where associated_page = ?',
                    (associated_page,))
        try:
            return [Comment._make_comment(row) for row in cur]
        finally:
            cur.close()

    @staticmethod
    def get_recent(n=10):
        cur = get_cursor()
        cur.execute('select * from comments order by comment_id desc limit ?',
                    (n,))
        try:
            return [Comment._make_comment(row) for row in cur]
        finally:
            cur.close()

    def __repr__(self):
        return '<Comment by %r on %r (%s)>' % (
            self.author,
            self.associated_page,
            self.comment_id or 'not saved'
        )
