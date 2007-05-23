# -*- coding: utf-8 -*-
"""
    sphinx.web.database
    ~~~~~~~~~~~~~~~~~~~

    The models of the MVC thing we don't really use. Currently
    just used for comments.

    :copyright: 2007 by Georg Brandl, Armin Ronacher.
    :license: Python license.
"""
import sqlite3
from threading import local
from datetime import datetime


_thread_local = local()


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
        self.pub_date = pub_date
        self.author = author
        self.author_mail = author_mail
        self.comment_body = comment_body

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
    def get(comment_id):
        cur = get_cursor()
        cur.execute('select * from comments where comment_id = ?', (comment_id,))
        row = cur.fetchone()
        if row is None:
            raise ValueError('comment not found')
        rv = Comment(*row[1:])
        rv.comment_id = row[0]
        cur.close()
        return rv

    @staticmethod
    def get_for_page(associated_page):
        cur = get_cursor()
        cur.execute('select * from comments where associated_page = ?',
                    (associated_page,))
        result = []
        for row in cur:
            rv = Comment(*row[1:])
            rv.comment_id = row[0]
            result.append(rv)
        cur.close()
        return result

    def __repr__(self):
        return '<Comment by %r on %r (%s)>' % (
            self.author,
            self.associated_page,
            self.comment_id or 'not saved'
        )


def connect(path):
    """Connect and create tables if required."""
    con = sqlite3.connect(path)
    con.isolation_level = None

    # create tables that do not exist.
    for table in tables:
        try:
            con.execute('select * from %s;' % table)
        except sqlite3.OperationalError:
            con.execute(tables[table])

    _thread_local.connection = con
    return con


def get_cursor():
    """Return a new cursor."""
    return _thread_local.connection.cursor()


def set_connection(con):
    """Call this after thread creation to make this connection
    the connection for this thread."""
    _thread_local.connection = con
