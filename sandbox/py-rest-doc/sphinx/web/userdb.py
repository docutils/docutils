# -*- coding: utf-8 -*-
"""
    sphinx.web.userdb
    ~~~~~~~~~~~~~~~~~

    A module that provides pythonic access to the `docusers` file
    that stores users and their passwords so that they can gain access
    to the administration system.

    :copyright: 2007 by Armin Ronacher.
    :license: Python license.
"""
from __future__ import with_statement
from os import path
from hashlib import sha1


class UserDatabase(object):

    def __init__(self, filename):
        self.filename = filename
        self.users = {}
        if path.exists(filename):
            with file(filename) as f:
                for line in f:
                    if line.strip():
                        user, password = line.split(':', 1)
                        self.users[user.strip()] = password.strip()

    def add_user(self, user, password):
        if user in self.users:
            raise ValueError('user %r already exists' % user)
        self.users[user] = sha1('%s|%s' % (user, password)).hexdigest()

    def remove_user(self, user):
        del self.users[user]

    def set_password(self, user, password):
        if user not in self.users:
            raise ValueError('unknown user %r' % user)
        self.users[user] = sha1('%s|%s' % (user, password)).hexdigest()

    def check_password(self, user, password):
        return user in self.users and \
            self.users[user] == sha1('%s|%s' % (user, password)).hexdigest()

    def save(self):
        with file(self.filename, 'w') as f:
            for username, password in self.users.iteritems():
                f.write('%s:%s\n' % (username, password))
