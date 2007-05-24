# -*- coding: utf-8 -*-
"""
    sphinx.web.antispam
    ~~~~~~~~~~~~~~~~~~~

    Small module that performs anti spam tests based on the bad content
    regex list provided by moin moin.

    :copyright: 2007 by Armin Ronacher.
    :license: Python license.
"""
from __future__ import with_statement
import re
import urllib
import time
from os import path

DOWNLOAD_URL = 'http://moinmaster.wikiwikiweb.de/BadContent?action=raw'
UPDATE_INTERVAL = 60 * 60 * 24 * 7


class AntiSpam(object):

    def __init__(self, bad_content_file):
        self.bad_content_file = bad_content_file
        self.lines = None

        if not path.exists(self.bad_content_file):
            last_change = 0
        else:
            last_change = path.getmtime(self.bad_content_file)
        if last_change + UPDATE_INTERVAL < time.time():
            try:
                f = urllib.urlopen(DOWNLOAD_URL)
                data = f.read()
            except:
                pass
            else:
                self.lines = [l.strip() for l in data.splitlines()
                              if not l.startswith('#')]
                f = file(bad_content_file, 'w')
                f.write('\n'.join(self.lines))
                last_change = int(time.time())

        if self.lines is None:
            with file(bad_content_file) as f:
                self.lines = [l.strip() for l in f]

    def is_spam(self, fields):
        for line in self.lines:
            regex = re.compile(line)
            for field in fields:
                if regex.search(field) is not None:
                    return True
        return False
