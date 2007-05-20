# -*- coding: utf-8 -*-
"""
    sphinx.util
    ~~~~~~~~~~~

    Utility functions for Sphinx.

    :copyright: 2007 by Georg Brandl.
    :license: BSD license.
"""

import os
import sys
import fnmatch
from os import path


def relative_uri(base, to):
    """Return a relative URL from ``base`` to ``to``."""
    b2 = base.split('/')
    t2 = to.split('/')
    # remove common segments
    for x, y in zip(b2, t2):
        if x != y:
            break
        b2.pop(0)
        t2.pop(0)
    return '../' * (len(b2)-1) + '/'.join(t2)


def ensuredir(path):
    """Ensure that a path exists."""
    try:
        os.makedirs(path)
    except OSError, err:
        if not err.errno == 17:
            raise


def status_iterator(iterable, pre='', post='',
                    colorfunc=lambda x: x, stream=sys.stdout):
    """Print out each item before yielding it."""
    if pre:
        print >>stream, pre,

    for item in iterable:
        print >>stream, colorfunc(item),
        stream.flush()
        yield item

    print >>stream, post


def get_matching_files(dirname, pattern, exclude=()):
    """Get all files matching a pattern in a directory, recursively."""
    # dirname is a normalized absolute path.
    dirname = path.normpath(path.abspath(dirname))
    dirlen = len(dirname) + 1    # exclude slash
    for root, dirs, files in os.walk(dirname):
        dirs.sort()
        files.sort()
        for sfile in files:
            if not fnmatch.fnmatch(sfile, pattern):
                continue
            qualified_name = path.join(root[dirlen:], sfile)
            if qualified_name in exclude:
                continue
            yield qualified_name


def get_category(filename):
    """Get the "category" part of a RST filename."""
    parts = filename.split('/', 1)
    if len(parts) < 2:
        return
    return parts[0]


class attrdict(dict):
    def __getattr__(self, key):
        return self[key]
    def __setattr__(self, key, val):
        self[key] = val
    def __delattr__(self, key):
        del self[key]
