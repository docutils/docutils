#!/usr/bin/env python

"""Generates Linux Format articles."""

__author__ = "Patrick K. O'Brien <pobrien@orbtech.com>"
__cvsid__ = "$Id$"
__revision__ = "$Revision$"[11:-2]

import locale
try:
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description

from LXFwriter import Writer


def main():
    description = ("Generates Linux Format articles.  " + default_description)
    publish_cmdline(writer=Writer(), description=description)


if __name__ == '__main__':
    main()
