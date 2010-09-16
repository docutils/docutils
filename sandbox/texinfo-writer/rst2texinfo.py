#!/usr/bin/env python

import locale
try:
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description
from texinfo import Writer


def main():
    description = ("Converts reStructuredText to Texinfo.  "
                   + default_description)
    publish_cmdline(writer=Writer(), description=description)


if __name__ == '__main__':
    main()
