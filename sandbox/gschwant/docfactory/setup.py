#!/usr/bin/env python

from distutils.core import setup

LONG_DESCRIPTION = """\
DocFactory is a wxPython-GUI for Docutils. It is distributed as
a subpackage of Docutils (docutils.factory). The main front-end
(docfactory.py) is installed as a "script". After installation
you should find it in the scripts-directory of your Python
environment.

System requirements:

* Python 2.1.1 or later (http://www.python.org).
* wxPython 2.3.4.2 or later (http://wxpython.org).  Be sure to get the
  build matching the version of Python you're using.
* Docutils 0.2.2 or later (http://docutils.sourceforge.net).  Use the
  CVS snapshot.
"""

def do_setup():
    dist = setup(
          name = 'DocFactory',
          description = 'wxPython-GUI for Docutils',
          long_description = LONG_DESCRIPTION,
          url = ('http://docutils.sourceforge.net/sandbox/gschwant/'
                 'docfactory/'),
          version = '0.2',
          author = 'Dr. Gunnar Schwant',
          author_email = 'g.schwant@gmx.de',
          license = 'BSD (see LICENSE.txt)',
          packages = ['docutils.factory'],
          package_dir = {'docutils.factory': 'factory'},
          scripts = ['docfactory.py'])
    return dist


if __name__ == '__main__' :
    do_setup()
