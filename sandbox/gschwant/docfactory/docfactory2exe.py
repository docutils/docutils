"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.2

Usage::

  python docfactory2exe.py py2exe -w -p encodings,docutils --icon docfactory.ico

"""

from distutils.core import setup
import py2exe

setup(name="DocFactory",
      scripts=["docfactory.py"],
      )
