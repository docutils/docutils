#!/usr/bin/env python

from distutils.core import setup

setup(name = "DocArticle",
      version = "0.1",
      description = "Turn reStructuredText source into O'Reilly DevCenter compatible HTML.",
      author = "Bill Bumgarner",
      url = "mailto:bbum@codefab.com",
      author_email = "bbum@codefab.com",
      packages = ['DocArticle'],
      scripts = ['docarticle.py']
      )
