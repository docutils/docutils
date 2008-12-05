#!/usr/bin/env python
import sys, os
from distutils.core import setup

setup(name="odtwriter",
    version="1.3d",
    description="convert rst to ODF/odt/odp.",
    author="Dave Kuhlman",
    author_email="dkuhlman@rexx.com",
    packages=['docutils.writers.odtwriter'],
    package_dir={'docutils.writers.odtwriter': 'odtwriter'},
    package_data={'docutils.writers.odtwriter': ['styles.odt', ]},
    scripts=["tools/rst2odt.py", "tools/rst2odt_prepstyles.py"],
    )



