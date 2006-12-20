#!/usr/bin/env python
import sys, os
from distutils.core import setup


setup(name="rst_to_odt",
    version="1.0b",
    description="convert rst to ODF/odt.",
    author="Dave Kuhlman",
    author_email="dkuhlman@rexx.com",
    packages=['docutils.writers.odtwriter'],
    package_dir={'docutils.writers.odtwriter': 'odtwriter'},
    package_data={'docutils.writers.odtwriter': ['styles.odt',]},
    scripts=["tools/rst2odt.py"],
    )



