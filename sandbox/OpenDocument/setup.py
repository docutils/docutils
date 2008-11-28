#!/usr/bin/env python
import sys, os
#from distutils.core import setup
from setuptools import setup


setup(name="odtwriter",
    version="1.3d",
    description="convert rst to ODF/odt/odp.",
    author="Dave Kuhlman",
    author_email="dkuhlman@rexx.com",
    packages=['docutils.writers.odtwriter', 'docutils.writers.odpwriter'],
    package_dir={'docutils.writers.odtwriter': 'odtwriter',    
                 'docutils.writers.odpwriter': 'odpwriter'},
    package_data={'docutils.writers.odtwriter': ['styles.odt', ],
                  'docutils.writers.odpwriter': ['styles.odp', ]},
    scripts=["tools/rst2odt.py", "tools/rst2odt_prepstyles.py",
             "tools/rst2odp.py"],
    )



