#!/usr/bin/env python

# Author: Aleksey Gurtovoy
# Contact: agurtovoy@meta-comm.com
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.


import sys, os
from distutils.core import setup

setup(
    name="html_frames",
    version=".2",
    description="convert rst to a set of HTML pages/frames.",
    author="Aleksey Gurtovoy",
    author_email="agurtovoy@meta-comm.com",
    packages=['docutils.writers.html4_frames'],
    package_dir={'docutils.writers.html4_frames': 'writers/html4_frames'},
    package_data={'docutils.writers.html4_frames': ['style.css']},
    scripts=["tools/rst2htmlframes.py"],
    )
