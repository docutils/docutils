#!/usr/bin/env python
import sys, os
from distutils.core import setup

from distutils.core import setup


# get the location for the data
var_file_exists = os.path.isfile('var_file')
if not var_file_exists:
    sys.stderr.write('Please run python configure.py first\n')
    sys.exit(1)
read_obj = open('var_file', 'r')
lines = read_obj.readlines()
data_location = lines[0]
data_xslt_location = os.path.join(data_location, 'xslt_stylesheets')
read_obj.close()



setup(name="rst_to_docbook",
    version=".1",
    description="Extensions that add features to docutils.",
    author="Paul Tremblay",
    author_email="phthenry@earthlink.net",
    packages=['rst_to_docbook'],
    data_files = [(data_xslt_location, ["data/xslt_stylesheets/*"])],
    scripts=["scripts/rst2docbook.py"],
    )

os.remove('var_file')


