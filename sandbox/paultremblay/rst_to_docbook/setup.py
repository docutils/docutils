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
read_obj.close()



setup(name="rst_to_docbook",
    version=".1",
    description="Extensions that add features to docutils.",
    author="Paul Tremblay",
    author_email="phthenry@earthlink.net",
    packages=['rst_to_docbook'],
    data_files = [(data_location, ["data/data.txt"])],
    # apparently, the first in the tupple below
    # is the dirctory to install to, and the second
    # is the current location to build from?
    # data_files = [("/etc/docutils_nest", ["data/configure.xml"])],
    # scripts=["scripts/docutils-nest-xml.py"],
    )

os.remove('var_file')


