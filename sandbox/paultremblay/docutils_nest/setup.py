#!/usr/bin/env python

import os, sys
from distutils.core import setup


# get the location for the data
var_file_exists = os.path.isfile('var_file')
if not var_file_exists:
    sys.stderr.write('Please run python configure.py first\n')
    sys.exit(1)
read_obj = open('var_file', 'r')
lines = read_obj.readlines()
target = lines[0]
data_location = os.path.join(target, '.docutils_nest')
read_obj.close()


setup(name="docutils_nest",
    version=".1",
    description="Add nested inline markup to an rst file.",
    author="Paul Tremblay",
    author_email="phthenry@earthlink.net",
    packages=['docutils_nest'],
    data_files = [(data_location, ["data/configure.xml"])],
    scripts=["scripts/docutils-nest-xml.py"],
    )

