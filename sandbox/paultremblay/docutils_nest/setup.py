#!/usr/bin/env python

from distutils.core import setup



setup(name="docutils_extension_tools",
    version=".1",
    description="Extensions that add features to docutils.",
    author="Paul Tremblay",
    author_email="phthenry@earthlink.net",
    packages=['docutils_nest'],
    # apparently, the first in the tupple below
    # is the dirctory to install to, and the second
    # is the current location to build from?
data_files = [("/etc/docutils_nest", ["data/configure.xml"])],
    scripts=["scripts/docutils-nest-xml.py"],
    )

