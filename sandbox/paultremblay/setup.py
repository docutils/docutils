#!/usr/bin/env python

from distutils.core import setup



setup(name="docutils_extension_tools",
    version=".1",
    description="Extensions that add features to docutils.",
    author="Paul Tremblay",
    author_email="phthenry@earthlink.net",
    packages=['nest_inline', 'xml_tools_trem', 'txt_to_xml', 'rst_tools_trem', '.'],
    # apparently, the first in the tupple below
    # is the dirctory to install to, and the second
    # is the current location to build from?
data_files = [("/etc/nest_docutils", ["data/configure.xml"])],
    scripts=["scripts/docutils-nest-xml.py"],
    )

