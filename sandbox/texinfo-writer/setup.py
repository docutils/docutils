
# Usage: python setup.py {build | install}

import os
from setuptools import setup, find_packages

setup(
    name                 = 'rst2texinfo',
    version              = '0.2',
    author               = 'Jon Waltman',
    author_email         = 'jonathan.waltman@gmail.com',
    description          = 'Converts reStructuredText to Texinfo',
    long_description     = open(os.path.join(os.path.dirname(__file__),
                                             'README.txt')).read(),
    license              = 'TBD',
    install_requires     = ['docutils'],
    scripts              = ['rst2texinfo.py'],
    py_modules           = ['texinfo'],
    include_package_data = True,
    classifiers = [
        "Development Status :: 2 - Pre-Alpha",
        "License :: OSI Approved",
        "Environment :: Console",
        "Intended Audience :: Developers",
        "Programming Language :: Python",
        "Topic :: Documentation",
        "Topic :: Terminals",
        "Topic :: Text Editors",
        "Topic :: Text Processing",
        "Topic :: Utilities",
    ],
)
