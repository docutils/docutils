#!/usr/bin/env python

from distutils.core import setup

def do_setup():
    dist = setup(
        name='html-with-navigation-bars-writer',
        description='A HTML writer for Docutils which supports navigation bars',
        url='http://docutils.sourceforg.net/sandbox/gschwant/htmlwnav/',
        version='0.1',
        author='Gunnar Schwant',
        author_email='g.schwant@gmx.de',
        license='Public Domain',
        packages=['docutils.writers'],
        package_dir={'docutils.writers':'writer'},
        scripts=['rst2htmlnav.py']
    )
    return dist

if __name__ == '__main__':
    do_setup()
