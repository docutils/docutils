#!/usr/bin/env python

from distutils.core import setup

def do_setup():
    dist = setup(
        name='Docutils DocBook XML Writer',
        description='A DocBook XML Writer for Docutils',
        url='http://docutils.sourceforge.net/sandbox/oliverr/docbook/',
        version='0.2',
        author='Ollie Rutherfurd',
        author_email='oliver@rutherfurd.net',
        license='Public Domain',
        packages=['docutils.writers'],
        package_dir={'docutils.writers':'writer'},
        scripts=['rst2docbook.py']
    )
    return dist

if __name__ == '__main__':
    do_setup()

#:indentSize=4:lineSeparator=\n:maxLineLen=76:noTabs=true:tabSize=4:wrap=hard:
