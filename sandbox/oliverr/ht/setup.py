#!/usr/bin/env python

from distutils.core import setup

def do_setup():
    dist = setup(
        name='rst2ht',
        description='A ht template Writer for Docutils',
        url='http://docutils.sf.net/',
        version='0.1',
        author='Ollie Rutherfurd',
        author_email='oliver@rutherfurd.net',
        license='Public Domain',
        packages=['docutils.writers'],
        package_dir={'docutils.writers':'writer'},
        scripts=['rst2ht.py']
    )
    return dist


if __name__ == '__main__':
    do_setup()

#:indentSize=4:lineSeparator=\n:maxLineLen=76:noTabs=true:tabSize=4:wrap=hard:
