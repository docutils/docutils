#!/usr/bin/env python
"""
.. epigraph::

    Using ``raw`` is almost always evidence of a hack in progress.  It's not a
    clean solution.

    -- David Goodger.

Convert the default and ``texmath`` role to raw latex inline math and the
``texmath`` directive to display math.

Mangles include directives, replacing ``.txt`` extension with
``.mathhack.txt`` to help arranging preprocessing of included files.
"""

from rolehack import *

texmath = template('''\
raw:: latex

    $''', '$\n')

texdisplay = template('''\
raw:: latex

    \[ ''', ' \]\n')

def mangle_include(text):
    if text.endswith('.txt'):
        text = text[:-4] + '.mathhack.txt'
    return 'include:: ' + text

main({'texmath': texmath}, texmath,
     {'texmath': texdisplay, 'include': mangle_include})
