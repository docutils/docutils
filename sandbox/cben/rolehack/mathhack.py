#!/usr/bin/env python
"""
.. epigraph::

    Using ``raw`` is almost always evidence of a hack in progress.  It's not a
    clean solution.

    -- David Goodger.

Convert the default and ``texmath`` role to raw latex inline math and the
``texmath`` directive to display math.
"""

from rolehack import *

texmath = template('''\
raw:: latex

    $''', '$\n')

texdisplay = template('''\
raw:: latex

    \[ ''', ' \]\n')

main({'texmath': texmath}, texmath,
     {'texmath': texdisplay})
