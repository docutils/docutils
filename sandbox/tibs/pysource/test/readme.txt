Files in the ``test`` directory
-------------------------------

This directory is intended to contain various test file. They are (hopefully)
named in a consistent manner:

README.txt
    This file.

``*.rtxt``
    Sample reST text files, suitable for checking odd things.

``*.py``
    Sample Python test files, ditto.

``*.doctest``
    Files that contain "semiliterate testing" [1]_ -- that is,
    descriptive text (written with reST) surrounding doctest blocks.
    The idea is that these files should be passed to (for instance)
    pydps.py (or whatever it is called at the moment) to be tested by
    Tim Peters' doctest - for instance::

        pydps/pydps.py --text --doctest simple.doctest



.. [1] a semi-lame reference to literate programming, I'm afraid.
