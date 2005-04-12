html2rst README
===============

Overview
--------

This is a hacked up version of Aaron Swartz' html2text_ to output reST.

Features:

- converts basic HTML documents to reST (titles, paragraphs, emphasizing)
- lists <ul> <ol>, images, <code> and <pre> blocks
- build a list of links, add reST references
- the output is word-wrapped

Problems:

- tables are not fully supported (cell contents is output, but no borders)
- it happens that it word-wraps URLs or does not catch the whole address
  in a reference
- it doesn't like non-ASCII characters, currently replaces them with a ``?``
- single backticks are put around all references even if it's one word.
- not configurable (wrap optional, references per chapter , etc.)


ToDo:

- totaly reinvent the thing and write a xml2rst writer (rst2rst ;-) and use
  XML transformations


Usage
-----
::

    html2rst.py index.html >index.txt


License
-------

The html2text_ script the code is based on, is licensed under the `GNU GPL 2`_.
This code is under the same license.



.. _html2text: http://www.aaronsw.com/2002/html2text/
.. _GNU GPL 2: http://www.gnu.org/copyleft/gpl.html