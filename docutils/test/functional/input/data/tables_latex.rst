Tables
======

In contrast to HTML, LaTeX does not support line-breaks in tables with
"automatic" column widths. Each cell has just one line, paragraphs are
merged (the writer emits a warning).

.. table:: problems with "auto" widths
   :widths: auto

   +-----------+------------------+
   | 11        | first paragraph  |
   |           |                  |
   |           | second paragraph |
   +-----------+------------------+
   | content   | 22               |
   | with      |                  |
   | linebreak |                  |
   +-----------+------------------+

To provide for arbitrary cell content, the LaTeX writer defaults to
specifying column widths computed from the source column widths. This
works sufficiently in many cases:

.. table:: a table with multi-paragraph multi-column cells

  +----------+--------------+---------------------------------+-----------+
  | test     | **bold hd**  | multicolumn 1                   | *emph hd* |
  |          |              |                                 |           |
  |          |              | With a second paragraph         |           |
  +----------+--------------+--------------+--------+---------+-----------+
  | multicolumn 2           | cell         | cell   | cell    | cell      |
  |                         |              |        |         |           |
  | With a second paragraph |              |        |         |           |
  +----------+--------------+--------------+--------+---------+-----------+
  | cell     | multicolumn 3 (one line,    | cell   | cell    | cell      |
  |          | but very very very very     |        |         |           |
  |          | very looooong)              |        |         |           |
  +----------+--------------+--------------+--------+---------+-----------+
  | cell     | cell         | cell         | Short multicolumn 4          |
  +----------+--------------+--------------+------------------------------+


A problem with the source-derived column widths is that simple tables
often use no padding while grid tables without padding look cramped:

.. table:: simple table, not padded in the source

   === = = =
    A  B C D
   === = = =
   100 2 3 4
   EUR b c d
   === = = =

.. table:: grid table, padded cells

   +-----+---+---+---+
   |  A  | B | C | D |
   +=====+===+===+===+
   | 100 | 2 | 3 | 4 |
   +-----+---+---+---+
   | EUR | b | c | d |
   +-----+---+---+---+

For better typographic results, setting the `width` and/or
`widths` options of the `table directive`_ is recommended.

.. table:: grid table, auto-width columns
   :widths: auto

   +-----+---+---+---+
   |  A  | B | C | D |
   +=====+===+===+===+
   | 100 | 2 | 3 | 4 |
   +-----+---+---+---+
   | EUR | b | c | d |
   +-----+---+---+---+

.. table:: table with multi-row header and "auto" column-widths
   :widths: auto

   +------------+-------------------+
   | XXX        | Variable Summary  |
   |            +-------------------+
   |            | Description       |
   +============+===================+
   | multi-column cell              |
   +--------------------------------+

The `width` option overrides "auto" `widths` as standard LaTeX tables
don't have a global width setting:

.. table:: This table has `widths` "auto" (ignored) and `width` 60%.
   :widths: auto
   :width: 60%

   === = = =
    A  B C D
   === = = =
   100 2 3 4
   EUR b c d
   === = = =

.. _table directive:
   https://docutils.sourceforge.io/docs/ref/rst/directives.html#table


Nested tables
-------------

+-----------------------------------------+-----------------+
| Lorem ipsum dolor sit amet, consectetur | adipisicing elit|
+-----------------------------------------+-----------------+
| .. table::                              | cell 1, 2       |
|     :align: right                       |                 |
|     :name: nested table                 |                 |
|                                         |                 |
|     +-----+-----+                       |                 |
|     |  1  |  2  |                       |                 |
|     +-----+-----+                       |                 |
+-----------------------------------------+-----------------+
| table width depends on parent column    | same table      |
|                                         |                 |
| .. table::                              |                 |
|     :align: center                      |                 |
|                                         |                 |
|     +-----+-----+                       | +-----+-----+   |
|     |  1  |  2  |                       | |  1  |  2  |   |
|     +-----+-----+                       | +-----+-----+   |
|                                         |                 |
| better use "auto" widths, see below     | in narrow column|
+-----------------------------------------+-----------------+
| .. table::                              | cell 3, 2       |
|     :align: right                       |                 |
|     :widths: auto                       |                 |
|                                         |                 |
|     +-----+-----+                       |                 |
|     |  1  |  2  |                       |                 |
|     +-----+-----+                       |                 |
|                                         |                 |
| definition:                             |                 |
|             list                        |                 |
+-----------------------------------------+-----------------+


TODO
----

* Tables with multi-paragraph multi-row cells currently fail due to a
  LaTeX limitation (see https://sourceforge.net/p/docutils/bugs/225/).

* Tweak vertical spacing in table cells containing multiple elements.

See also ``test/functional/input/data/latex-problematic.rst``.
