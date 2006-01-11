=================
 aafigure README
=================

Overview
========

The idea is to parse ASCII art images, embedded in reST documents and output
an image. This would mean that simple illustrations could be embedded as
ASCII art in the reST source and still look nice when converted to e.g. HTML


Implementation
==============

Files:

aafigure.py
    ASCII art parser. This is the main module.

aafigure_directive.py
    Implmements the ``aafigure`` Docutils directive that takes these
    ASCII art figures ang generates SVG.

aa.py
    ASCII art output backend. Intended for tests, not for the end user.

pil.py
    Bitmap output backend. Using PIL, it can write PNG, JPEG and more formats.

rst2html.py
    Patched version that adds the ``aafigure`` Docutils directive.
    
svg.py
    SVG output backend.


The ``aafigure`` module contains code to parse ASCII art figures and create
a list of of shapes. The different output modules can walk trough a list of
shapes and write image files.


Usage
=====
::

    ./rst2html.py README.txt >README.html

This results in the ``README.html`` file and a ``.svg`` file for each
``aafigure``.

Display the resulting ``README.html`` file in a SVG capable browser. It has
been tested with Firefox 1.5.



Short introduction
==================
This code in a reST document that is processed with the enhanced ``rst2html.py``
looks like this::

    .. aafigure::
   
        -->

Which results in an image like this:

.. aafigure::

    -->

The ``aafigure`` directive has the following options:

- ``:scale: <float>``   enlarge or shrink image

- ``:line_width: <float>``   change line with (svg only currently)

- ``:line_format: <str>`` choose backend/output format: 'svg', 'png', all
  bitmap formats that PIL supports can be used but only few make sense. Line
  drawings have a good compression and better qualify when saved as PNG
  rather than a JPEG. The best quality will be achieved with SVG, tough not
  all browsers support this vector image format at this time.

- ``:foreground: <str>``   foreground color in the form ``#rgb`` or ``#rrggbb``

- ``:background: <str>``   background color in the form ``#rgb`` or ``#rrggbb``
  (*not* for SVG output)

- ``:fill: <str>``   fill color in the form ``#rgb`` or ``#rrggbb``

- ``:name: <str>``   use this as filename instead of the automatic generated
  name


Lines
-----
The ``-`` and ``|`` are normaly used for lines. ``_`` can also be used. it is a
slightly longer line than the ``-`` and it's is drawn a bit lower::

  ---- |         ___
       | --  ___|

.. aafigure::

  ---- |         ___
       | --  ___|

Arrows
------
Arrow styles are::

    --->   | | | | | |
    ---<   | | | | | |
    ---o   ^ V v o O #
    ---O
    ---#

.. aafigure::

    --->   | | | | | |
    ---<   | | | | | |
    ---o   ^ V v o O #
    ---O
    ---#

Boxes
-----
Boxes are automaticaly draw when the edges are made with ``+``, filled
boxes are made with ``X``::

    +-----+   XXX  /--\
    |     |   XXX  |  |
    +-----+   XXX  \--/

.. aafigure::

    +-----+   XXX  /--\
    |     |   XXX  |  |
    +-----+   XXX  \--/

Text
----
The images may contain text too. Currently only alphanumeric characters are
allowed::

    Hello World

.. aafigure::

    Hello World

Other
-----

::

    A big dot:  *

.. aafigure::

    A big dot:  *



TODO
====

- Symbol detection: scan for predefined shapes in the ASCII image
  and output them as symbol from a library
- Symbol libraries for UML, flowchart, electronic schematics, ...
- The way the image is embedded is a hack (inserting a tag trough a raw node...)
- Search for ways to bring in color.
- aafigure probably needs arguments like ``font-family``, ...
- Punctuation not included in strings, underlines in strings are tricky to
  detect...


License
=======

BSD

Tests
=====

Simple tests
------------
Different arrow types:

.. aafigure::

    <-->  >->   --> <--
    >--<  o-->  -->+<--
    o--o          o=>

Boxes and shapes:

.. aafigure::

    +---------------+
    |A box with text|
    +---------------+

.. aafigure::

        ---> | ^|   |   +++
        <--- | || --+-- +++
        <--> | |V   |   +++<-
     __             __    ^
    |  |__  +---+  |__|   |
            |box|   ..
            +---+  Xenophon


Flow chart
----------
.. aafigure::

        /---------\
        |  Start  |
        \----+----/
             |
             V
        +----+----+
        |  Init   |
        +----+----+
             |
             +<-----------+
             |            |
             V            |
        +----+----+       |
        | Process |       |
        +----+----+       |
             |            |
             V            |
        +----+----+  yes  |
        |  more?  +-------+
        +----+----+
             | no
             V
        /----+----\
        |   End   |
        \---------/


UML
---
No not realy, yet. But you get the idea.

.. aafigure::
    :scale: 0.8

    +---------+ +---------+ +---------+
    |object 1 | |object 2 | |object 3 |
    +----+----+ +----+----+ +----+----+
         |           |           |
         |           |           |
         X           |           |
         X---------->X           |
         X           X           |
         X<----------X           |
         X           |           |
         X           |           |
         X---------------------->X
         |           |           X
         X---------->X           X--+
         X           X           X  |
         |           |           X<-+
         X<----------------------X
         X           |           |
         |           |           |
         |           |           |

.. aafigure::
    :scale: 0.8
    
    +---------+         +---------+     +---------+
    |  Shape  |         |  Line   |     |  Point  |
    +---------+         +---------+   2 +---------+
    | draw    +<--------+ start   +----O+ x       |
    | move    +<---+    | end     |     | y       |
    +---------+    |    +---------+     +---------+
                   |                  
                   |    +---------+
                   +----+ Circle  |
                        +---------+
                        | center  |
                        | radius  |
                        +---------+

.. aafigure::

                            /-----------\     yes /-----------\
                      +  +->| then this |--->*--->| then this |
     /------------\   +--+  \-----------/    |no  \-----------/
     | First this |-->+                      |
     \------------/   +--+  /---------\      V               /----- \
                      +  +->| or that |------*-------------->| Done |
                            \---------/                      \------/
                                                         
Electronics
-----------
It would be cool if it could display simple schematics.

.. aafigure::
    :fill: #fff

           R1    
    O------XXXX----o-----O
           100k    |
                  --- C1
                  --- 100n
                   |
    O--------------o-----O

.. - Resistor should not be filled -> can be solved by symbol detection

- Capacitor not good, would prefer ``--||--``  -> symbol detection


Timing diagrams
---------------
.. aafigure::

      ^    ___     ___           ____
    A |___|   |___|   |_________|    |______
      |      ___        ___           __   
    B |_____|   |______|   |________XX  XX__
      |
      +-------------------------------------> t
      
Here is one with descriptions:

.. aafigure::

                        SDA edge
         start                              stop
           |    |          |                 |
           v    v          v                 v
        ___      __________                   ___
    SDA    |    |          |                 |
           |____|          |_____..._________|
        ______      _____       _..._       _____
    SCL       |    |     |     |     |     |
              |____|     |_____|     |_____|
                                            
              ^    ^     ^     ^     ^     ^
              |    |     |     |     |     |  
              |  sh_in   |   sh_in   |   sh_in
            sh_out     sh_out      sh_out   
                                            
                        SCL edge

Statistical diagrams
--------------------

Benfords_ distribution of the sizes of files on my harddrive:

.. _Benfords: http://en.wikipedia.org/wiki/Benfords_law

.. aafigure::
    :name: benford
    :foreground: #ff1050

      |
    1 +------------------------------------------------------------> 31.59%
    2 +-------------------------------> 16.80%
    3 +-----------------------> 12.40%
    4 +-----------------> 9.31%
    5 +--------------> 7.89%
    6 +-----------> 6.10%
    7 +---------> 5.20%
    8 +---------> 4.90%
    9 +--------> 4.53%
      |         +         |         +         |         +         |
      +---------+---------+---------+---------+---------+---------+--->
      |         +         |         +         |         +         |
      0         5        10        15        20        25        30

Just some bars:

.. aafigure::
    :fill: #00b

    ^     2
    |    XX
    | 1  XX       4
    |XX  XX   3  XX
    |XX  XX  XX  XX
    |XX  XX  XX  XX
    +------------------>
