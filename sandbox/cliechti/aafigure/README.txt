aafigure README
===============

Overview
--------

The idea is to parse ASCII art images, embedded in reST documents and output
an image. This would mean that simple illustrations could be embedded as
ASCII art in the reST source and still look nice when converted to e.g. HTML

Implementation
--------------

Files:

aafigure.py
    ASCII art parser. This is the main module.

aafigure_directive.py
    Implmements the ``aafigure`` Docutils directive that takes these
    ASCII art figures ang generates SVG.

aa.py
    ASCII art output backend. Intended for tests, not for the end user.

rst2html.py
    Patched version that adds the ``aafigure`` Docutils directive.
    
svg.py
    SVG output backend.


Usage
-----
::

    ./rst2html.py README.txt >README.html

This results in the ``README.html`` file and a ``.svg`` file for each
``aafigure``.

Display the resulting ``README.html`` file in a SVG capable browser. It has
been tested with Firefox 1.5.

TODO
----

- symbol detection: scan for some predefined shapes in the ASCII image
  and output them as symbol form a library
- symbol libraries for UML, flowchart, electronic schematics, ...
- more arrow heads
- the way the image is embedded is a hack (inserting an ``<embed ..>`` tag
  trough a raw node...
- search for ways to bring in color
- aafigure probably need arguments like ``font-family``, ...
- punctuation not included in strings

License
-------

BSD

Tests
-----

Simple tests
~~~~~~~~~~~~
Different arrow types:

.. aafigure::

    <-->
    >--<
    o--o

Boxes and shapes:

.. aafigure::

    +---------------+
    |A box with text|
    +---------------+

.. aafigure::

        ---> | ^|   |
        <--- | || --+--
        <--> | |V   |
     __             __
    |  |__  +---+  |__|
            |box|   ..
            +---+  Xenophon

A flow chart
~~~~~~~~~~~~
.. aafigure::
    :scale: 0.6

        +---------+
        | State 1 |
        +----+----+
             |
             +<------+
             |       |
             V       |
        +----+----+  |
        | State 2 |  |
        +----+----+  |
             |       |
             V       |
        +----+----+  |
        | State 3 |  |
        +----+----+  |
             |       |
             +-------+

UML
~~~
No not realy, yet. But you get the idea.

.. aafigure::

    object 1   object 2
    ----+----  ----+----
        |          |
        |          |
        X          |
        X--------->X
        X          X
        X<---------X
        X          |
        |          |
        |          |

Electronics
~~~~~~~~~~~
It would be cool if it could display simple schematics.

.. aafigure::

           R1    
    o------XXXX----o-----o
           100k    |
                  -+- C1
                  -+- 100n
                   |
    o--------------o-----o

- Resistor should not be filled -> can be solved by symbol detection
- Capacitor not good, would prefer --||--  -> symbol detection

Timing diagrams
~~~~~~~~~~~~~~~
.. aafigure::
    :scale: 0.4

        ___     ___
    ___|   |___|   |_____________________
          ___     ___
    _____|   |___|   |___________________

Or one with descriptions:

.. aafigure::
    :scale: 0.8

                        sda_edge
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
                                            
                        scl_edge
