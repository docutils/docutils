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

aa.py
    ASCII art output backend. Intended for tests, not for the end user.

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
- aafigure probably need arguments like ``scale``, ``font-family``, ...
- the svg is sometimes cropped / too small

License
-------

BSD

Tests
-----

Simple tests
~~~~~~~~~~~~
.. aafigure::

    -->

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
It would be cool if if could display simple schematics.

.. aafigure::

           R1    
    o------XXXX----+-----o
           100k    |
                  -+- C1
                  -+- 100n
                   |
    o--------------+-----o

Timing diagrams
~~~~~~~~~~~~~~~
.. aafigure::

        ___     ___
    ___|   |___|   |_____________________
          ___     ___
    _____|   |___|   |___________________

Or one with descriptions:

.. aafigure::

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
