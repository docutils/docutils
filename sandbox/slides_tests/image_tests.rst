==============================
Images Test for Slides Writers
==============================


:author: Au Thor
:date: January 1, 2525
:organization: Cu U


Slide 1
=======

Left-aligned image.

.. image:: test.png
   :align: left
   :height: 5cm
   :width: 5cm
   :alt: Test Image



Slide 2
=======

Right-aligned image.

.. image:: test.png
   :align: right
   :height: 5cm
   :width: 5cm
   :alt: Test Image



Slide 3
=======

Center-aligned image.

.. image:: test.png
   :align: right
   :height: 5cm
   :width: 5cm
   :alt: Test Image


Slide 4
=======

Center-aligned image with caption and legend.

.. figure:: test.png
   :align: right
   :height: 5cm
   :width: 5cm
   :alt: Test Image

   Caption for Test Figure

   Legend for Test Figure.


Slide 5
=======

Two images in a table.

.. list-table::

   * + .. image:: test.png
          :width: 90%
     + .. image:: test.png
          :width: 90%



Slide 6
=======

A figure and a list in a table.

.. list-table::

   * + .. image:: test.png
          :width: 90%
     +  First info
          with some details
          with some details
          with some details
        Second info
          with some details
          with some details
          with some details
        Third info
          with some details
          with some details
          with some details

