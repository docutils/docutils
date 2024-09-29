Test length specifications
==========================

Images and Figures
------------------

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :scale: 100

   scale 100%, no size specification

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :width: 25 %
   :figwidth: 50%

   width 25%, no height, figure width 50%

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 25%
   :figwidth: 50 %

   width 25%, height 16px, figure width 50%

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 10em
   :figwidth: 20 em

   width 10 em, figure width 20 em

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 20ex
   :figwidth: 40 ex

   width 20 ex, figure width 40 ex

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 15 ch
   :figwidth: 30ch

   width 15 ch, figure width 30 ch

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 10 rem
   :figwidth: 20rem

   width 10 rem, figure width 20 rem

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 25 vw
   :figwidth: 30 vw

   width 25 vw, figure width 30 vw

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 25 vh
   :figwidth: 30 vh

   width 25 vh, figure width 30 vh

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 25 vmin
   :figwidth: 30 vmin

   width 25 vmin, figure width 30 vmin

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 25 vmax
   :figwidth: 30 vmax

   width 25 vmax, figure width 30 vmax

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 5 cm
   :figwidth: 10 cm

   width 5 cm, figure width 10 cm

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 50mm
   :figwidth: 100 mm

   width 50 mm, figure width 100 mm

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 200 Q
   :figwidth: 400Q

   width 200 Q, figure width 400 Q

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 2 in
   :figwidth: 4in

   width 2 in, figure width 4 in

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 12 pc
   :figwidth: 24pc

   width 12 pc, figure width 24 pc

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 144pt
   :figwidth: 288pt

   width 144 pt, figure width 288pt

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 192 px
   :figwidth: 384px

   width 192 px, figure width 384 px

.. figure:: ../input/data/blue%20square.png
   :alt: blue square
   :height: 16px
   :width: 192
   :figwidth: 384

   width 192 <no unit>, figure width 384 <no unit>


ch, em, and rem: |test1ch| |test1em| |test1rem|
-----------------------------------------------

.. |test1em| image:: ../input/data/blue%20square.png
   :height: 1em

.. |test1rem| image:: ../input/data/blue%20square.png
   :height: 1rem

.. |test1ch| image:: ../input/data/blue%20square.png
   :height: 1ch

Image height 1ch, 1em, and 1rem: |test1ch| |test1em| |test1rem|

The units "em" and "ch" change with the current font size.
The unit "rem" is tied to the document root fontsize.


Tables
------

.. csv-table::

   value, unit

.. csv-table::
   :width: 25%

   25, %


.. csv-table::
   :width: 10em

   10, em

.. csv-table::
   :width: 20ex

   20, ex

.. csv-table::
   :width: 15 ch

   15, ch

.. csv-table::
   :width: 10rem

   10, rem

.. csv-table::
   :width: 25vw

   25, vw

.. csv-table::
   :width: 25vh

   25, vh

.. csv-table::
   :width: 25vmin

   25, vmin

.. csv-table::
   :width: 25vmax

   25, vmax

.. csv-table::
   :width: 5 cm

   5, cm

.. csv-table::
   :width: 50mm

   50, mm

.. csv-table::
   :width: 200 Q

   200, Q

.. csv-table::
   :width: 2 in

   2, in

.. csv-table::
   :width: 12 pc

   12, pc

.. csv-table::
   :width: 144pt

   144, pt

.. csv-table::
   :width: 192 px

   192, px

.. csv-table::
   :width: 192

   192, <no unit>


.. raw:: html

   <style type="text/css"><!--
    figure {background: lightgreen;}
    --></style>
