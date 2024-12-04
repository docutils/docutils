Embedded images
===============

The image_loading_ setting can be used to tell the "html5" writer
to embed still images in the output document. [#]_ SVG images are
directly included, other images are base64_ encoded and included
as a `data URI`_.

.. figure:: ../../../docs/user/rst/images/biohazard.png
   :alt: biohazard
   :align: left
   :width: 2em
   :figwidth: 45%
   :class: align-center

   Embedded PNG image in a figure.

.. figure:: ../../../docs/user/rst/images/biohazard-scaling.svg
   :alt: biohazard
   :align: right
   :width: 2em
   :figwidth: 45%
   :class: align-center

   Embedded SVG image in a figure.

Embedded inline PNG image |inline-embedded| and SVG image
|inline-embedded-2| scaled to a height of 0.8 em.

.. |inline-embedded| image:: ../../../docs/user/rst/images/biohazard.png
   :height: 0.8 em
.. |inline-embedded-2| image::
   ../../../docs/user/rst/images/biohazard-scaling.svg
   :height: 0.8 em


.. image:: ../../../docs/user/rst/images/biohazard.svg
   :align: left
   :width: 1.5em
   :height: 1.5em
   :loading: lazy

The ``:loading:`` option of `"image" and "figure" directives`_ overrides the
image_loading_ setting for the respective image.

.. [#] Videos are only embedded if indicated in the
       `"image" directive`_'s "loading" option.

.. _image_loading:
    https://docutils.sourceforge.io/docs/user/config.html#image-loading
.. _base64: https://en.wikipedia.org/wiki/Base64
.. _data URI: https://en.wikipedia.org/wiki/Data_URI_scheme
.. _"image" directive:
.. _"image" and "figure" directives:
    ../../../docs/ref/rst/directives.html#images
