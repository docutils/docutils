SVG Images
----------

.. image:: ../../../docs/user/rst/images/biohazard.svg
   :width: 48 px
   :height: 48 px
   :align: left

Scalable vector graphics (SVG) images are the only standards-compatible
way to include vector graphics in HTML documents. However, they are not
supported by all backends/output formats (LaTeX, e.g., supports the PDF
or Postscript formats for vector graphics instead). Rendering behaviour
varies, depending on the SVG image itself, the method used to put the
image in the document, and the viewing agent.

All up-to-date HTML browsers support SVG, however not all do this fully
and in the same manner. Many older versions, especially IE < 9, have
deficiencies or require plug-ins (i.e. don't support the ``<img>`` tag).
The `html4css1` writer includes SVG images using ``<object>`` tags,
the `html5` writer uses ``<img>`` tags.


If the image is wrapped in ``<object>`` or ``<svg>`` tags, it depends on the
"viewBox" declaration inside the image's root ``<svg>`` element whether an
SVG image is scaled or clipped/padded. Images wrapped in ``<img>`` are
always scaled.

* .. image:: ../../../docs/user/rst/images/title-scaling.svg
     :width: 50%
     :align: right

  An image occupying 50% of the line width. The "viewBox" attribute in
  the image file enables auto-scaling also in ``<object>`` tags and
  embedded SVG.

* .. image:: ../../../docs/user/rst/images/title.svg
     :width: 50%
     :height: 15 px
     :align: right

  An image without "viewBox" in a box 50% wide and 15 pixles high.
  This image is scaled, if wrapped in an ``<img>`` tag but clipped in
  an ``<object>`` tag or within SVG.

* .. image:: ../../../docs/user/rst/images/title-scaling.svg
     :width: 50 %
     :height: 1.5 em
     :align: right

  A right aligned image with "viewBox", 50% wide and 1.5 em high.
  (SVG images keep the aspect ratio unless the "preserveAspectRatio"
  attribute is ``"none"``.)

* An inline image |inline-svg| scaled to a height of 0.8 em.

  .. |inline-svg| image:: ../../../docs/user/rst/images/biohazard-scaling.svg
     :height: 0.8 em

* .. image:: ../../../docs/user/rst/images/biohazard-scaling.svg
     :height: 1 em
     :align: right

  An image 1 em high, right aligned:

* An image 5 mm x 5 mm, centred, with hyperlink reference:

  .. image:: ../../../docs/user/rst/images/biohazard-scaling.svg
     :target: `SVG Images`_
     :width: 5 mm
     :height: 5 mm
     :align: center

* .. image:: ../../../docs/user/rst/images/biohazard.svg
     :width: 4 cm
     :height: 2 em
     :align: right

  An image without "viewBox" in a  4 cm x 2 em box.

Older versions of `webkit` based browsers (chromium, safari, midori,
konqueror) support the ``<img>`` tag but don't display contained bitmap
images.

* .. image:: ../../../docs/user/rst/images/biohazard-bitmap.svg
     :width: 3em
     :align: right

  A small SVG image with embedded bitmap, The ``:width:`` is set to 3 em
  in the rST source. Does not scale if wrapped in ``<object>`` tags
  because there is no "viewBox" attribute.


* .. image:: ../../../docs/user/rst/images/biohazard-bitmap-scaling.svg
     :width: 3em
     :align: right

  An SVG image with embedded bitmap and "viewBox", 3 em wide.

SVG images can also be put in figures:

   .. figure:: ../../../docs/user/rst/images/title-scaling.svg
      :alt: reStructuredText, the markup syntax
      :width:  290 px
      :height:  28 px
      :align: center

      SVG image in a figure.
