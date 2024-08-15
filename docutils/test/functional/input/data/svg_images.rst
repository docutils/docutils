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

.. figure:: ../../../docs/user/rst/images/title-scaling.svg
   :width: 40%
   :figwidth: 75%
   :align: center

   Figure with image occupying 40% of the figure width.

   The `viewBox` attribute in the image file enables scaling
   also in ``<object>`` and ``<svg>`` nodes.

All up-to-date HTML browsers support SVG, however not all do this fully
and in the same manner. Some older browsers, especially IE < 9, have
deficiencies or require plug-ins (i.e. don't support the ``<img>`` tag).
Older versions of `webkit` based browsers (chromium, safari, midori,
konqueror) support the ``<img>`` tag but don't display contained bitmap
images.

The "html4css1" writer includes SVG images as ``<object>`` elements,
the "html5" writer uses ``<svg>`` for embedded images and ``<img>``
else. The element type influences several aspects of image behaviour:

* Due to security/privacy considerations, browsers may block ``<object>``
  data from 3rd party sources.

* If an image is included as ``<object>`` or ``<svg>``,
  it depends on the `viewBox` declaration of the root ``<svg>`` element
  whether it is scaled or clipped/padded.
  Images in ``<img>`` elements are always scaled.

  .. image:: ../../../docs/user/rst/images/biohazard-scaling.svg
     :height: 1.2 em
     :align: left

  Image with `viewBox`, 1.2 em high, left aligned and |inline-svg| inline.

  .. |inline-svg| image:: ../../../docs/user/rst/images/biohazard-scaling.svg
     :height: 1.2 em

  .. image:: ../../../docs/user/rst/images/biohazard-scaling.svg
     :height: 5 mm
     :width: 15 mm
     :align: left

  Image with `viewBox`, 5 mm x 15 mm.

  .. image:: ../../../docs/user/rst/images/biohazard.svg
     :height: 5 mm
     :width: 15 mm
     :align: left

  Image without `viewBox`, 5 mm x 15 mm.

  .. image:: ../../../docs/user/rst/images/biohazard-bitmap-scaling.svg
     :width: 2 em
     :align: left

  Image with embedded bitmap and `viewBox`, 2 em wide.

  .. image:: ../../../docs/user/rst/images/biohazard-bitmap.svg
     :width: 2 em
     :align: left

  Image with embedded bitmap without `viewBox`.

* SVG images with `viewBox` keep the aspect ratio unless the
  `preserveAspectRatio` attribute is ``"none"``.
  The following two images are 25% wide and 1 em high:

  .. image:: ../../../docs/user/rst/images/title-scaling.svg
     :width: 25 %
     :height: 1 em
     :align: left

  Image with `viewBox`.

  .. image:: ../../../docs/user/rst/images/title.svg
     :width: 25 %
     :height: 1 em
     :align: left

  Image without `viewBox`.


  .. image:: http://oreillymedia.github.io/svg-essentials-examples/ch14/
             animated_clock_js.svg
     :target: http://oreillymedia.github.io/svg-essentials-examples/ch14/
              animated_clock_js.svg
     :alt: [animated clock]
     :height: 3em
     :align: right

* Hyperlinks and script actions attached to SVG elements are ignored in
  images included as ``<img>``. Hyperlinks in images included as
  ``<object>`` are opened in the "object frame". Hyperlinks specified in
  the rST source (with the ``:target:`` directive option) work in
  ``<img>`` and ``<svg>`` elements but are ignored in images included as
  ``<object>`` (unless the object is blocked).

  .. image:: ../input/data/object-with-hyperlink.svg
     :width: 2.4 em
     :align: left
     :target: `SVG Images`_

  .. image:: ../input/data/interactive-button.svg
     :class: align-right
     :target: `SVG Images`_

  Image with a link attached to the upper rectangle;
  interactive clock and button aligned to the right.

  .. image:: ../input/data/object-with-hyperlink.svg
     :width: 2.4 em
     :class: align-left
     :loading: embed
     :target: `SVG images`_

  .. image:: ../input/data/interactive-button.svg
     :class: align-right
     :loading: embed
     :target: `SVG images`_

  Embedded interactive SVG images.
