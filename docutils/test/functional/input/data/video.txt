Moving images (video)
=====================

If the URL given to `images and figures` hints to a video format
supported by HTML 5 (MIME types 'video/mp4', 'video/webm', or
'video/ogg'), the HTML5 writer will place it in a `video element`_.

.. image:: ../../../docs/user/rst/images/pens.mp4
   :align: left
   :alt: left-aligned test video

A class option value "controls" tells the browser to display controls
for video playback.

It is a good idea to include width and height attributes. If
height and width are not set, the page might flicker while the video
loads. According to the `HTML5 spec`__, hight and width must be
specified as pixel values.

.. figure:: ../../../docs/user/rst/images/pens.mp4
   :width: 200px
   :align: center
   :class: controls
   :alt: test video in a figure

   Simple test video in a centered figure

.. |testvideo| image:: ../../../docs/user/rst/images/pens.mp4
   :width: 60 px
   :alt: rotating pens video

A video like this |testvideo| can be included inline via substitution.

__
.. _video element:
   https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
