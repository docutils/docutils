Differences to the `html4css1` Writer
=====================================

* Use only meta_ keywords recognized by HTML 5.
  Add HTML5-compatible meta tags for docinfo items
  "authors", "date", and "copyright".

  Add a `viewport meta tag`__ to tell mobile browsers
  to use the device-width as viewport.

  __ https://developer.mozilla.org/en-US/docs/Web/HTML/Viewport_meta_tag

* Set table column widths with <style="width: ...">, not "width" argument.

* Horizontal alignment of table heads with CSS.

* Do not drop paragraph elements, use CSS rules to prevent unwanted vertical
  space.

* Put subtitles in <p> elements.

* Use the new semantic tags <main>, <section>, <header>,
  <footer>, <aside>, <figure>, and <figcaption>.
  See ``minimal.css`` and ``responsive.css`` for styling rule examples.

  Change the `initial_header_level` setting default to "2", as browsers
  use the `same style for <h1> and <h2> when nested in a <section\>`__.

  __ https://stackoverflow.com/questions/39547412/same-font-size-for-h1-and-h2-in-article

* Use HTML5 tags <small>, <s>, <q>, <dfn>, <var>, <samp>, <kbd>,
  <i>, <b>, <u>, <mark>, and <bdi> if a matching class value
  is found in `inline` and `literal` elements.
  Use <ins> and <del> if a matching class value
  is found in `inline`, `literal`, or `container` elements.
  (See `text-level semantics`_ and `indicating edits`_.)

* Use <img> tags for SVG images and <video> for video formats.

* .. image:: ../input/data/blue%20square.png
     :loading: embed
     :alt: blue square
     :align: right

  Embed images or defer fetching images with the image-loading_
  configuration setting or the "loading" option of the "image" directive.

  .. image:: ../../../docs/user/rst/images/biohazard.png
     :loading: lazy
     :width: 16
     :height: 16
     :align: right

  Especially with "lazy" loading, it is strongly recommended to
  specify both width and height of the image to prevent content layout
  shifts or use the "scale" option to let the writer insert the size
  determined from the image file.

.. _image-loading:
    https://docutils.sourceforge.io/docs/user/config.html#image-loading


Field List Rendering
--------------------

A `field list` is converted to a HTML `definition list` and given the
``field-list`` class argument value to allow CSS styling.
With ``html_plain.css``, the layout is similar to `html4css1`:

.. class:: open

:A long field name: sticks into the field body.

    The field body is pushed to the next line (you can suppress this
    behaviour with the `run-in`_ class argument).

:Customization: of the field name width is possible with CSS instead
    of the `field-name-limit` configuration setting, for
    example::

      dl.field-list > dd { margin-left: 6em; }

:Empty:

:fields:  must not lead to misalignment of the following content.

Styling with Class Arguments
----------------------------

The ``plain.css`` style sheet comes with some pre-defined style variants
that can be chosen via a class argument.

Description Lists
`````````````````

Definition lists with the "description" class argument:

.. class:: description

description lists
    Definition lists that are styled like in most dictionaries,
    encyclopedias etc. (as well as the LaTeX `description` environment).
label
    The term to be described. Put in boldface or italic.
Content
    starts on the same line and has a hanging indent.
A label without content
    ..
and the next label
    must align.

as well
    as content containing more than one

    paragraph

or
    * nested lists
    * item 2
or
    nested
      definition lists

Details disclosure elements
```````````````````````````

Items of `definition lists`_ with class argument "details" are converted
to `details`_ disclosure elements with the term becoming the "summary".

.. _closed-details:
.. class:: details

A summary
  with details only visible after user interaction.

.. _open-details:
.. class:: details open

Another summary
  with open details because the source list has the additional class
  value "open".

.. _details:
   https://www.w3.org/TR/html52/interactive-elements.html#the-details-element

Field List Variants
```````````````````

For field lists, the "compact/open", "narrow" and "run-in" styles are defined
in the style sheets ``plain.css`` and ``responsive.css``.

*compact*
  .. class:: compact

  :Feature: No additional space between list items.

  :Option: The ``--compact-field-lists`` command line option (and the
           corresponding configuration setting) set the `compact`
           class argument on all "simple" field lists, if not
           overridden with `open`.

  :Use:  For lists with short field body.

*open*
  .. class:: open

  :Feature: Additional space between list items also in "simple" lists.
            (Overrides the ``--compact-field-lists`` command line
            option and the corresponding configuration setting)

  :Use: For "simple" lists that should keep the space between list items.

*narrow*
  .. class:: narrow

  :Feature: Less indented field body.
  :Use:   For lists with short field names.
  :A long field name:
       sticks into the field body and the field body starts on a
       new line (if not combined with `run-in`_).

custom *field-indent*
  .. class:: field-indent-3em

  :Feature: Field body indented by custom amount.
  :Use:     class value starting with ``field-indent-`` followed by
            a valid length, e.g. ``field-indent-3em``.
  :The writer:
            will convert this class value to a ``style`` attribute setting.

.. _`run-in`:

*run-in*
  .. class:: run-in

  :Feature: Field body starts on the same line also after long field
            names.

  :A long field name: sticks into the field body which continues on
                         the same line.

  :The next field name:  and field body should align. Long text in the field
                         body is wrapped and aligns with other fields.

Table Variants
``````````````

The following styles can be applied to individual tables via a class
argument or as document wide setting with the table-style_ configuration
setting (or command line argument).

* Numbered tables can be achieved with the "numbered" class option:

  .. table:: truth values
     :class: numbered

     ======= ======= ==========
     A       B       A or B
     ======= ======= ==========
     False   False   False
     True    False   True
     False   True    True
     True    True    True
     ======= ======= ==========

  Currently, referencing to the table by number is not supported. This is a
  common request and already on the `TODO list`.

* A table with "booktabs" class value, is rendered similar to the style
  from the booktabs_ LaTeX package.

  .. _table-style:
     https://docutils.sourceforge.io/docs/user/config.html#table-style
  .. _booktabs:
     http://tug.ctan.org/tex-archive/macros/latex/contrib/booktabs/booktabs.pdf

"Booktabs" style table, numbered, centre-aligned, with auto-sized columns:

  .. table:: I/O values
     :class: booktabs numbered
     :align: center
     :widths: auto

     ======= ======= ==========
     Input           Output
     --------------- ----------
     A       B       A or B
     ======= ======= ==========
     False   False   False
     True    False   True
     False   True    True
     True    True    True
     ======= ======= ==========

Numbered Figures
````````````````

Numbered figures can be achieved with the "numbered" ``:figclass:`` option:

.. figure:: ../../../docs/user/rst/images/title-scaling.svg
   :alt: reStructuredText, the markup syntax
   :figclass: numbered
   :width: 100%
   :loading: embed

   Embedded SVG image in a numbered figure.
