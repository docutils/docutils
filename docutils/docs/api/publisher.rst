.. include:: ../header.rst

========================
 The Docutils Publisher
========================

:Author: David Goodger
:Contact: docutils-develop@lists.sourceforge.net
:Date: $Date$
:Revision: $Revision$
:Copyright: This document has been placed in the public domain.

.. contents::
   :depth: 2

.. _Publisher:

The ``docutils.core.Publisher`` class is the core of Docutils,
managing all the processing and relationships between components. [#]_

The `Publisher convenience functions`_ are the normal entry points
for using Docutils as a library.
`Entry point functions`_ with pre-set components are provided
for use in `"console_scripts" entry points`_.
Configuration is done via `convenience function arguments`_ and
`runtime settings`_ assembled from several sources.

.. [#] See the `Docutils Project Model`_ in PEP 258 for an overview.
.. _Docutils Project Model: ../peps/pep-0258.html#docutils-project-model


Publisher Convenience Functions
===============================

There are several convenience functions in the ``docutils.core`` module.
Each of these functions sets up a `docutils.core.Publisher` object,
then calls its `publish()` method which handles everything else.
For details, see the `argument reference`_, the function docstrings
and the source file `core.py`_.

.. TODO: generate API documentation with Sphinx and add links to it.


publish_cmdline()
-----------------

Function for custom `command-line applications`_.

.. parsed-literal::

  def publish_cmdline(reader_\ =None, reader_name=None, [#component-names]_
                      parser_\ =None, parser_name=None, [#component-names]_
                      writer_\ =None, writer_name=None, [#component-names]_
                      settings_\ =None, settings_spec_\ =None,
                      settings_overrides_\ =None, config_section_\ =None,
                      enable_exit_status_\ =False,
                      argv=None, usage=default_usage,
                      description=default_description) -> str | bytes

The function reads input from `sys.stdin` or a file specified on the
command line, writes the output document to a file, and also returns it
as `str` instance. [#binary-output]_

See the `entry point functions`_ in `core.py`_, the scripts in the
`tools/`_ directory, and `Inside A Docutils Command-Line Front-End Tool`_
for usage examples.

.. [#binary-output] Documents in binary formats (currently only ODT_)
                    are returned as a `bytes` instance.


publish_file()
--------------

For programmatic use with `file I/O`_.

.. parsed-literal::

    def publish_file(source__\ =None, source_path__\ =None,
                     destination_\ =None, destination_path__\ =None,
                     reader_\ =None, reader_name=None, [#component-names]_
                     parser_\ =None, parser_name=None, [#component-names]_
                     writer_\ =None, writer_name=None, [#component-names]_
                     settings_\ =None, settings_spec_\ =None,
                     settings_overrides_\ =None, config_section_\ =None,
                     enable_exit_status_\ =False) -> str | bytes

Read input from a file-like object.
Write the output document to a file-like object and also return it as
`str` instance. [#binary-output]_

__ `source (file I/O)`_
__ `source_path (file I/O)`_
__ `destination_path (file I/O)`_


publish_string()
----------------

For programmatic use with `string I/O`_.

.. parsed-literal::

    def publish_string(source__, source_path__\ =None,
                       destination_path__\ =None,
                       reader_\ =None, reader_name=None, [#component-names]_
                       parser_\ =None, parser_name=None, [#component-names]_
                       writer_\ =None, writer_name=None, [#component-names]_
                       settings_\ =None, settings_spec_\ =None,
                       settings_overrides_\ =None, config_section_\ =None,
                       enable_exit_status_\ =False) -> bytes | str

__ `source (string I/O)`_
__ `source_path (string I/O)`_
__ `destination_path (string I/O)`_

Get the input from the `source <source (string I/O)_>`__ argument
(a `str` or `bytes` instance).
Return the output document as a **bytes** instance unless the
output_encoding_ runtime setting has the special value ``"unicode"``. [#]_

Example:
  Return a Docutils XML version of the source as `str` instance::

    publish_string(source, writer='xml',
        settings_overrides={'output_encoding': 'unicode'})


.. Caution::
    The output_encoding_ and output_encoding_error_handler_ runtime
    settings may affect the content of the output document:
    Some document formats contain an *encoding declaration*,
    some formats use substitutions for non-encodable characters.

    Use `publish_parts()`_ to get a `str` instance of the output document
    together with values of the "output_encoding" and
    "output_encoding_error_handler" settings.

The `publish_str()` function is *provisional* because in Python 3 the
name and default return type no longer match.

.. [#] similar to `xml.etree.ElementTree.tostring()`__
__ https://docs.python.org/3/library/xml.etree.elementtree.html
   #xml.etree.ElementTree.tostring


publish_doctree()
-----------------

For programmatic use with `string input`_.
Parse input into a `Docutils Document Tree`_ data structure.
Return a `nodes.document`_ instance.

.. parsed-literal::

    publish_doctree(source__, source_path__\ =None,
                    source_class_\ =io.StringInput,
                    reader_\ =None, reader_name=None, [#component-names]_
                    parser_\ =None, parser_name=None, [#component-names]_
                    settings_\ =None, settings_spec_\ =None,
                    settings_overrides_\ =None, config_section_\ =None,
                    enable_exit_status_\ =False) -> nodes.document_

__ `source (string I/O)`_
__ `source_path (string I/O)`_

The Document Tree can be modified, pickled & unpickled,
etc., and then reprocessed with `publish_from_doctree()`_.


publish_from_doctree()
----------------------

For programmatic use with `string output`_.
Render from an existing Document Tree data structure
(nodes.document_ instance).
Return the output document as a `bytes` or `str` instance
(cf. `publish_string()`_).

.. parsed-literal::

  publish_from_doctree(document, destination_path__\ =None,
                       writer_\ =None, writer_name=None, [#component-names]_
                       settings_\ =None, settings_spec_\ =None,
                       settings_overrides_\ =None, config_section_\ =None,
                       enable_exit_status_\ =False) -> bytes | str

__ `destination_path (string I/O)`_

The `publish_from_doctree()` function is *provisional* because in
Python 3 the name and default return type of the `string I/O`_ interface
no longer match.


.. _publish-parts-details:

publish_parts()
---------------

For programmatic use. With `string input`_ (default) or `file input`_
(depending on the value of the source_class_ argument).
Returns a dictionary of document parts.

.. parsed-literal::

    def publish_parts(source__, source_path__\ =None,
                      source_class_\ =io.StringInput,
                      destination_path__\ =None,
                      reader_\ =None, reader_name=None, [#component-names]_
                      parser_\ =None, parser_name=None, [#component-names]_
                      writer_\ =None, writer_name=None, [#component-names]_
                      settings_\ =None, settings_spec_\ =None,
                      settings_overrides_\ =None, config_section_\ =None,
                      enable_exit_status_\ =False) -> dict

__ `source (string I/O)`_
__ `source_path (string I/O)`_
__ `destination_path (string I/O)`_


Dictionary keys are the part names.
Each Writer component may publish a different set of document parts:

.. contents::
   :local:

Example:
  post-process the output document with a custom function
  ``post_process()`` before encoding with user-customizable
  encoding and errors::

       def publish_bytes_with_postprocessing(*args, **kwargs):
           parts = publish_parts(*args, **kwargs)
           out_str = post_process(parts['whole'])
           return out_str.encode(parts['encoding'], parts['errors'])

There are more usage examples in the `docutils/examples.py`_ module.

.. _docutils/examples.py: ../../docutils/examples.py
.. _ODT: ../user/odt.html


Parts Provided by All Writers
`````````````````````````````

_`"encoding"` : str
    The `output_encoding`_ setting.

_`"errors"` : str
    The `output_encoding_error_handler`_ setting.

_`"version"` : str
    The version of Docutils used.

_`"whole"` : str | bytes
    The entire formatted document. [#binary-output]_


Parts Provided by the HTML Writers
``````````````````````````````````

All parts returned by the HTML writers are of data type `str`.

HTML4 Writer
^^^^^^^^^^^^

_`"body"`
    Equivalent to `"fragment"`_.  It is *not* equivalent to `"html_body"`_.

_`"body_prefix"`
    Contains ::

        </head>
        <body>
        <div class="document" ...>

    and, if applicable ::

        <div class="header">
        ...
        </div>

_`"body_pre_docinfo"`
    Contains (as applicable)::

        <h1 class="title">...</h1>
        <h2 class="subtitle" id="...">...</h2>

_`"body_suffix"`
    Contains ::

        </div>

    (the end-tag for ``<div class="document">``), the footer division
    if applicable::

        <div class="footer">
        ...
        </div>

    and ::

        </body>
        </html>

_`"docinfo"`
    Bibliographic data, i.e. the `\<docinfo>`_ element's content, [#]_
    rendered as a table.

    .. [#] Abstract and dedication are included in
           the `"body"`_/`"fragment"`_ part.

_`"footer"`
    The document footer content, meant to
    appear at the bottom of a web page, or repeated at the bottom of
    every printed page.

_`"fragment"`
    The document body (*not* the HTML ``<body>``).
    In other words, ``part['fragment']`` contains the entire document,
    less the document `"title"`_, `"subtitle"`_, `"docinfo"`_,
    `"header"`_, and `"footer"`_.  Equivalent to part `"body"`_.

_`"head"`
    Contains ``<meta ... />`` tags and the document
    ``<title>...</title>``. See also `"html_head"`_.

_`"head_prefix"`
    The XML declaration, the DOCTYPE declaration,
    the ``<html ...>`` start tag, and the ``<head>`` start tag.

_`"header"`
    The document header content, meant to appear at the top
    of a web page, or repeated at the top of every printed page.

_`"html_body"`
    The HTML ``<body>`` content,
    less the ``<body>`` and ``</body>`` tags themselves.

_`"html_head"`
    The HTML ``<head>`` content, less the `"stylesheet"`_
    and the ``<head>`` and ``</head>`` tags themselves.

    The "Content-Type" meta tag's "charset" value is left unresolved,
    as ``%s``::

        <meta http-equiv="Content-Type" content="text/html; charset=%s" />

    The interpolation should be done by client code.
    Alternatively, use part `"head"`_ which interpolates
    the "charset" value with `"encoding"`_.

_`"html_prolog"`
    The XML declaration and the doctype declaration.
    The XML declaration's "encoding" attribute's value is left unresolved,
    as "%s"::

        <?xml version="1.0" encoding="%s" ?>

    The interpolation should be done by client code.

_`"html_subtitle"`
    The document subtitle, including the
    enclosing ``<h2 class="subtitle">`` and ``</h2>`` tags.

_`"html_title"`
    The document title, including the
    enclosing ``<h1 class="title">`` and ``</h1>`` tags.

_`"meta"`
    Contains all ``<meta ... />`` tags.

_`"stylesheet"`
    The embedded stylesheet or stylesheet link.

_`"subtitle"`
    The document subtitle text and any inline markup.
    It does not include the enclosing ``<h2>`` and ``</h2>`` tags.

_`"title"`
    The document title text and any inline markup.
    It does not include the enclosing ``<h1>`` and ``</h1>`` tags.

The default template_ joins `"head_prefix"`_, `"head"`_, `"stylesheet"`_,
`"body_prefix"`_, `"body_pre_docinfo"`_, `"docinfo"`_, `"body"`_, and
`"body_suffix"`_ to get part `"whole"`_.


PEP/HTML Writer
^^^^^^^^^^^^^^^

The PEP/HTML writer provides the same parts as the `HTML4 writer`_,
plus the following:

_`"pepnum"`
    The PEP number (extracted from the `header preamble`__).

    __ https://peps.python.org/pep-0001/#pep-header-preamble


S5/HTML Writer
^^^^^^^^^^^^^^

The S5/HTML writer provides the same parts as the `HTML4 writer`_.


HTML5 Writer
^^^^^^^^^^^^

The HTML5 writer provides the same parts as the `HTML4 writer`_.
However, it uses semantic HTML5 elements for the document, `"header"`_, and
`"footer"`_ and a description list for the `"docinfo"`_ part.


Parts Provided by the (Xe)LaTeX Writers
```````````````````````````````````````

All parts returned by the (Xe)LaTeX writers are of data type `str`.

_`"abstract"`
    Formatted content of the "abstract" `bibliographic field`_.

"body"
    The document's content. In other words, ``parts['body']`` contains
    the entire document, except the document title, subtitle, and
    docinfo.

    This part can be included into another LaTeX document body using the
    ``\input{}`` command.

"body_pre_docinfo"
    The ``\maketitle`` command.

_`"dedication"`
    Formatted content of the "dedication" `bibliographic field`_.

"docinfo"
    Bibliographic data, i.e. the `\<docinfo>`_ element's content,
    rendered as a table.

    With ``--use-latex-docinfo`` the <author>, <organization>,
    <contact>, <address" and <date> are moved to `"titledata"`_.

"fallbacks"
    Fallback definitions for Docutils-specific LaTeX commands and environments.

"head_prefix"
    The declaration of documentclass and document options.

"latex_preamble"
    The argument of the ``--latex-preamble`` option.

"pdfsetup"
    PDF properties ("hyperref" package setup).

"requirements"
    Required packages and setup before the stylesheet inclusion.

"stylesheet"
    The embedded stylesheet(s) or stylesheet loading command(s).

"subtitle"
    Document subtitle text and any inline markup.

"title"
    Document title text and any inline markup.

_`"titledata"`
    The combined title data in ``\title``, ``\author``, and ``\date`` macros.

    With ``--use-latex-docinfo``, this includes the <author>,
    <organization>, <contact>, <address" and <date> docinfo items.

See the template files default.tex_, titlepage.tex_, titlingpage.tex_,
and xelatex.tex_ for examples how these parts are combined
into a valid LaTeX document.

.. _default.tex: ../../docutils/writers/latex2e/default.tex
.. _titlepage.tex: ../../docutils/writers/latex2e/titlepage.tex
.. _titlingpage.tex: ../../docutils/writers/latex2e/titlingpage.tex
.. _xelatex.tex: ../../docutils/writers/latex2e/xelatex.tex


publish_programmatically()
--------------------------

Auxiliary function used by `publish_file()`_, `publish_string()`_,
`publish_doctree()`_, and `publish_parts()`_.
Applications should not need to call this function directly.


Entry Point Functions
---------------------

The functions rst2html(), rst2html4(), rst2html5(), rst2latex(),
rst2man(), rst2odt(), rst2pseudoxml(), rst2s5(), rst2xetex(),
and rst2xml() are wrappers around `publish_cmdline()`_ used in
`"console_scripts" entry points`_ for eponymous `command-line applications`_.

.. _"console_scripts" entry points:
    https://packaging.python.org/en/latest/specifications/entry-points/


.. _convenience function arguments:

Argument Reference
==================

.. _file input:

File I/O
--------

The function `publish_file()`_ uses the file I/O interface;
`publish_parts()`_ can be configured to use file input
by setting the `source_class`_ argument to `docutils.io.FileInput`.

.. _source (file I/O):

source : file-like
  A file-like object holding the document source
  (must have `read()` and `close()` methods).

  Default: None (open `source_path <source_path (file I/O)_>`__
  or use `sys.stdin`).

  .. _source_path (file I/O):

source_path : str | pathlib.Path
  Path to the source file,
  opened if `source <source (file I/O)_>`__ is None.

  Default: None (use `source <source (file I/O)_>`__).

_`destination` : file-like
  A file-like object that will receive the output document
  (must have `write()` and `close()` methods).

  Default: None (open `destination_path <destination_path (file I/O)_>`__
  or use `sys.stdout`).

  .. _destination_path (file I/O):

destination_path : str | pathlib.Path
  Path to the destination file, opened if destination_ is None.

  Default: None (use destination_).


.. _string input:
.. _string output:

String I/O
----------

The functions `publish_string()`_, `publish_doctree()`_,
`publish_from_doctree()`_, and `publish_parts()`_ use the
string I/O interface provided by the `docutils.io.StringInput`
and `docutils.io.StringOutput` classes.

.. _source (string I/O):

source : str | bytes
  The document source. `bytes` are decoded with the encoding
  specified in the input_encoding_ setting.

  Required.

  .. _source_path (string I/O):

source_path : str
  Path to the file or name of the object that produced
  `source <source (string I/O)_>`__.
  Used as `"source" attribute`_ in diagnostic output.

  Default: None.

_`source_class` : docutils.io.Input
  Change the semantics of the "source" and "source_path" arguments.
  The value `docutils.io.FileInput` lets "source" and "source_path"
  behave as described in `file I/O`_.

  Default: `docutils.io.StringInput`.

  .. _destination_path (string I/O):

destination_path : str
  Path to the file or name of the object which will receive the output.
  Used for determining relative paths (stylesheets, source links, etc.)

  Default: None.

Component Specification
-----------------------

_`reader` : str | docutils.readers.Reader
  `Reader component name`_ or instance. [#component-names]_

  Default: "standalone".

_`parser` : str | docutils.parsers.Parser
  `Parser component name`_ or instance. [#component-names]_

  Default: "restructuredtext".

_`writer` : str | docutils.writers.Writer
  `Writer component name`_ or instance. [#component-names]_

  Default: "pseudoxml".

.. [#component-names] Up to Docutils 0.21, component *names* were specified
   with the "_`reader_name`", "_`parser_name`", and "_`writer_name`" arguments.
   These arguments are deprecated and will be removed in Docutils 2.0.


Settings Specification
----------------------

See also `Runtime Settings`_.

_`settings` : docutils.frontend.Values
  Runtime settings object.
  If `settings` is passed, it's assumed to be the end result of
  `runtime settings processing`_ and no further setting/config/option
  processing is done.

  Default: None.

_`settings_spec` : docutils.SettingsSpec_
  Application-specific settings definitions, independent of components.
  In other words, the application becomes a component, and its settings
  data is processed after the other components.
  Used only if settings_ is None.

  Default: None.

_`settings_overrides` : dict
  Application-specific settings defaults that override the defaults
  of other components.  Used only if settings_ is None.

  Default: None.

_`config_section` : str
  Name of the configuration file section for this application.

  Can be specified instead of settings_spec_ (a new
  docutils.SettingsSpec_ will be created) or in addition to
  settings_spec_ (overriding its `config_section` attribute).
  Used only if settings_ is None.

  Default: None.

_`enable_exit_status` : bool
  Set "system exit status" at end of processing?

  Default: False.


.. References
   ----------

.. _runtime settings processing:
.. _Runtime Settings: runtime-settings.html
.. _docutils.SettingsSpec: runtime-settings.html#settingsspec

.. _Inside A Docutils Command-Line Front-End Tool: ../howto/cmdline-tool.html

.. _Document Tree:
.. _Docutils Document Tree: ../ref/doctree.html
.. _<docinfo>: ../ref/doctree.html#docinfo
.. _nodes.document: ../ref/doctree.html#document
.. _"source" attribute: ../ref/doctree.html#source

.. _bibliographic field:
    ../ref/rst/restructuredtext.html#bibliographic-fields

.. _Docutils Configuration: ../user/config.html
.. _configuration files: ../user/config.html#configuration-files
.. _input_encoding: ../user/config.html#input-encoding
.. _output_encoding: ../user/config.html#output-encoding
.. _output_encoding_error_handler:
    ../user/config.html#output-encoding-error-handler
.. _template: ../user/config.html#template
.. _reader component name: ../user/config.html#reader
.. _parser component name: ../user/config.html#parser
.. _writer component name: ../user/config.html#writer-docutils-application

.. _command-line applications: ../user/tools.html

.. _core.py: ../../docutils/core.py
.. _tools/: ../../tools/
