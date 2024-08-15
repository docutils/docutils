.. include:: ../header.rst

========================
 Docutils Configuration
========================

:Author: David Goodger
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

.. sidebar:: Docutils Security for Web Applications

   For a discussion about securing web applications, please see
   `Deploying Docutils Securely <../howto/security.html>`_.

.. contents::

.. raw:: html

   <style type="text/css"><!--
    dl.field-list {--field-indent: 4.7em;}
    --></style>


Introduction
============

Settings priority
-----------------

Configuration file settings override the built-in defaults, and
command-line options override all.
Some settings override also an associated setting; [#override]_
others append values from the various sources. [#append-values]_

For the technicalities, see `Docutils Runtime Settings`_.


Configuration Files
-------------------

Configuration files are used for persistent customization;
they can be set once and take effect every time you use a component,
e.g., via a `front-end tool`_.

By default, Docutils checks the following places for configuration
files, in the following order:

1. ``/etc/docutils.conf``: This is a system-wide configuration file,
   applicable to all Docutils processing on the system.

2. ``./docutils.conf``: This is a project-specific configuration file,
   located in the current directory.
   The Docutils front end has to be executed from the directory
   containing this configuration file for it to take effect (note that
   this may have nothing to do with the location of the source files).

3. ``~/.docutils``: This is a user-specific configuration file,
   located in the user's home directory.

If more than one configuration file is found, all will be read and
**later entries** will **override earlier ones**. [#append-values]_
For example, a "stylesheet" entry in a user-specific configuration file
will override a "stylesheet" entry in the system-wide file.

.. _DOCUTILSCONFIG:

The default implicit config file paths can be overridden by the
``DOCUTILSCONFIG`` environment variable.  ``DOCUTILSCONFIG`` should
contain a colon-separated (semicolon-separated on Windows) sequence of
config file paths to search for; leave it empty to disable implicit
config files altogether.  Tilde-expansion is performed on paths.
Paths are interpreted relative to the current working directory.
Empty path items are ignored.

.. |config| replace:: ``--config``

In addition, configuration files may be explicitly specified with the
|config|_ command-line option.  These configuration files are read after
the three implicit ones listed above (or the ones defined by the
``DOCUTILSCONFIG`` environment variable), and its entries will have
priority.


Configuration File Syntax
-------------------------

Configuration files are UTF-8-encoded text files.  The ConfigParser.py_
module from Python_'s standard library is used to read them.
From its documentation:

    The configuration file consists of sections, lead by a "[section]"
    header and followed by "name: value" entries, with continuations
    in the style of `RFC 822`_; "name=value" is also accepted.  Note
    that leading whitespace is removed from values.  ...  Lines
    beginning with "#" or ";" are ignored and may be used to provide
    comments.

.. Note:: No format string interpolation is done.

The following conventions apply to Docutils configuration files:

* Configuration file **entry names** correspond to internal `runtime
  settings`_.  Underscores ("_") and hyphens ("-") can be used interchangeably
  in entry names; hyphens are automatically converted to underscores.
  Entries with non-applicable or misspelled entry names are silently ignored.

  .. _boolean:

* For **boolean** settings (switches), the following values are
  recognized (case-independent):

  :True: "true", "yes", "on", "1"
  :False: "false", "no", "off", "0", "" (no value)

.. _comma-separated:

* **Lists** are specified either comma- or _`colon-separated`:

  *comma-separated*:
    strip_classes_, strip_elements_with_classes_, smartquotes_locales_,
    stylesheet_, stylesheet_dirs_, stylesheet_path_, use_bibtex_

    Whitespace around list values is stripped, so you can write, e.g., ::

      strip-classes: ham,eggs,
      strip-elements-with-classes: sugar, salt, flour
      stylesheet: html4css1.css,
                  math.css,
                  style sheet with spaces.css


  *colon-separated*:
    ignore_, prune_, sources_, expose_internals_

    Whitespace around the delimiter is not stripped. Write, e.g., ::

      expose_internals: source:line


Example
~~~~~~~

This is from the ``tools/docutils.conf`` configuration file supplied
with Docutils::

    # These entries affect all processing:
    [general]
    source-link: yes
    datestamp: %Y-%m-%d %H:%M UTC
    generator: on

    # These entries affect HTML output:
    [html writers]
    embed-stylesheet: no

    [html4css1 writer]
    stylesheet-path: docutils/writers/html4css1/html4css1.css
    field-name-limit: 20

    [html5 writer]
    stylesheet-dirs: docutils/writers/html5_polyglot/
    stylesheet-path: minimal.css, responsive.css


.. _configuration section:
.. _configuration sections:

Configuration File Sections & Entries
-------------------------------------

Below are the Docutils `runtime settings`_, listed by config file section.
Sections correspond to Docutils components (module name
or alias; section names are always in lowercase letters).

.. important:: Any setting may be specified in any section, but only
               settings from "`active sections`_" will be used.


.. _active sections:

Each Docutils application_ uses a specific set of components;
`corresponding configuration file sections`__ are "active" when the
application is used.

Configuration sections are applied in general-to-specific order:

1. `[general]`_

2. `[parsers]`_, parser dependencies, and the section specific to the
   Parser used ("[... parser]").

3. `[readers]`_, reader dependencies, and the section specific to the
   Reader used ("[... reader]").  For example, `[pep reader]`_ depends
   on `[standalone reader]`_.

4. `[writers]`_, writer family ("[... writers]"; if applicable),
   writer dependencies, and the section specific to the writer used
   ("[... writer]").  For example, `[pep_html writer]`_ depends
   on `[html writers]`_ and `[html4css1 writer]`_.

5. `[applications]`_, application dependencies, and the section
   specific to the Application (`front-end tool`_) in use
   ("[... application]").

Since any setting may be specified in any section, this ordering
allows component- or application-specific overrides of earlier
settings.  For example, there may be Reader-specific overrides of
general settings; Writer-specific overrides of Parser settings;
Application-specific overrides of Writer settings; and so on.

If multiple configuration files are applicable, the process is
completed (all sections are applied in the order given) for each one
before going on to the next.  For example, a "[pep_html writer]
stylesheet" setting in an earlier configuration file would be
overridden by an "[html4css1 writer] stylesheet" setting in a later
file.

Individual configuration sections and settings are described
in the following sections.
Some knowledge of Python_ is assumed for some attributes.


.. _ConfigParser.py:
   https://docs.python.org/3/library/configparser.html
.. _Python: https://www.python.org/
.. _RFC 822: https://www.rfc-editor.org/rfc/rfc822.txt
.. _front-end tool:
.. _application: tools.html
__ ../api/runtime-settings.html#active-sections


[general]
=========

Settings in the "[general]" section are always applied.


auto_id_prefix
--------------

Prefix prepended to all auto-generated `identifier keys` generated within
the document, after id_prefix_. Ensure the value conforms to the
restrictions on identifiers in the output format, as it is not subjected to
the `identifier normalization`_.

A trailing "%" is replaced with the tag name (new in Docutils 0.16).

:Default: "%" (changed from "id" in Docutils 0.18).
:Option:  ``--auto-id-prefix`` (hidden, intended mainly for programmatic use).

.. _identifier normalization:
   ../ref/rst/directives.html#identifier-normalization


datestamp
---------

Include a time/datestamp in the document footer.  Contains a
format string for Python's `time.strftime()`__.

Configuration file entry examples::

    # Equivalent to --date command-line option, results in
    # ISO 8601 extended format datestamp, e.g. "2001-12-21":
    datestamp: %Y-%m-%d

    # Equivalent to --time command-line option, results in
    # date/timestamp like "2001-12-21 18:43 UTC":
    datestamp: %Y-%m-%d %H:%M UTC

    # Disables datestamp; equivalent to --no-datestamp:
    datestamp:

:Default: None.
:Options: ``--date``, ``-d``, ``--time``, ``-t``, ``--no-datestamp``.

__ https://docs.python.org/3/library/time.html#time.strftime


debug
-----

Report debug-level system messages.

*Default*: None (disabled).  *Options*: ``--debug``, ``--no-debug``.


dump_internals
--------------

At the end of processing, write all internal attributes of the
document (``document.__dict__``) to stderr.

*Default*: None (disabled).
*Option*: ``--dump-internals`` (hidden, for development use only).


dump_pseudo_xml
---------------

At the end of processing, write the pseudo-XML representation of
the document to stderr.

*Default*: None (disabled).
*Option*: ``--dump-pseudo-xml`` (hidden, for development use only).


dump_settings
-------------

At the end of processing, write all Docutils settings to stderr.

*Default*: None (disabled).
*Option*: ``--dump-settings`` (hidden, for development use only).


dump_transforms
---------------

At the end of processing, write a list of all transforms applied
to the document to stderr.

*Default*: None (disabled).
*Option*: ``--dump-transforms`` (hidden, for development use only).


error_encoding
--------------

The text encoding [#encodings]_ for error output.

:Default: The encoding reported by ``sys.stderr``, locale encoding, or "ascii".
:Options: ``--error-encoding``, ``-e``.


error_encoding_error_handler
----------------------------

The error handler for unencodable characters in error output.
Acceptable values are the `Error Handlers`_ of Python's "codecs" module.
See also output_encoding_error_handler_.

:Default: "backslashreplace"
:Options: ``--error-encoding-error-handler``, ``--error-encoding``, ``-e``.


exit_status_level
-----------------

A system message level threshold; non-halting system messages at
or above this level will produce a non-zero exit status at normal
exit.  Exit status is the maximum system message level plus 10 (11
for INFO, etc.).

*Default*: 5 (disabled).  *Option*: ``--exit-status``.


expose_internals
----------------

List of internal attributes (colon-separated_) to expose
as external attributes (with "internal:" namespace prefix).
Values are appended. [#append-values]_

*Default*: empty list.
*Option*: ``--expose-internal-attribute`` (hidden, for development use only).


footnote_backlinks
------------------

Enable backlinks from footnotes_ and citations_ to their references.

:Default: True.
:Options: ``--footnote-backlinks``, ``--no-footnote-backlinks``.


generator
---------

Include a "Generated by Docutils" credit and link in the document footer.

:Default: None (disabled).
:Options: ``--generator``, ``-g``, ``--no-generator``.


halt_level
----------

The threshold at or above which system messages are converted to
exceptions, halting execution immediately.  If `traceback`_ is set, the
exception will propagate; otherwise, Docutils will exit.

See also report_level_.

*Default*: 4 (severe).  *Options*: ``--halt``, ``--strict``.


id_prefix
---------

Prefix prepended to all identifier keys generated within the document.
Ensure the value conforms to the restrictions on identifiers in the output
format, as it is not subjected to the `identifier normalization`_.
See also auto_id_prefix_.

*Default*: "" (no prefix).
*Option*: ``--id-prefix`` (hidden, intended mainly for programmatic use).


input_encoding
--------------

The text encoding [#encodings]_ for input.

:Default: utf-8 (changed from None (auto-detect_) in Docutils 0.22).
:Option: ``--input-encoding`` (shortcut ``-i`` removed in Docutils 0.22).


input_encoding_error_handler
----------------------------

The error handler for undecodable characters in the input.
Acceptable values are the `Error Handlers`_ of Python's "codecs" module,
including:

strict
    Raise an exception in case of an encoding error.
replace
    Replace malformed data with the official Unicode replacement
    character, U+FFFD.
ignore
    Ignore malformed data and continue without further notice.

The error handler may also be appended to the input_encoding_
setting, delimited by a colon, e.g. ``--input-encoding=ascii:replace``.

*Default*: "strict".
*Options*: ``--input-encoding-error-handler``.


language_code
-------------

Case-insensitive `language tag`_ as defined in `BCP 47`_.

Sets the document language, also used for localized directive and
role names as well as Docutils-generated text.

A typical language identifier consists of a 2-letter language code
from `ISO 639`_ (3-letter codes can be used if no 2-letter code
exists). The language identifier can have an optional subtag,
typically for variations based on country (from `ISO 3166`_
2-letter country codes).  Avoid subtags except where they add
useful distinguishing information. Examples of language tags
include "fr", "en-GB", "pt-br" (the same as "pt-BR"), and
"de-1901" (German with pre-1996 spelling).

The language of document parts can be specified with a
"language-<language tag>" `class attribute`_, e.g.
``.. class:: language-el-polyton`` for a quote in polytonic Greek.

*Default*: "en" (English).  *Options*: ``--language``, ``-l``.

.. _class attribute: ../ref/doctree.html#classes


output
------

The path of the output destination.
An existing file will be overwritten without warning.
Use "-" for `stdout`.

Obsoletes the `\<destination>`_ positional argument
(cf. `Future changes`_ in the RELEASE-NOTES).

*Default*: None (stdout). *Option*: ``--output``.

New in Docutils 0.20.

.. _Future changes: ../../RELEASE-NOTES.html#future-changes


output_encoding
---------------

The text encoding [#encodings]_ for output.
The special value "unicode" can be used with
the Publisher convenience functions `publish_string()`_ and
`publish_from_doctree()`_ to skip encoding and return a `str` instance
instead of `bytes`.

.. Note::
   * The "output_encoding" setting may affect the content of the output
     (e.g. an encoding declaration in HTML or XML or the representation
     of characters as LaTeX macro vs. literal character).
   * Docutils may introduce non-ASCII text if you use
     `auto-symbol footnotes`_.
     In non-English documents, also auto-generated labels
     may contain non-ASCII characters.

This setting is ignored by the `ODF/ODT Writer`_ which always usues UTF-8.

:Default: "utf-8".
:Option: ``--output-encoding`` (shortcut ``-o`` removed in Docutils 0.22).


output_encoding_error_handler
-----------------------------

The error handler for unencodable characters in the output.
Acceptable values are the `Error Handlers`_ of Python's "codecs" module,
including:

strict
    Raise an exception in case of an encoding error.
replace
    Replace malformed data with a suitable replacement marker,
    such as "?".
ignore
    Ignore malformed data and continue without further notice.
xmlcharrefreplace
    Replace with the appropriate XML character reference, such as
    "``&#8224;``".
backslashreplace
    Replace with backslash escape sequences, such as "``\u2020``".

The error handler may also be appended to the output_encoding_
setting using a colon as delimiter, e.g.
``--output-encoding=ascii:xmlcharrefreplace``.

*Default*: "strict".
*Options*: ``--output-encoding-error-handler``.


record_dependencies
-------------------

Path to a file where Docutils will write a list of files that
were required to generate the output, e.g. included files or embedded
stylesheets. [#dependencies]_  The format is one path per line with
forward slashes as separator, the encoding is UTF-8.

Set it to "-" in order to write dependencies to stdout.

This option is particularly useful in conjunction with programs like
``make`` using ``Makefile`` rules like::

  ham.html: ham.rst $(shell cat hamdeps.rst)
    rst2html --record-dependencies=hamdeps.rst ham.rst > ham.html

If the filesystem encoding differs from UTF-8, replace the ``cat``
command with a call to a converter, e.g.::

  $(shell iconv -f utf-8 -t latin1 hamdeps.rst)

*Default*: None (disabled).  *Option*: ``--record-dependencies``.

.. [#dependencies] Images are only added to the dependency list if they
   are embedded into the output or the reStructuredText parser extracted
   image dimensions from the file.


report_level
------------

Report system messages at or higher than <level>:

  1  info
  2  warning
  3  error
  4  severe
  5  none

See also halt_level_.

:Default: 2 (warning).
:Options: ``--report``, ``-r``, ``--verbose``, ``-v``, ``--quiet``, ``-q``.


root_prefix
-----------

Base directory, prepended to a filesystem path__ starting with "/" when
including files with the `"include"`_, `"raw"`_, or `"csv-table"`_
directives.

Also applied when a writer converts an image URI__ to a local filesystem
path in order to determine the image size or embed the image in the output.

:Default: "/" (keep paths unchanged).
:Option:  ``--root-prefix``

New in Docutils 0.21.

__ ../ref/rst/directives.html#path
__ ../ref/rst/directives.html#uri


sectnum_xform
-------------

Enable automatic section numbering by Docutils
(`docutils.transforms.parts.SectNum`) associated
with the `"sectnum" directive`_.

If disabled, section numbers might be added to the output by the
renderer (e.g. by LaTeX or via a CSS style definition).

:Default: True.
:Options: ``--section-numbering``, ``--no-section-numbering``.


source_link
-----------

Include a "View document source" link in the document footer.  URL will
be relative to the destination.

:Default: None (disabled).
:Options: ``--source-link``, ``-s``, ``--no-source-link``.


source_url
----------

An explicit URL for a "View document source" link, used verbatim.

:Default: None (compute from source_link_).
:Options: ``--source-url``, ``--no-source-link``.


strict_visitor
--------------

When processing a `document tree`_ with the Visitor pattern, raise an
error if a writer does not support a node type listed as optional.
For transitional development use.

:Default: None (disabled).
:Option:  ``--strict-visitor`` (hidden, for development use only).


strip_classes
-------------

List of "classes" attribute values (comma-separated_) that will be
removed from all elements in the `document tree`_.
Values are appended. [#append-values]_

Allows eliding class values that interfere with, e.g, CSS rules from 3rd
party tools/frameworks.

.. WARNING:: Some standard class values are required in order to achieve
             the expected output rendering; use with caution.

*Default*: empty list.  *Option*: ``--strip-class``.


strip_comments
--------------

Enable or disable the removal of comment elements from the `document tree`_.

:Default: None (disabled).
:Options: ``--strip-comments``, ``--leave-comments``.


strip_elements_with_classes
---------------------------

List of "classes" attribute values (comma-separated_).
Values are appended. [#append-values]_
Matching elements are removed from the `document tree`_.

.. WARNING:: Potentially dangerous: may lead to an invalid document tree
   and subsequent writer errors.  Use with caution.

*Default*: empty list.  *Option*: ``--strip-elements-with-class``.


title
-----

The `document title` as metadata which does not become part of the
document body. Stored as the document's `title attribute`_.
For example, in HTML output the metadata document title
appears in the title bar of the browser window.

This setting overrides a displayed `document title`_ and
is overridden by a `"title" directive`_.

:Default: None (the displayed `document title`_).
:Option:  ``--title``.

.. _title attribute: ../ref/doctree.html#title-attribute


toc_backlinks
-------------

Enable backlinks from section titles to table of contents entries
("entry"), to the top of the ToC ("top"), or disable (False).

:Default: "entry".
:Options: ``--toc-entry-backlinks``, ``--toc-top-backlinks``,
          ``--no-toc-backlinks``.


traceback
---------

Enable or disable Python tracebacks when halt-level system messages and
other exceptions occur.  Useful for debugging, and essential for issue
reports.  Exceptions are allowed to propagate, instead of being
caught and reported (in a user-friendly way) by Docutils.

:Default: None (disabled). [#]_
:Options: ``--traceback``, ``--no-traceback``.

.. [#] unless Docutils is run programmatically
       using the `Publisher Interface`_


warning_stream
--------------

Path [#pwd]_ to a file for the output of system messages (warnings).

*Default*: None (stderr).  *Option*: ``--warnings``.


[parsers]
=========

Generic parser options:

file_insertion_enabled
----------------------

Enable directives inserting the contents of external files,
such as `"include"`_ directive or the `"raw"`_ and `"csv-table"`_
directives with option "url" or "file".
A "warning" system message (including the directive text) is inserted
instead.  (See also raw_enabled_ for another security-relevant setting.)

:Default: True.
:Options: ``--file-insertion-enabled``, ``--no-file-insertion``.


line_length_limit
-----------------

Maximal number of characters in an input line or `"substitution"`_
definition. To prevent extraordinary high processing times or memory
usage for certain input constructs, a "warning" system message is
inserted instead.

*Default*: 10 000.
*Option*: ``--line-length-limit``

New in Docutils 0.17.


raw_enabled
-----------

Enable the `"raw" directive`_.  A "warning" system message
(including the directive text) is inserted instead.  See also
file_insertion_enabled_ for another security-relevant setting.

*Default*: True.  *Options*: ``--raw-enabled``, ``--no-raw``.


validate
--------

Validate the parsing result.

*Default*: False.  *Options*: ``--validate``, ``--no-validation``.


[restructuredtext parser]
-------------------------

character_level_inline_markup
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Relax the `inline markup recognition rules`_
requiring whitespace or punctuation around inline markup.

Allows character level inline markup without escaped whithespace and is
especially suited for languages that do not use whitespace to separate words
(e.g. Japanese, Chinese).

.. WARNING:: Potentially dangerous; use with caution.

   When changing this setting to "True", inline markup characters in
   URLs, names and formulas must be escaped to prevent recognition and
   possible errors.

   Examples:
     .. class:: borderless

     ===========================  ====================================
     ``http://rST_for_all.html``  hyperlinks to ``rST_`` and ``for_``
     ``x_2, inline_markup``       hyperlinks to ``x_`` and ``inline_``
     ``2*x``                      starts emphasised text
     ``a|b``                      starts a substitution reference
     ===========================  ====================================

:Default: False.
:Options: ``--character-level-inline-markup``, ``--word-level-inline-markup``.

New in Docutils 0.13.

pep_references
~~~~~~~~~~~~~~
Recognize and link to standalone PEP references (like "PEP 258").

:Default: None (disabled); True in PEP Reader.
:Option:  ``--pep-references``.

pep_base_url
~~~~~~~~~~~~
Base URL for PEP references.

*Default*: "https://peps.python.org/".
*Option*: ``--pep-base-url``.

pep_file_url_template
~~~~~~~~~~~~~~~~~~~~~
Template for PEP file part of URL, interpolated with the PEP
number and appended to pep_base_url_.

*Default*: "pep-%04d".  *Option*: ``--pep-file-url``.

rfc_references
~~~~~~~~~~~~~~
Recognize and link to standalone RFC references (like "RFC 822").

:Default: None (disabled); True in PEP Reader.
:Option:  ``--rfc-references``.

rfc_base_url
~~~~~~~~~~~~
Base URL for RFC references.

*Default*: "http://www.faqs.org/rfcs/".  *Option*: ``--rfc-base-url``.

smart_quotes
~~~~~~~~~~~~
Activate the SmartQuotes_ transform to
change straight quotation marks to typographic form. `Quote characters`_
are selected according to the language of the current block element (see
language_code_, smartquotes_locales_, and the `pre-defined quote sets`_).

Also changes consecutive runs of hyphen-minus and full stops (``---``,
``--``, ``...``) to em-dash, en-dash, and ellipsis Unicode characters
respectively.

Supported values:

:boolean_: Use smart quotes?
:alt:       Use alternative quote set (if defined for the language).

*Default*: None (disabled). *Option*: ``--smart-quotes``.

.. _SmartQuotes: smartquotes.html
.. _pre-defined quote sets: smartquotes.html#localization
.. _quote characters:
   https://en.wikipedia.org/wiki/Non-English_usage_of_quotation_marks

smartquotes_locales
~~~~~~~~~~~~~~~~~~~

List with language tag and a string of four typographical quote
characters (primary open/close, secondary open/close) for use by
the SmartQuotes_ transform.
Values are appended. [#append-values]_

Example:
  Ensure a correct leading apostrophe in ``'s Gravenhage`` in Dutch (at the
  cost of incorrect opening single quotes) and set French quotes to double
  and single guillemets with inner padding [#]_::

          smartquote-locales: nl: „”’’,
                              fr: « : »:‹ : ›

:Default: SmartQuotes' `pre-defined quote sets`_.
:Option:  ``--smartquotes-locales``.

New in Docutils 0.14.

.. [#] If more than one character per quote is required (e.g. padding in
       French quotes), a colon-separated list may be used for the quote set.

syntax_highlight
~~~~~~~~~~~~~~~~
Token type names used by Pygments_ when parsing contents of the `"code"`_
directive and role.

Supported values:

:long:  Use hierarchy of long token type names.
:short: Use short token type names.
        (For use with `Pygments-generated stylesheets`_.)
:none:  No code parsing. Use this to avoid the "Pygments not
        found" warning when Pygments is not installed.

*Default*: "long".  *Option*: ``--syntax-highlight``.

.. _Pygments: https://pygments.org/
.. _Pygments-generated stylesheets:
   https://pygments.org/docs/cmdline/#generating-styles

tab_width
~~~~~~~~~
Number of spaces for `hard tab expansion`_.

*Default*: 8.  *Option*: ``--tab-width``.

.. _hard tab expansion: ../ref/rst/restructuredtext.html#whitespace

trim_footnote_reference_space
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Remove spaces before `footnote references`_?

:Default: None (depends on the `footnote_references setting`_ [#]_).
:Options: ``--trim-footnote-reference-space``,
          ``--leave-footnote-reference-space``.

.. [#] The footnote space is trimmed if the reference style is "superscript",
       and it is left if the reference style is "brackets".


[myst parser]
-------------

Parser for Markdown (CommonMark_) with rST-compatibility extensions
provided by the 3rd party package `myst-docutils`_.
See `MyST with Docutils`_ and MyST's `Sphinx configuration options`_
(some settings are not applicable with Docutils).

.. _myst-docutils: https://pypi.org/project/myst-docutils/
.. _MyST with Docutils:
   https://myst-parser.readthedocs.io/en/latest/docutils.html
.. _Sphinx configuration options:
   https://myst-parser.readthedocs.io/en/latest/sphinx/reference.html#sphinx-config-options


[pycmark parser]
----------------

Parser for Markdown (CommonMark_)
provided by the 3rd party package `pycmark`_.
Currently no configuration settings.

.. _pycmark: https://pypi.org/project/pycmark/


[recommonmark parser]
---------------------

Parser for Markdown (CommonMark_)
provided by the 3rd party package recommonmark_.

.. admonition:: Deprecated

   Depends on deprecated 3rd-party package recommonmark_.
   Support will be removed in Docutils 1.0.

Currently no configuration settings.

.. _recommonmark: https://pypi.org/project/recommonmark/


[xml parser]
------------

The `Docutils XML parser` processes an XML representation of a
`Docutils Document Tree`_
(e.g. the output of the `Docutils XML writer <[docutils_xml writer]_>`__).

New in Docutils 0.22

Parser Specific Defaults
~~~~~~~~~~~~~~~~~~~~~~~~
.. class:: run-in narrow

:doctitle_xform_:  False.
:validate_:        True.


[readers]
=========

[standalone reader]
-------------------

docinfo_xform
~~~~~~~~~~~~~
Enable the `bibliographic field list`_ transform
(docutils.transforms.frontmatter.DocInfo).

*Default*: True.  *Options*: ``--no-doc-info``.

doctitle_xform
~~~~~~~~~~~~~~
Enable the promotion of a lone top-level section title
to `document title`_ (and subsequent section title to document
subtitle promotion; docutils.transforms.frontmatter.DocTitle).

*Default*: True.  *Options*: ``--no-doc-title``.

sectsubtitle_xform
~~~~~~~~~~~~~~~~~~
Enable the promotion of the title of a lone subsection
to a subtitle (docutils.transforms.frontmatter.SectSubTitle).

:Default: False.
:Options: ``--section-subtitles``, ``--no-section-subtitles``.


[pep reader]
------------

The `pep_references`_ and `rfc_references`_ settings
(`[restructuredtext parser]`_) are set on by default.


.. [python reader]
   ---------------

   Not implemented.


[writers]
=========

[docutils_xml writer]
---------------------

doctype_declaration
~~~~~~~~~~~~~~~~~~~
Generate XML with a DOCTYPE declaration.

*Default*: True.  *Options*: ``--no-doctype``.

indents
~~~~~~~
Generate XML with indents and newlines.

*Default*: None (disabled).  *Options*: ``--indents``.

newlines
~~~~~~~~
Generate XML with newlines before and after block-level tags.

*Default*: None (disabled).  *Options*: ``--newlines``.


.. _xml_declaration [docutils_xml writer]:

xml_declaration
~~~~~~~~~~~~~~~
Generate XML with an XML declaration.
See also `xml_declaration [html writers]`_.

.. Caution:: The XML declaration carries text encoding information.
   If the encoding is not UTF-8 or ASCII and the XML declaration is
   missing, standard tools may be unable to read the generated XML.

*Default*: True.  *Option*: ``--no-xml-declaration``.


[html writers]
--------------

.. _attribution [html writers]:

attribution
~~~~~~~~~~~
Format for `block quote`_ attributions: one of "dash" (em-dash
prefix), "parentheses"/"parens", or "none".
See also `attribution [latex writers]`_.

*Default*: "dash".  *Option*: ``--attribution``.


cloak_email_addresses
~~~~~~~~~~~~~~~~~~~~~
Scramble email addresses to confuse harvesters.  In the reference
URI, the "@" will be replaced by %-escapes (as of RFC 1738).  In
the visible text (link text) of an email reference, the "@" and
all periods (".") will be surrounded by ``<span>`` tags.
Furthermore, HTML entities are used to encode these characters in
order to further complicate decoding the email address.  For
example, "abc@example.org" will be output as::

    <a class="reference" href="mailto:abc&#37;&#52;&#48;example&#46;org">
    abc<span>&#64;</span>example<span>&#46;</span>org</a>

.. Note:: While cloaking email addresses will have little to no
   impact on the rendering and usability of email links in most
   browsers, some browsers (e.g. the ``links`` browser) may decode
   cloaked email addresses incorrectly.

*Default*: None (disabled).  *Option*: ``--cloak-email-addresses``.

compact_lists
~~~~~~~~~~~~~
Remove extra vertical whitespace between items of `bullet lists`_ and
`enumerated lists`_, when list items are all "simple" (i.e., items
each contain one paragraph and/or one "simple" sub-list only).
With the "html5" writer, the setting can be overridden for individual
lists using the `"class" directive`_ (values "compact" and "open").

:Default: True.
:Options: ``--compact-lists``, ``--no-compact-lists``.


compact_field_lists
~~~~~~~~~~~~~~~~~~~
Remove extra vertical whitespace between items of `field lists`_ that
are "simple" (i.e., all field bodies each contain at most one paragraph).
With the "html5" writer, the setting can be overridden for individual
lists using the `"class" directive`_ (values "compact" and "open").

:Default: True.
:Options: ``--compact-field-lists``, ``--no-compact-field-lists``.


.. _embed_stylesheet [html writers]:

embed_stylesheet
~~~~~~~~~~~~~~~~
Embed the stylesheet in the output HTML file.  The stylesheet file
must specified by the stylesheet_path_ setting and must be
accessible during processing.
See also `embed_stylesheet [latex writers]`_.

:Default: True.
:Options: ``--embed-stylesheet``, ``--link-stylesheet``.


.. _footnote_references setting:
.. _footnote_references [html writers]:

footnote_references
~~~~~~~~~~~~~~~~~~~
Format for `footnote references`_, one of "superscript" or "brackets".
See also `footnote_references [latex writers]`_.

Overrides also trim_footnote_reference_space_,
if the parser supports this option. [#override]_

*Default*: "brackets".  *Option*: ``--footnote-references``.

initial_header_level
~~~~~~~~~~~~~~~~~~~~
The initial level for section header elements.  This does not affect the
document title & subtitle; see doctitle_xform_.

:Default: writer dependent
          (see `[html4css1 writer]`_, `[html5 writer]`_, `[pep_html writer]`_).
:Option:  ``--initial-header-level``.


math_output
~~~~~~~~~~~
The format of mathematical content (`"math" directive`_ and role) in
the output document. Supported values are (case insensitive):

HTML:
  Format math in standard HTML enhanced by CSS rules.
  Requires the ``math.css`` stylesheet (in the system
  `stylesheet directory <stylesheet_dirs [html writers]_>`__)

  A `stylesheet_path <stylesheet_path [html writers]_>`__
  can be appended after whitespace. The specified
  stylesheet(s) will only be referenced or embedded if required
  (i.e. if there is mathematical content in the document).

MathML:
  Embed math content as presentational MathML_.

  Self-contained documents (no JavaScript, no external downloads).
  MathML is part of the HTML5 standard [#mathml-in-html4]_ and
  `supported by all major browsers`__ (since January 2023 also by Chrome).

  .. [#mathml-in-html4]
    With the "html4css1" writer, the resulting HTML document does
    not validate, as there is no DTD for `MathML + XHTML Transitional`.
    However, MathML-enabled browsers will render it fine.

  Docutil's latex2mathml converter supports a considerable
  `subset of LaTeX math syntax`__.

  An external converter can be appended after whitespace, e.g.,
  ``--math-output="MathML blahtexml"``:

  .. class:: run-in narrow

  :blahtexml_: Fast conversion, support for many symbols and environments,
               but no "align" (or other equation-aligning) environment. (C++)
  :LaTeXML_:   Comprehensive macro support but **very** slow. (Perl)
  :TtM_:       No "matrix", "align" and  "cases" environments.
               Support may be removed.
  :Pandoc_:    Comprehensive macro support, fast, but a large install size.
               (Haskell)

  __ https://developer.mozilla.org/en-US/docs/Web/MathML#browser_compatibility
  __ ../ref/rst/mathematics.html

MathJax:
  Format math for display with MathJax_, a JavaScript-based math rendering
  engine.

  :Pro: Works across multiple browsers and platforms.
        Large set of `supported LaTeX math commands and constructs`__
  :Con: Rendering requires JavaScript and an Internet connection or local
        MathJax installation.

  __ http://docs.mathjax.org/en/latest/input/tex/macros/index.html

  A URL pointing to a MathJax library should be appended after whitespace.
  A warning is given if this is missing.

  * It is recommended to install__ the MathJax library on the same
    server as the rest of the deployed site files.

    __ https://www.mathjax.org/#installnow

    Example: Install the library at the top level of the web
    server’s hierarchy in the directory ``MathJax`` and set::

      math-output: mathjax /MathJax/MathJax.js

  * The easiest way to use MathJax is to link directly to a public
    installation. In that case, there is no need to install MathJax locally.

    Downside: Downloads JavaScript code from a third-party site --- opens
    the door to cross-site scripting attacks!

    Example: MathJax `getting started`__ documentation uses::

      math-output: mathjax
         https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js

    See https://www.jsdelivr.com/ for details and terms of use.

    __ https://www.mathjax.org/#gettingstarted

  * Use a local MathJax installation on the *client* machine, e.g.::

      math-output: MathJax file:/usr/share/javascript/mathjax/MathJax.js

    This is the fallback if no URL is specified.

LaTeX:
  Include literal LaTeX code.

  The failsafe fallback.

:Default: writer dependent (see `[html4css1 writer]`_, `[html5 writer]`_).
:Option:  ``--math-output``.

.. _MathJax: http://www.mathjax.org/
.. _MathML: https://www.w3.org/TR/MathML/
.. _blahtexml: http://gva.noekeon.org/blahtexml/
.. _LaTeXML: http://dlmf.nist.gov/LaTeXML/
.. _TtM: http://hutchinson.belmont.ma.us/tth/mml/
.. _Pandoc: https://pandoc.org/


.. _stylesheet:
.. _stylesheet [html writers]:

stylesheet
~~~~~~~~~~
List of CSS stylesheet URLs  (comma-separated_). Used verbatim.

Overrides also stylesheet_path_. [#override]_
See also `stylesheet [latex writers]`_.

*Default*: empty list.  *Option*: ``--stylesheet``.


.. _stylesheet_dirs:
.. _stylesheet_dirs [html writers]:

stylesheet_dirs
~~~~~~~~~~~~~~~
List of directories where stylesheets can be found (comma-separated_).
Used by the stylesheet_path_ setting when resolving relative path arguments.

See also `stylesheet_dirs [latex writers]`_.

Note: This setting defines a "search path" (similar to the PATH variable for
executables). However, the term "path" is already used in the
stylesheet_path_ setting with the meaning of a file location.

:Default: the working directory of the process at launch
          and the directory with default stylesheet files
          (writer and installation specific).
          Use the ``--help`` option to get the exact value.
:Option:  ``--stylesheet-dirs``.


.. _stylesheet_path:
.. _stylesheet_path [html writers]:

stylesheet_path
~~~~~~~~~~~~~~~
List of paths to CSS stylesheets (comma-separated_). Relative paths are
expanded if a matching file is found in the stylesheet_dirs__.
If embed_stylesheet__ is False, paths are rewritten relative to
the output_ file (if specified) or the current work directory.

See also `stylesheet_path [latex writers]`_.

Overrides also stylesheet_. [#override]_
Pass an empty string (to either "stylesheet" or "stylesheet_path") to
deactivate stylesheet inclusion.

:Default: writer dependent (see `[html4css1 writer]`_,
          `[html5 writer]`_, `[pep_html writer]`_).
:Option:  ``--stylesheet-path``.

__ `embed_stylesheet [html writers]`_
__ `stylesheet_dirs [html writers]`_


.. _table_style [html writers]:

table_style
~~~~~~~~~~~
Class value(s) added to all tables_.
See also `table_style [latex writers]`_.

The default CSS sylesheets define:

:borderless: no borders around table cells,
:align-left, align-center, align-right: align table.

The HTML5 stylesheets also define:

:booktabs:
    Only lines above and below the table and a thin line after the head.
:captionbelow:
    Place the table caption below the table (new in Docutils 0.17).

In addition, the HTML writers process:

:colwidths-auto:
    Delegate the determination of table column widths to the back-end
    (leave out the ``<colgroup>`` column specification).
    Overridden by the "widths" option of the `"table" directive`_.
:colwidths-grid:
    Write column widths determined from the source to the HTML file.
    Overridden by the "widths" option of the `"table" directive`_.

*Default*: "".  *Option*: ``--table-style``.


.. _template [html writers]:

template
~~~~~~~~
Path [#pwd]_ to template file, which must be encoded in UTF-8.
See also `template [latex writers]`_.

:Default: "template.txt" in the writer's directory (installed automatically)
          For the exact machine-specific path, use the ``--help`` option).
:Option:  ``--template``.


.. _xml_declaration [html writers]:

xml_declaration
~~~~~~~~~~~~~~~
Prepend an XML declaration.
See also `xml_declaration [docutils_xml writer]`_.

.. Caution:: The XML declaration carries text encoding information.  If the
   encoding is not UTF-8 or ASCII and the XML declaration is missing,
   standard XML tools may be unable to read the generated XHTML.

:Default: writer dependent.
:Options: ``--xml-declaration``, ``--no-xml-declaration``.


[html4css1 writer]
~~~~~~~~~~~~~~~~~~
The `HTML4/CSS1 Writer`_ generates output that conforms to the
`XHTML 1 Transitional`_ specification.
It shares all settings defined in the `[html writers]`_
`configuration section`_.


Writer Specific Defaults
""""""""""""""""""""""""
.. class:: run-in narrow

:`initial_header_level`_:                               1 (for "<h1>").
:`math_output`_:                                        "HTML math.css".
:`stylesheet_path <stylesheet_path [html writers]_>`__: "html4css1.css".
:`xml_declaration <xml_declaration [html writers]_>`__: True.

.. _HTML4/CSS1 Writer: html.html#html4css1
.. _XHTML 1 Transitional: https://www.w3.org/TR/xhtml1/


field_name_limit
""""""""""""""""
The maximum length (in characters) for one-column `field names`_. Longer
field names will span an entire row of the table used to render the field
list.  0 indicates "no limit".  See also option_limit_.

*Default*: 14.  *Option*: ``--field-name-limit``.


option_limit
""""""""""""
The maximum length (in characters) for one-column options in `option lists`_.
Longer options will span an entire row of the table used to render
the option list.  0 indicates "no limit".
See also field_name_limit_.

*Default*: 14.  *Option*: ``--option-limit``.


[html5 writer]
~~~~~~~~~~~~~~
The `HTML5 Writer`_ generates valid XML that is compatible with `HTML5`_.
It shares all settings defined in the `[html writers]`_
`configuration section`_.

New in Docutils 0.13.

.. _HTML5 Writer: html.html#html5-polyglot
.. _HTML5: https://www.w3.org/TR/2014/REC-html5-20141028/


Writer Specific Defaults
""""""""""""""""""""""""
.. class:: run-in narrow

:initial_header_level_:  2 (reserve <h1> for the `document title`_). [#]_
:`math_output`_:  "MathML" (changed from "HTML math.css"in Docutils 0.22).
:`stylesheet_path <stylesheet_path [html writers]_>`__:
  "minimal.css, plain.css".
:`xml_declaration <xml_declaration [html writers]_>`__:  False.

.. [#] Documents without (visible) document title may have <h2> as highest
   heading level, which is not recommended but valid (cf. "`Headings and
   outlines`__" in the HTML Standard). The default will change to None
   (<h2> if there is a document title, else <h1>) in Docutils 1.0.

__ https://html.spec.whatwg.org/multipage/sections.html
   #headings-and-outlines-2

image_loading
"""""""""""""
Indicate at which point images should be loaded.
Overridden by the `"image" directive`_'s ``:loading:`` option.

Supported values:

:embed: Embed still images into the HTML document
        (ignored for videos).
:link:  Refer to images in the HTML document (default).
:lazy:  Refer to images. Additionally specify the
        `lazy loading attribute`_ to defer fetching the image.

*Default*: "link".  *Option*: ``--image-loading``.

New in Docutils 0.18.

.. _lazy loading attribute: https://html.spec.whatwg.org/multipage/
    urls-and-fetching.html#lazy-loading-attributes

section_self_link
"""""""""""""""""
Append an empty anchor element with a ``href`` to the section to
section headings. See ``responsive.css`` for an example how this can be
styled to show a symbol allowing users to copy the section's URL.

:Default: False.
:Options: ``--section-self-link``, ``--no-section-self-link``.

New in Docutils 0.18.


[pep_html writer]
~~~~~~~~~~~~~~~~~
The PEP/HTML Writer derives from the HTML4/CSS1 Writer, and shares
all settings defined in the `[html writers]`_ and `[html4css1 writer]`_
`configuration sections`_.

Writer Specific Defaults
""""""""""""""""""""""""
.. class:: run-in narrow

:`initial_header_level`_:  1 (for "<h1>").
:`stylesheet_path <stylesheet_path [html writers]_>`__:  "pep.css".
:`template <template [html writers]_>`__:
  ``docutils/writers/pep_html/template.txt`` in the installation directory.
  For the exact machine-specific path, use the ``--help`` option.

no_random
"""""""""
Do not use a random banner image.  Mainly used to get predictable
results when testing.

*Default*: None (use random banner).  *Options*: ``--no-random`` (hidden).

pep_home
""""""""
Home URL prefix for PEPs.

*Default*: "." (current directory).  *Option*: ``--pep-home``.

python_home
"""""""""""
Python's home URL.

*Default*: "https://www.python.org".  *Option*: ``--python-home``.


[s5_html writer]
~~~~~~~~~~~~~~~~
The S5/HTML Writer derives from the HTML4/CSS1 Writer, and shares
all settings defined in the `[html writers]`_ and `[html4css1 writer]`_
`configuration sections`_.

Writer Specific Defaults
""""""""""""""""""""""""
.. class:: run-in narrow

:compact_lists_:  disable compact lists.
:template__:      ``docutils/writers/s5_html/template.txt`` in the
                  installation directory.  For the exact machine-specific
                  path, use the ``--help`` option.

__ `template [html writers]`_


hidden_controls
"""""""""""""""
Auto-hide the presentation controls in slideshow mode, or keep
them visible at all times.

:Default: True (auto-hide).
:Options: ``--hidden-controls``, ``--visible-controls``.

current_slide
"""""""""""""
Enable or disable the current slide indicator ("1/15").

:Default: None (disabled).
:Options: ``--current-slide``, ``--no-current-slide``.

overwrite_theme_files
"""""""""""""""""""""
Allow or prevent the overwriting of existing theme files in the
``ui/<theme>`` directory.  This has no effect if "theme_url_" is
used.

:Default: None (keep existing theme files).
:Options: ``--keep-theme-files``, ``--overwrite-theme-files``.

theme
"""""
Name of an installed S5 theme, to be copied into a ``ui/<theme>``
subdirectory, beside the destination file (output HTML).  Note
that existing theme files will not be overwritten; the existing
theme directory must be deleted manually.
Overrides also theme_url_. [#override]_

*Default*: "default".  *Option*: ``--theme``.

theme_url
"""""""""
The URL of an S5 theme directory.  The destination file (output
HTML) will link to this theme; nothing will be copied.
Overrides also theme_. [#override]_

*Default*: None.  *Option*: ``--theme-url``.

view_mode
"""""""""
The initial view mode, either "slideshow" or "outline".

*Default*: "slidewhow".  *Option*: ``--view-mode``.



[latex writers]
----------------

Common settings for the `LaTeX writers`_
`[latex2e writer]`_ and `[xetex writer]`_.

.. _LaTeX writers: latex.html


.. _attribution [latex writers]:

attribution
~~~~~~~~~~~
Format for `block quote`_ attributions: one of "dash" (em-dash
prefix), "parentheses"/"parens", or "none".
See also `attribution  [html writers]`_.

*Default*: "dash".  *Option*: ``--attribution``.

compound_enumerators
~~~~~~~~~~~~~~~~~~~~
Enable or disable compound enumerators for nested `enumerated lists`_
(e.g. "1.2.a.ii").

:Default: None (disabled).
:Options: ``--compound-enumerators``, ``--no-compound-enumerators``.

documentclass
~~~~~~~~~~~~~
Specify LaTeX documentclass.

*Default*: "article".  *Option*: ``--documentclass``.

documentoptions
~~~~~~~~~~~~~~~
Specify document options.  Multiple options can be given, separated by
commas.

*Default*: "a4paper".  *Option*: ``--documentoptions``.


docutils_footnotes
~~~~~~~~~~~~~~~~~~
Use the Docutils-specific macros ``\DUfootnote`` and
``\DUfootnotetext`` for footnotes_.

TODO: The alternative, "latex_footnotes" is not implemented yet.

*Default*: True.  *Option*: ``--docutils-footnotes``.


.. _embed_stylesheet [latex writers]:

embed_stylesheet
~~~~~~~~~~~~~~~~
Embed the stylesheet(s) in the header of the output file.  The
stylesheets must be accessible during processing.  Currently, this
fails if the file is not available via the given path (i.e. the
file is *not* searched in the `TeX input path`_).
See also `embed_stylesheet [html writers]`_.

:Default: False (link to stylesheet).
:Options: ``--embed-stylesheet``, ``--link-stylesheet``.


.. _footnote_references [latex writers]:

footnote_references
~~~~~~~~~~~~~~~~~~~
Format for `footnote references`_: one of "superscript" or "brackets".
See also `footnote_references [html writers]`_.

Overrides also trim_footnote_reference_space_,
if the parser supports this option. [#override]_

*Default*: "superscript".  *Option*: ``--footnote-references``.


graphicx_option
~~~~~~~~~~~~~~~
LaTeX graphicx package option.

Possible values are "dvips", "pdftex", "dvipdfmx".

*Default*: "".  *Option*: ``--graphicx-option``.

hyperlink_color
~~~~~~~~~~~~~~~
Color of any hyperlinks embedded in text.

Special values:

.. class:: run-in narrow

:"0" or "false": disables coloring of links
                 (links will be marked by red boxes that are not printed).
:"black":        results in “invisible“ links.

Set hyperref_options_ to "draft" to completely disable hyperlinking.

*Default*: "blue".  *Option*: ``--hyperlink-color``.

hyperref_options
~~~~~~~~~~~~~~~~
Options for the `hyperref TeX package`_.

If hyperlink_color_ is not "false", the expansion of ::

  'colorlinks=true,linkcolor=%s,urlcolor=%s' % (
      hyperlink_color, hyperlink_color)

is prepended.

*Default*: "".   *Option*: ``--hyperref-options``.

.. _hyperref TeX package: http://tug.org/applications/hyperref/


latex_preamble
~~~~~~~~~~~~~~
LaTeX code that will be inserted in the document preamble.
Can be used to load packages with options or (re-) define LaTeX
macros without writing a custom style file.

:Default: writer dependent (see `[latex2e writer]`_, `[xetex writer]`_).
:Option:  ``--latex-preamble``.


legacy_class_functions
~~~~~~~~~~~~~~~~~~~~~~
Use legacy functions ``\DUtitle`` and ``\DUadmonition`` with a
comma-separated list of class values as optional argument. If `False`, class
values are handled with wrappers and admonitions use the ``DUadmonition``
environment. See `Generating LaTeX with Docutils`__ for details.

:Default: False (default changed in Docutils 0.18).
:Options: ``--legacy-class-functions``, ``--new-class-functions``.

New in Docutils 0.17

__ latex.html#classes


legacy_column_widths
~~~~~~~~~~~~~~~~~~~~
Use "legacy algorithm" to determine table column widths (for backwards
compatibility).

The new algorithm limits the table width to the text width or specified
table width and keeps the ratio of specified column widths.

Custom table and/or column widths can be set with the respective options
of the `"table" directive`_. See also `Generating LaTeX with Docutils`__.

:Default: True (will change to False in Docutils 1.0).
:Options: ``--legacy-column-widths``, ``--new-column-widths``.

New in Docutils 0.18.

__ latex.html#table-style


literal_block_env
~~~~~~~~~~~~~~~~~
Environment for `literal blocks`_. Used when the block does not contain
inline elements. [#]_

The values "lstlisting", "listing", "verbatim", "Verbatim", and
"verbatimtab" work out of the box; required LaTeX package are
automatically loaded.

:Default: "" (use "alltt" with quoting of whitespace and special chars).
:Option:  ``--literal-block-env``.

.. [#] A <literal-block> element originating from a `"parsed-literal"`_ or
   `"code"`_ directive may contain inline elements. LaTeX' verbatim-like
   environments cannot be used in this case.


reference_label
~~~~~~~~~~~~~~~
Per default the LaTeX writer uses ``\hyperref`` for `hyperlink
references`_ to internal__ or implicit__ targets.
Specify an alternative reference command name, e.g., "ref" or "pageref"
to get the section number or the page number as reference text.

.. Caution::
   * Drops the original reference text.
   * Experimental. Not sufficiently tested.
   * Fails, e.g., with section numbering by Docutils (cf. sectnum_xform_)
     or tables without caption.

.. admonition:: Provisional

   To be replaced by a dedicated `interpreted text role`_ for references
   (cf. TODO__).

*Default*: "" (use ``\hyperref``).  *Option*: ``--reference-label``.

__ ../ref/rst/restructuredtext.html#internal-hyperlink-targets
__ ../ref/rst/restructuredtext.html#implicit-hyperlink-targets
__ ../dev/todo.html#object-numbering-and-object-references
.. _hyperlink references: ../ref/rst/restructuredtext.html#hyperlink-references
.. _interpreted text role: ../ref/rst/restructuredtext.html#interpreted-text

section_enumerator_separator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The separator between section number prefix and enumerator for
compound enumerated lists (see `compound_enumerators`_).

Generally it isn't recommended to use both, numbered sub-sections and
nested enumerated lists with compound enumerators.  This setting avoids
ambiguity in the situation where a section "1" has a list item
enumerated "1.1", and subsection "1.1" has list item "1".  With a
separator of ".", these both would translate into a final compound
enumerator of "1.1.1".  With a separator of "-", we get the
unambiguous "1-1.1" and "1.1-1".

*Default*: "-".  *Option*: ``--section-enumerator-separator``.

section_prefix_for_enumerators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Enable or disable section ("." subsection ...) prefixes for
compound enumerators.  This has no effect unless
`compound_enumerators`_ are enabled.

:Default: None (disabled).
:Options: ``--section-prefix-for-enumerators``,
  ``--no-section-prefix-for-enumerators``.


.. _stylesheet [latex writers]:

stylesheet
~~~~~~~~~~
List of style files (comma-separated_). Used verbatim
(under Windows, path separators are normalized to forward slashes).
Overrides also stylesheet_path__. [#override]_
See also `stylesheet [html writers]`_.

If `embed_stylesheet`__ is False (default), the stylesheet files are
referenced with ``\usepackage`` (values with extension ``.sty`` or no
extension) or ``\input`` (any other extension).
LaTeX will search the specified files in the `TeX input path`_.

*Default*: empty list.  *Option*: ``--stylesheet``.

__ `stylesheet_path [latex writers]`_
__ `embed_stylesheet [latex writers]`_
.. _TeX input path: https://texfaq.org/FAQ-tds


.. _stylesheet_dirs [latex writers]:

stylesheet_dirs
~~~~~~~~~~~~~~~
List of directories where stylesheets can be found (comma-separated_).
Used by the stylesheet_path__ setting.

Note: This setting defines a "search path" (similar to the PATH variable
for executables). However, the term "path" is already used in the
stylesheet_path__ setting with the meaning of a file location.

:Default: the working directory of the process at launch.
:Option:  ``--stylesheet-dirs``.

__
__ `stylesheet_path [latex writers]`_


.. _stylesheet_path [latex writers]:

stylesheet_path
~~~~~~~~~~~~~~~
List of style files (comma-separated_). Relative paths are expanded if a
matching file is found in the stylesheet_dirs__.
If embed_stylesheet__ is False, paths are rewritten relative to
the output file (if output_ or `\<destination>`_ are specified)
or the current work directory.
Overrides also stylesheet__. [#override]_
See also `stylesheet_path [html writers]`_.

For files in the `TeX input path`_, the stylesheet__  option is recommended.

*Default*: empty list.  *Option*: ``--stylesheet-path``.

.. _<destination>: tools.html#usage-pattern
__ `stylesheet_dirs [latex writers]`_
__ `embed_stylesheet [latex writers]`_
__
__ `stylesheet [latex writers]`_


.. _table_style [latex writers]:

table_style
~~~~~~~~~~~
Specify the default style for tables_.
See also `table_style [html writers]`_.

Supported values: "booktabs", "borderless", "colwidths-auto", and "standard".
See `Generating LaTeX with Docutils`__ for details.

*Default*: "standard".  *Option*: ``--table-style``.

__ latex.html#table-style


.. _template [latex writers]:

template
~~~~~~~~
Path [#pwd]_ to template file, which must be encoded in UTF-8.
See also `template [html writers]`_.

:Default: writer dependent (see `[latex2e writer]`_, `[xetex writer]`_).
:Option:  ``--template``.

use_bibtex
~~~~~~~~~~

List of style and database(s) for the experimental `BibTeX` support
(comma-separated_). Example::

  --use-bibtex=unsrt,mydb1,mydb2

.. admonition:: Provisional

   Name, values, and behaviour may change in future versions or the
   option may be removed.

*Default*: empty list (don't use BibTeX).  *Option* ``--use-bibtex``.

use_latex_abstract
~~~~~~~~~~~~~~~~~~
Use LaTeX abstract environment for the document's abstract_.

*Default*: False.
*Options*: ``--use-latex-abstract``, ``--topic-abstract``.

use_latex_citations
~~~~~~~~~~~~~~~~~~~
Use ``\cite`` for citations_.

:Default: False (use simulation with figure-floats).
          The default will change in Docutils 1.0.
:Options: ``--use-latex-citations``, ``--figure-citations``.

use_latex_docinfo
~~~~~~~~~~~~~~~~~
Attach author and date to the `document title`_.

:Default: False (attach author and date to the `bibliographic fields`_).
:Options: ``--use-latex-docinfo``, ``--use-docutils-docinfo``.

use_latex_toc
~~~~~~~~~~~~~
Let LaTeX generate the `table of contents`_. Generates a ToC with page numbers.
Usually LaTeX must be run twice to get the numbers correct.

*Default*: True.  *Options*: ``--use-latex-toc``, ``--use-docutils-toc``.

use_part_section
~~~~~~~~~~~~~~~~
Add parts on top of the section hierarchy.

*Default*: False.  *Option*: ``--use-part-section``.

[latex2e writer]
~~~~~~~~~~~~~~~~
The `LaTeX2e writer`_ generates a LaTeX source suited for compilation
with 8-bit LaTeX (pdfTeX_). It shares all settings defined in the `[latex
writers]`_ `configuration section`_.

.. _LaTeX2e writer: latex.html#latex2e-writer
.. _pdfTeX: https://www.tug.org/applications/pdftex/


Writer Specific Defaults
""""""""""""""""""""""""
.. class:: run-in narrow

:latex_preamble_: Load the "PDF standard fonts" (Times, Helvetica, Courier)::

    \usepackage{mathptmx} % Times
    \usepackage[scaled=.90]{helvet}
    \usepackage{courier}

:template__:
  "default.tex" in the ``docutils/writers/latex2e/`` directory
  (installed automatically).

  __ `template [latex writers]`_


font_encoding
"""""""""""""
String with `LaTeX font encoding`_.  Multiple encodings can be specified
separated by commas. The last value becomes the document default.

*Default*: "T1".  *Option*: ``--font-encoding``.

.. _LaTeX font encoding: latex.html#font-encoding


[xetex writer]
~~~~~~~~~~~~~~
The `XeTeX writer`_ generates a LaTeX source suited for compilation with
`XeTeX or LuaTeX`_. It derives from the latex2e writer, and shares all
settings defined in the `[latex writers]`_ `configuration section`_.

.. _XeTeX writer: latex.html#xetex-writer
.. _XeTeX or LuaTeX: https://texfaq.org/FAQ-xetex-luatex

Writer Specific Defaults
""""""""""""""""""""""""
.. class:: run-in narrow

:latex_preamble_:  Font setup for `Linux Libertine`_,::

      % Linux Libertine (free, wide coverage, not only for Linux)
      \setmainfont{Linux Libertine O}
      \setsansfont{Linux Biolinum O}
      \setmonofont[HyphenChar=None]{DejaVu Sans Mono}

  The optional argument ``HyphenChar=None`` to the monospace font
  prevents word hyphenation in literal text.

:template__: "xelatex.tex" in the ``docutils/writers/latex2e/`` directory
             (installed automatically).

             .. TODO: show full path with ``--help`` (like in the HTML
                writers) and add the following line: for the exact
                machine-specific path, use the ``--help`` option).

.. _Linux Libertine: http://www.linuxlibertine.org/
__ `template [latex writers]`_


.. _ODF/ODT Writer:

[odf_odt writer]
----------------

The ODF/`ODT Writer`_ generates documents in the
OpenDocument_ Text format (.odt).

The output_encoding_ setting is ignored, the output encoding is
always "UTF-8".

.. _ODT Writer: odt.html
.. _OpenDocument: https://en.wikipedia.org/wiki/OpenDocument

add-syntax_highlighting
~~~~~~~~~~~~~~~~~~~~~~~
Add syntax highlighting in literal code blocks.
See section "`Syntax highlighting`__" in the ODT Writer documentation
for details.

:Default: False.
:Options: ``--add-syntax-highlighting``, ``--no-syntax-highlighting``.

__ odt.html#syntax-highlighting

create_links
~~~~~~~~~~~~
Create links.

*Default*: False.
*Options*: ``--create-links``, ``--no-links``.

create_sections
~~~~~~~~~~~~~~~
Create sections for headers.

:Default: True.
:Options: ``--create-sections``, ``--no-sections``.

cloak_email_addresses
~~~~~~~~~~~~~~~~~~~~~
Obfuscate email addresses to confuse harvesters while still
keeping email links usable with standards-compliant browsers.

:Default: False.
:Options: ``--cloak-email-addresses``, ``--no-cloak-email-addresses``.

custom_header
~~~~~~~~~~~~~
Specify the contents of a custom header line.
See section "`Custom header/footers`_" in the ODT Writer documentation
for details.

*Default*: "" (no header).
*Option*: ``--custom-odt-header``.

custom_footer
~~~~~~~~~~~~~
Specify the contents of a custom footer line.
See section "`Custom header/footers`_" in the ODT Writer documentation
for details.

*Default*: "" (no footer).
*Option*: ``--custom-odt-footer``.

.. _custom header/footers:
    odt.html#custom-header-footers-inserting-page-numbers-date-time-etc

endnotes_end_doc
~~~~~~~~~~~~~~~~
Generate endnotes at end of document, not footnotes at bottom of page.

:Default: False.
:Options: ``--endnotes-end-doc``, ``--no-endnotes-end-doc``.

generate_oowriter_toc
~~~~~~~~~~~~~~~~~~~~~
Generate a native ODF table of contents, not a bullet list.
See section "`Table of contents`__" in the ODT Writer documentation
for details.

:Default: True.
:Options: ``--generate-oowriter-toc``, ``--generate-list-toc``.

__ odt.html#table-of-contents

odf_config_file
~~~~~~~~~~~~~~~
Path [#pwd]_ to a configuration/mapping file for additional ODF options.
In particular, this file may contain a mapping of default style names to
names used in the resulting output file.
See section `How to use custom style names`__ in the
ODT Writer documentation for details.

*Default*: None.
*Option*: ``--odf-config-file``.

__ odt.html#how-to-use-custom-style-names

stylesheet
~~~~~~~~~~
Path [#pwd]_ to a style file.  See section `Styles and Classes`_
in the ODT Writer documentation for details.

:Default: "writers/odf_odt/styles.odt" in the installation directory.
:Option:  ``--stylesheet``.

.. _styles and classes: odt.html#styles-and-classes

table_border_thickness
~~~~~~~~~~~~~~~~~~~~~~
Specify the thickness of table borders in thousands of a centimetre.
The `Table styles`__ section in the ODT Writer documentation describes
alternatives for additional customisation of the table style.

*Default*: 35 (0.35 mm).
*Option*: ``--table-border-thickness``.

__ odt.html#table-styles


[pseudoxml writer]
------------------

detailed
~~~~~~~~~
Pretty-print <#text> nodes.

*Default*: False.  *Option*: ``--detailed``.


[applications]
==============

Some `front end tools`_ provide additional settings.


.. _buildhtml:

[buildhtml application]
-----------------------

buildhtml.py_ generates HTML documents from reStructuredText source
files in a set of directories and their subdirectories.
All visited directories are scanned for "docutils.conf" files which are
parsed after the standard configuration files. Path settings [#pwd]_ in
these files are resolved relative to the respective directory.

The output_ setting is ignored.

dry_run
~~~~~~~
Do not process files, show files that would be processed.

*Default*: False (process files).  *Option*: ``--dry-run``.

ignore
~~~~~~
List of glob-style patterns [#globbing]_ (colon-separated_).
Source files with matching filename are silently ignored.
Values are appended. [#append-values]_

*Default*: empty list.  *Option*: ``--ignore``.

prune
~~~~~
List of glob-style patterns [#globbing]_ (colon-separated_).
Matching directories are skipped. Values are appended. [#append-values]_

Patterns are expanded similar to path settings [#pwd]_ and matched
against the absolute path of to-be-processed directories.
Example: a directory is pruned if it contains a "docutils.conf" file
with the lines ::

  [buildhtml application]
  prune: '.'

The default patterns skip auxiliary directories from Python or
popular version control tools anywhere [#]_.

:Default: ``/*/.hg:/*/.bzr:/*/.git:/*/.svn:/*/.venv:/*/__pycache__``.
:Option:  ``--prune``.

.. [#] The leading "/" prevents expansion with `pwd`;
       ``fnmatch('/*')`` matches any absolute path.

recurse
~~~~~~~
Recursively scan subdirectories.

*Default*: True.  *Options*: ``--recurse``, ``--local``.

sources
~~~~~~~
List of glob-style [#globbing]_ patterns (colon-separated_).
Files with matching filename are treated as source documents.
Values in configuration files overwrite the default and are
overwritten by the command line option.

*Default*: ``*.rst:*.rst``.  *Option*: ``--rst-sources``.

New in Docutils 0.21.

silent
~~~~~~
Work silently (no progress messages).
Independent of report_level_.

*Default*: None (show progress).  *Option*: ``--silent``.

.. _html_writer:
.. _writer [buildhtml application]:

writer
~~~~~~
`HTML writer`_ version. One of "html", "html4", "html5".

:Default: "html" (use Docutils' `default HTML writer`_).
:Option:  ``--writer``

New in 0.17. Obsoletes the ``html_writer`` option.

.. _HTML writer: html.html
.. _default HTML writer: html.html#html

.. [#globbing] Pattern matching is done with the `fnmatch module`_.
   It resembles shell-style globbing, but without special treatment
   of path separators and '.' (in contrast__ to the `glob module`_ and
   `pathlib.PurePath.match()`_).
   For example, "``/*.py``" matches "/a/b/c.py".

   Provisional: may use `pathlib.PurePath.match()` once this supports "**".

.. _fnmatch module:
    https://docs.python.org/3/library/fnmatch.html#module-fnmatch
.. _glob module:
    https://docs.python.org/3/library/glob.html#module-glob
.. _pathlib.PurePath.match():
    https://docs.python.org/3/library/pathlib.html#pathlib.PurePath.match
__ https://github.com/python/cpython/issues/106747


[docutils application]
--------------------------

Docutils' `generic front end`_ tool allows combining “reader”, “parser”,
and “writer” components from the Docutils package or 3rd party plug-ins.

| New in 0.17. Config file support added in 0.18. Renamed in 0.19
  (the old section name "docutils-cli application" is kept as alias).
| Support for reader/parser import names added in 0.19.

.. _generic front end: tools.html#generic-command-line-front-end

reader
~~~~~~
Reader component name.
One of "standalone", "pep", or the import name of a plug-in reader module.

*Default*: "standalone".
*Option*: ``--reader``

parser
~~~~~~
Parser component name.
Either "`rst <[restructuredtext parser]_>`__", "`xml <[xml parser]_>`__",
or the import name of a plug-in parser module.

Parsers for CommonMark_ known to work with Docutils include
"`pycmark <[pycmark parser]_>`__", "`myst <[myst parser]_>`__",
and "`recommonmark <[recommonmark parser]_>`__".

*Default*: "rst".
*Option*: ``--parser``

.. _CommonMark: https://spec.commonmark.org/current/


.. _writer [docutils application]:

writer
~~~~~~
Writer component name.
One of "html", "html4", "html5", "latex", "xelatex", "odt", "xml",
"pseudoxml", "manpage", "pep_html", "s5", an alias,
or the import name of a plug-in writer module.

*Default*: "html5".
*Option*: ``--writer``


Other Settings
==============

Command-Line Only
-----------------

These settings are only effective as command-line options; setting
them in configuration files has no effect.

config
~~~~~~
Path to an additional configuration file.
The file is processed immediately (if it exists) with
settings overriding defaults and earlier settings.

Filesystem path settings [#pwd]_ contained within the config file will be
interpreted relative to the config file's location (*not* relative to the
current working directory).

Multiple ``--config`` options may be specified;
each will be processed in turn.

*Default*: None.  *Option*: ``--config``.


Internal Settings
-----------------

These settings are for internal use only; setting them in
configuration files has no effect, and there are no corresponding
command-line options.

_config_files
~~~~~~~~~~~~~
List of paths of applied configuration files.

*Default*: None.  No command-line options.

_directories
~~~~~~~~~~~~
(``buildhtml.py`` front end.)  List of paths to source
directories, set from positional arguments.

*Default*: None (current working directory).  No command-line options.

_disable_config
~~~~~~~~~~~~~~~
Prevent standard configuration files from being read.
For command-line use, set the DOCUTILSCONFIG_ variable.

:Default: None (config files enabled).  No command-line options.

_destination
~~~~~~~~~~~~
Path to output destination, set from positional arguments.

*Default*: None (stdout).  No command-line options.

_source
~~~~~~~
Path to input source, set from positional arguments.

*Default*: None (stdin).  No command-line options.

--------------------------------------------------------------------------

.. [#override] The overridden setting will automatically be set to
   ``None`` for command-line options and config file settings.  Client
   programs which specify defaults that override other settings must
   do the overriding explicitly, by assigning ``None`` to the other
   settings.

.. [#append-values] Some settings append values from the various
   sources to a list instead of overriding previous values.
   The corresponding command line options may be used more than once.

.. [#encodings] Docutils supports all `standard encodings`_ and encodings
   registered__ with the codecs_ module.

.. [#pwd] Filesystem path relative to the working directory of the
   process at launch.
   Exception: Path settings in configuration files specified by the
   config_ command line option or in directories visited by the
   buildhtml_ application are resolved relative to the directory of
   the respective configuration file.

__ https://docs.python.org/3/library/codecs.html#codecs.register


Old-Format Configuration Files
==============================

Formerly, Docutils configuration files contained a single "[options]"
section only.  This was found to be inflexible, and in August 2003
Docutils adopted the current component-based configuration file
sections as described above.
Up to version 2.0, Docutils will still recognize the old "[options]"
section, but complain with a deprecation warning.

To convert existing config files, the easiest way is to change the
section title: change "[options]" to "[general]".  Most settings
haven't changed.  The only ones to watch out for are these:

=====================  =====================================
Old-Format Setting     New Section & Setting
=====================  =====================================
pep_stylesheet         [pep_html writer] stylesheet
pep_stylesheet_path    [pep_html writer] stylesheet_path
pep_template           [pep_html writer] template
=====================  =====================================


.. _auto-detect:

Input Encoding Auto-Detection
=============================

Up to Docutils 0.21, the input_encoding_ default value was ``None`` and
the actual input encoding detected from a `Unicode byte order mark` (BOM_)
or an `encoding declaration`_ in the source.

The default input encoding changed to "utf-8" in Docutils 0.22.
Currently, auto-detection can be selected with an input_encoding_ value
``None`` (rsp. an empty string in a configuration file).
However, **this feature** is deprecated and **will be removed** in
Docutils 1.0.  See the `inspecting_codecs`_ package for a replacement.

Encoding Declaration
--------------------

Input encoding auto-detection scans the source for an
*encoding declaration* inspired by :PEP:`263`:

A comment like ::

  .. text encoding: <encoding name>

on the first or second line defines `<encoding name>`
as the source's input encoding.

Examples: (using formats recognized by popular editors) ::

    .. -*- mode: rst -*-
       -*- coding: latin1 -*-

or::

    .. vim: set fileencoding=cp737 :

More precisely, the first and second line are searched for the following
regular expression::

    coding[:=]\s*([-\w.]+)

The first group of this expression is then interpreted as encoding name.
If the first line matches the second line is ignored.


.. References

.. _Docutils Document Tree:
.. _Document Tree: ../ref/doctree.html

.. _Docutils Runtime Settings:
.. _runtime settings: ../api/runtime-settings.html

.. _Publisher Interface: ../api/publisher.html
.. _publish_string(): ../api/publisher.html#publish-string
.. _publish_from_doctree(): ../api/publisher.html#publish-from-doctree

.. RestructuredText Directives
.. _"class" directive: ../ref/rst/directives.html#class
.. _"code": ../ref/rst/directives.html#code
.. _"csv-table": ../ref/rst/directives.html#csv-table
.. _"image" directive: ../ref/rst/directives.html#image
.. _"include": ../ref/rst/directives.html#include
.. _"math" directive: ../ref/rst/directives.html#math
.. _"parsed-literal": ../ref/rst/directives.html#parsed-literal
.. _"raw":
.. _"raw" directive: ../ref/rst/directives.html#raw
.. _"sectnum" directive: ../ref/rst/directives.html#sectnum
.. _"substitution": ../ref/rst/directives.html#substitution
.. _"table" directive: ../ref/rst/directives.html#table
.. _"title" directive: ../ref/rst/directives.html#metadata-document-title
.. _table of contents: ../ref/rst/directives.html#table-of-contents

.. RestructuredText Markup Specification
.. _auto-symbol footnotes:
    ../ref/rst/restructuredtext.html#auto-symbol-footnotes
.. _abstract:
.. _bibliographic field list:
.. _bibliographic fields:
    ../ref/rst/restructuredtext.html#bibliographic-fields
.. _block quote: ../ref/rst/restructuredtext.html#block-quotes
.. _bullet lists: ../ref/rst/restructuredtext.html#bullet-lists
.. _citations: ../ref/rst/restructuredtext.html#citations
.. _document title: ../ref/rst/restructuredtext.html#document-title
.. _enumerated lists: ../ref/rst/restructuredtext.html#enumerated-lists
.. _field lists: ../ref/rst/restructuredtext.html#field-lists
.. _field names: ../ref/rst/restructuredtext.html#field-names
.. _footnotes: ../ref/rst/restructuredtext.html#footnotes
.. _footnote references: ../ref/rst/restructuredtext.html#footnote-references
.. _inline markup recognition rules:
    ../ref/rst/restructuredtext.html#inline-markup-recognition-rules
.. _literal blocks: ../ref/rst/restructuredtext.html#literal-blocks
.. _option lists: ../ref/rst/restructuredtext.html#option-lists
.. _tables: ../ref/rst/restructuredtext.html#tables

.. _front end tools: tools.html
.. _buildhtml.py: tools.html#buildhtml-py

.. _BCP 47: https://www.rfc-editor.org/rfc/bcp/bcp47.txt
.. _Error Handlers:
    https://docs.python.org/3/library/codecs.html#error-handlers
.. _ISO 639: http://www.loc.gov/standards/iso639-2/php/English_list.php
.. _ISO 3166: http://www.iso.ch/iso/en/prods-services/iso3166ma/
    02iso-3166-code-lists/index.html
.. _language tag: https://www.w3.org/International/articles/language-tags/

.. _codecs: https://docs.python.org/3/library/codecs.html
.. _standard encodings:
    https://docs.python.org/3/library/codecs.html#standard-encodings
.. _BOM: https://docs.python.org/3/library/codecs.html#codecs.BOM
.. _inspecting_codecs: https://codeberg.org/milde/inspecting-codecs
