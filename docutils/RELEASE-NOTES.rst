.. include:: docs/header0.rst

========================
 Docutils Release Notes
========================

:Contact: grubert@users.sourceforge.net
:Maintainer: docutils-develop@lists.sourceforge.net
:Date: $Date$
:Revision: $Revision$
:Web site: https://docutils.sourceforge.io/
:Copyright: This document has been placed in the public domain.


This document summarizes the major changes in previous and upcoming releases.
For a more detailed list of changes, please see the Docutils `HISTORY`_.

.. contents::

Future changes
==============

Command line interface
----------------------

* The _`command-line usage pattern` will change:

  .. code:: diff

       - COMMAND [OPTIONS] [SOURCE [DESTINATION]]
       + COMMAND [OPTIONS] [SOURCE [SOURCE2 [...]]]

  * Stop accepting the DESTINATION positional argument in Docutils 1.0.
    Use ``--output=DESTINATION`` (cf. the "output_path_" configuration setting)
    or output redirection.

  * Accept the short option ``-o`` for ``--output`` in Docutils 1.0

  * Accept more than one source document in Docutils 2.0

  For the rationale, see https://clig.dev/#arguments-and-flags.

* The `front end tools`_ will use argparse_ for command line parsing
  in Docutils 2.0 or later.

  .. _argparse: https://docs.python.org/3/library/argparse.html

Document Tree / Docutils DTD
----------------------------

* Drop the ``name`` attribute from <reference> nodes in Docutils 1.0.

* Use the ``%tbl.table.att`` parameter entity instead of ``%bodyatt``
  to customize the <table> element's attribute list in Docutils 1.0.

* The <footnote> element's first child (<label>) will become mandatory
  in Docutils 1.0.

* The "rst" parser will warn if a `"figure"`_ directive is missing both
  caption and legend in Docutils 1.0.

* The "rst" parser will use <inline> elements for inline targets
  in Docutils 1.0.

* <target> elements with content will be deprecated in Docutils 1.0
  and invalid in Docutils 2.0.

* To match the definition in the "Exchange Table Model", the
  `"colwidth" attribute`_ will be stored as a `str` (instead of
  numerical) value in Python element instances in Docutils 1.0.
  Proportional values will be stored with unit "*" in Docutils 2.0.
  The default unit will change to "pt" in Docutils 3.0.

* The `\<doctest_block>`_ element will be deprecated in Docutils 1.0.
  The rST parser will handle a `doctest block`_ similar to a "code" directive
  with language "pycon" (Python console) and generate a <literal_block>.

Writers
-------

* The `default HTML writer`__  will change in Docutils 2.0:

  The rst2html_ front end and ``get_writer_by_name('html')`` select
  "html4css1" now and will select "html5" in Docutils 2.0 and later.

  - Use rst2html4_, ``docutils --writer=html4``, or
    ``get_writer_by_name('html4')`` if you depend on stability of the
    generated HTML code, e.g. because you use a custom style sheet or
    post-processing that may break otherwise.
  - Use rst2html5_, ``docutils`` or ``get_writer_by_name('html5')``
    if you want a HTML5 document.

  __ docs/user/html.html#html

* "html5" writer:

  - Move attribution behind the blockquote to comply with the
    `"HTML living standard"`__ [#]_ and adapt CSS stylesheets
    in Docutils 1.0.

    __ https://html.spec.whatwg.org/#the-blockquote-element

  - Change the default value of the initial_header_level_ setting to None
    (<h2> if there is a document title, else <h1>) in Docutils 1.0.

  - Remove option ``--embed-images`` (obsoleted by "image_loading_")
    in Docutils 2.0.

* "latex2e" writer:

  - Change default of use_latex_citations_ setting to True
    in Docutils 1.0.

  - Change default of legacy_column_widths_ setting to False
    in Docutils 1.0.

  - Remove ``use_verbatim_when_possible`` setting
    (use literal_block_env_: verbatim) in Docutils 2.0.

  - The `default length unit`__ will change from "bp" (DTP point)
    to "px" (pixel unit) in Docutils 1.0.

    __ docs/user/latex.html#length-units

* "manpage" writer:

  - Change default of the text_references_ setting to False in Docutils 1.0.

* "odt" writer:

  - Align adjustment of relative image paths with the behaviour of HTML
    and LaTeX writers: assume them relative to the *output* directory (as
    required for image references in HTML), not the *source* directory.

.. [#] The now retired `HTML5 W3C recommendation`__ allows <cite> elements
   inside a blockquote.

__ https://www.w3.org/TR/2014/REC-html5-20141028/grouping-content.html
   #the-blockquote-element

Removals
--------

* Remove `io.BinaryFileOutput` and `core.publish_cmdline_to_binary()`
  in Docutils 0.24.

* Remove `writers.latex2e.SortableDict` in Docutils 0.24.

* Remove `parsers.rst.directives.length_units` in Docutils 0.24.
  Use `parsers.rst.directives.CSS3_LENGTH_UNITS`.  Mind that this
  is a tuple, not a list.

* Remove the "name" argument from
  `writers.latex2e.LaTeXTranslator.visit_docinfo_item()`
  (ignored since Docutils 0.22) in Docutils 0.24.

* Remove `parsers.rst.directives.CSVTable.HeaderDialect`
  in Docutils 1.0.

* Remove `utils.decode_path()` and `utils.get_stylesheet_reference()`
  in Docutils 1.0.

* Remove support for the `recommonmark parser`_ in Docutils 1.0.
  Recommonmark is unmaintained since 2021 and deprecated in favour
  of the `MyST parser`_.

  .. _recommonmark parser: docs/user/config.html#recommonmark-parser
  .. _MyST parser: docs/user/config.html#myst-parser

* Remove the input_encoding_ auto-detection code in Docutils 1.0.

* Remove the "TransformSpec.unknown_reference_resolvers" hook chain
  in Docutils 1.0.  Use a transform_,
  see `transforms.references.CitationReferences` for an example.

* Remove the internal attribute `nodes.Targetable.indirect_reference_name`
  in Docutils 1.0. (Was required for MoinMoin <= 1.9.)

* Don't call `transforms.references.DanglingReferences` and
  `transforms.references.DanglingReferencesVisitor` in Docutils 1.0;
  remove them in Docutils 2.0. [rationale__]

  __ docs/api/transforms.html#dangling

* Remove `parsers.rst.roles.set_classes()` and
  `parsers.rst.roles.normalized_role_options()`
  (obsoleted by `parsers.rst.roles.normalize_options()`) in Docutils 2.0.

* Remove the "rawsource" argument from `nodes.Text.__init__()`
  in Docutils 2.0.

* Remove the internal attributes `nodes.Element.known_attributes`,
  `nodes.Element.basic_attributes`, and `nodes.Element.local_attributes`,
  in Docutils 2.0.

* Drop support for `old-format configuration files`_ in Docutils 2.0.

* Remove the ``--html-writer`` option of the `buildhtml.py`_ application
  (obsoleted by the `"writer" setting`_ since Docutils 0.18)
  in Docutils 2.0.

* Drop short option ``-e`` in Docutils 2.0.
  Use the long equivalent ``--error-encoding``.

* Remove the "reader_name", "parser_name", and "writer_name" arguments of
  `core.Publisher.__init__()` and the `core.publish_*()` convenience
  functions as well as the "parser_name" argument of `Reader.__init__()`
  in Docutils 2.0.  Since Docutils 0.22, you may use "reader", "parser",
  and "writer" arguments for component names as well as instances.

* Remove `states.RSTStateMachine.memo.reporter`,
  `states.RSTStateMachine.memo.section_bubble_up_kludge`,
  `states.RSTStateMachine.memo.section_level`,
  `states.RSTState.title_inconsistent()`, and `states.Line.eofcheck`
  in Docutils 2.0. Ignored since Docutils 0.22.

* Remove `parsers.rst.states.Struct` (obsoleted by `types.SimpleNamespace`)
  in Docutils 2.0.

* Ignore the "match_titles" argument of
  `parsers.rst.states.RSTState.nested_list_parse()` in Docutils 1.0;
  remove it in Docutils 2.0.

* Remove `frontend.OptionParser`, `frontend.Option`, `frontend.Values`,
  `frontend.store_multiple()`, and `frontend.read_config_file()` when
  migrating to argparse_ in Docutils 2.0 or later.

Misc
----

* Prefer explicit reference names as base for an HTML element's ID
  in Docutils 1.0. No change for internal cross-references.
  Cf. `Sphinx issue #1961`__

  __ https://github.com/sphinx-doc/sphinx/issues/1961

* Revise the `String I/O`__ interface used by the `publish_string()`
  and `publish_from_doctree()` publisher convenience functions.
  (In Python 3, name and behaviour no longer match.)

  __ docs/api/publisher.html#string-i-o

* Change the default priority of the `universal.SmartQuotes` transform_
  from 855 (very late) to 510 (main) in Docutils 1.0.

* Move math format conversion from docutils/utils/math (called from
  docutils/writers/_html_base.py) to a transform_.

* If the environment variable `SOURCE_DATE_EPOCH`_ is set, the `"date"`_
  directive and the timestamp inserted by the "datestamp_"
  configuration setting will use its value instead of the current time to
  support `reproducible builds`_ in Docutils 1.0. [#]_

  .. _SOURCE_DATE_EPOCH:
      https://reproducible-builds.org/docs/source-date-epoch/
  .. _reproducible builds: https://reproducible-builds.org/

.. [#] The `Debian package`__ python-docutils (0.21.2+dfsg-2)
   contains a patch to implement this behaviour since May 2023.

   __ https://packages.debian.org/source/trixie/python-docutils


Release 0.22.1b2.dev (unpublished)
==================================

Hopefully 0.22.1 will be next.


Release 0.22.1rc1 (2025-09-13)
==============================

* docutils/parsers/rst/states.py

  - Relax "section title" system messages from SEVERE to ERROR.
  - Fix behaviour with nested parsing into a detached node
    (cf. bugs #508 and #509).
  - New attribute `NestedStateMachine.parent_state_machine`.
    Use case: update the "current node" of parent state machine(s)
    after nested parsing.
  - Better error messages for grid table markup errors (bug #504),
    based on patch #214 by Jynn Nelson.

* docutils/writers/latex2e/__init__.py

  - Add cross-reference anchors (``\phantomsection\label{...}``)
    for elements with IDs (fixes bug #503).
  - Fix cross-reference anchor placement in figures, images,
    literal-blocks, tables, and (sub)titles.

and improvements (see HISTORY_).


Release 0.22 (2025-07-29)
=========================

reStructuredText:
  - Support `CSS3 units`_. This adds "ch", "rem", "vw", "vh", "vmin",
    "vmax", and "Q" to the `supported length units`__. Note that some
    output formats don't support all units.
  - New option "figname" for the `"figure"`_ directive.
  - Targets generated from hyperlink references with embedded URI or
    alias are no longer "explicit" but "implicit" (i.e. with the same
    priority as auto-generated section targets, see `implicit hyperlink
    targets`__).
  - Don't report an error for duplicate targets with identical refname.
  - Drop the "name" option of the "target-notes" directive.
    (Report an error instead of silently ignoring the value.)
  - New alias "rst-class" for the `"class"`_ directive to improve the
    compatibility with Sphinx.

  .. _CSS3 units: https://www.w3.org/TR/css-values-3/#lengths
  __ docs/ref/rst/restructuredtext.html#length-units
  __ https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html
     #implicit-hyperlink-targets

Document Tree / Docutils DTD
  - Allow multiple <term> elements in a `\<definition_list_item>`__
    (third-party writers may need adaption).
  - The first element in a <figure> may also be a <reference>
    (with nested "clickable" <image>).

  __ docs/ref/doctree.html#definition-list-item

Configuration changes
  - Make MathML the default math_output_ for the "html5" writer.
  - Change the default input_encoding_ from ``None`` (auto-detect) to "utf-8".
  - Drop short options ``-i`` and ``-o``.
    Use the long equivalents ``--input-encoding`` and ``--output-encoding``.
    (See `command line interface`_ for the rationale.)
  - Rename configuration setting "output" to "output_path_".
  - New setting "validate_".
  - The manpage writer now recognizes the sections [writers] and
    [manpage writer] with the new setting `text_references`_.

Output changes
  LaTeX:
     Don't wrap references with custom reference_label_ in a ``\hyperref``
     command. The "hyperref" package generates hyperlinks for labels by
     default, so there is no change in the PDF
     (except for the starred forms like ``reference_label = \ref*``).

     Stop requiring "ifthen.sty". Add "ifthen" to the stylesheet__ setting
     or replace use of ``\ifthenelse{\isundefined...`` with the eTeX
     primitive ``\ifdefined``.

     __ docs/user/config.html#stylesheet-2

  HTML5:
     Unitless image_ size measures__ are written as <img> "width" and
     "hight" values instead of "style" rules.  The current behaviour
     is kept for values with units, so users may specify, e.g. ``:width:
     50px`` instead of ``:width: 50`` to override CSS stylesheet rules.

     __ docs/ref/doctree.html#measure

  manpage:
     Don't UPPERCASE section headings.

     Handle hyperlink references (see the text_references_ setting).

  null:
     The "null" writer output changed from None to the empty string.

     `publish_string()` now returns a `bytes` or `str` instance
     for all writers (as documented).

New objects
  `parsers.docutils_xml`
     parser for `Docutils XML`_ (e.g., the output of the "xml" writer).
     Provisional.

     Try ``docutils --parser=xml test/data/multiple-term-definitions.xml``
     or use the :parser: option of the `"include"`_ directive to include
     an XML file in a rST document.

  `nodes.Element.validate()`
     Raise `nodes.ValidationError` if the element does not comply with
     the `Docutils Document Model`_.
     Provisional.

  `transforms.references.CitationReferences`
     Mark citation_references as resolved if the backend
     uses a BibTeX database.

  `writers.DoctreeTranslator`
     Generic Docutils document tree translator base class with
     `uri2path()` auxiliary method.
     Provisional.

Removed objects
  `core.Publisher.setup_option_parser()`
     internal, obsolete,
  `frontend.ConfigParser.get_section()`
     obsoleted by the configparser's "Mapping Protocol Access",
  `frontend.OptionParser.set_defaults_from_dict()`
     obsolete,
  `nodes.Element.set_class()`
     obsolete, append to Element['classes'] directly,
  `parsers.rst.directives.tables.CSVTable.decode_from_csv()`
     not required with Python 3,
  `parsers.rst.directives.tables.CSVTable.encode_from_csv()`
     not required with Python 3,
  `transforms.writer_aux.Compound`
     not used since Dec 2010,
  `utils.error_reporting`
     obsolete in Python 3,
  `utils.Reporter.set_conditions()`
     obsolete, set attributes via configuration settings or directly.

Removed localisations
  Mistranslations of the "admonition" directive name:
     Use "advies" (af), "varsel" (da), "warnhinweis" (de), "aviso" (es),
     "sciigo" (eo), "annonce" (fr), "avviso" (it), "advies" (nl),
     "zauważenie" (pl) (introduced in Docutils 0.21)
     or the English name "admonition".

New files
  ``docutils/parsers/rst/include/html-roles.txt``
     `Standard definition file`_ for additional roles matching HTML tags.

Removed files
  ``tools/rst2odt_prepstyles.py``
     Obsoleted by `writers.odf_odt.prepstyles`.
  ``docutils/utils/roman.py``
     Obsoleted by ``docutils/utils/_roman_numerals.py``

Bugfixes and improvements (see HISTORY_).


Release 0.21.2 (2024-04-23)
===========================

* Document Tree / Docutils DTD

  - Remove declaration of unsupported element <info>.
  - Remove <decoration> from content declaration of <section> elements.

* Declare support for languages Georgian and Catalan (Valencian).

* Fix test failures.


Release 0.21.1 (2024-04-10)
===========================

Add missing metadata files to sdist.
No changes to the code.


Release 0.21 (2024-04-09)
=========================

* General:

  - Drop support for Python 3.7 and 3.8.

  - Provide ``rst2*`` "console_scripts" `entry points`_
    (without the ``.py`` extension) instead of installing the
    ``rst2*.py`` `front end tools`_ in the binary PATH. [#]_

    Exceptions: ``rstpep2html.py`` and ``rst2odt_prepstyles.py``:

    - Use ``docutils --reader=pep --writer=pep_html`` for a PEP preview. [#]_
    - Use ``python -m docutils.writers.odf_odt.prepstyles``
      to `strip the page size`__ from an ODT writer stylesheet.

  .. [#] Some Linux distributions already use the short names.
  .. [#] The final rendering is done by a Sphinx-based build system
         (cf. :PEP:`676`).

  .. _entry points:
      https://packaging.python.org/en/latest/specifications/entry-points/
  __ docs/user/odt.html#page-size

* reStructuredText:

  - Use the same CSV format for the ``:header:`` option and the main data
    of the "csv-table_" directive.

  - New option "loading" for the "image_" directive.
    Sets the new attribute loading__ of the <image> doctree element.

  __ docs/ref/doctree.html#loading

* Configuration changes:

  - New configuration setting root_prefix_.
    Configurable root directory for included files.

  - New configuration setting sources_ for the "buildhtml.py" application.

  - Simpler and more secure `input_encoding`_ default behaviour:

    Do not use the locale encoding as fallback if Python is started in
    `UTF-8 mode`_. Stop using "latin1" as second fallback.

    Remove BOM (U+FEFF ZWNBSP at start of data) only if the `input_encoding`_
    configuration setting is None, '', 'utf-8-sig', 'utf-16', or 'utf-32'.
    Do not remove other ZWNBSPs.

    .. _UTF-8 mode: https://docs.python.org/3/library/os.html#utf8-mode

* Output changes:

  HTML5:
    Stop setting the "footnote-reference" class value for footnote
    references. Use the CSS selector ``[role="doc-noteref"]``
    (works since Docutils 0.18, see minimal.css for examples).

    Fix MathML rendering problems in Chrome/Chromium based browsers.

    Embed SVG images as ``<svg>`` instead of data-URI.

  manpage:
    Use .EE/.EX macros for literal blocks.

    Render URI references (do not use .UR/.UE).

    Use box option for tables.

* Removed objects:

  `nodes.reprunicode`, `nodes.ensure_str()`
    Python 2 compatibility hacks
  `utils.Reporter.set_conditions()`
    obsolete
  `writers.latex2e.Table.get_caption`
    obsolete

* New files:

  ``docutils/writers/html5_polyglot/italic-field-names.css``
    Alternative style for Docutils field-lists.

* Removed files:

  ``install.py``, ``setup.py``
    Metadata is now stored in ``pyproject.toml``,
    supported by pip_ since version 19.0 (2019-01-22).
    See README__ for installation alternatives.

  __ README.html#installation

* Bugfixes and improvements (see HISTORY_).


Release 0.20.1 (2023-05-17)
===========================

Bugfix release. See HISTORY_ for details.


Release 0.20 (2023-05-04)
=========================

.. Note::

   Docutils 0.20 is the last version supporting Python 3.7 and 3.8.

* General

  - Support Python 3.11 (patch #198 by Hugo van Kemenade).

* Output changes:

  HTML5:
    Use dpub-ARIA role "doc-footnote" (instead of ARIA role "note")
    for footnotes.

  LaTeX:
    Do not load the `inputenc` package in UTF-8 encoded LaTeX sources.
    (UTF-8 is the default encoding for LaTeX2e since 2018).

* Configuration changes:

  - Settings in the [latex2e writer] configuration file section
    are now ignored by the "xetex" writer.
    Place common settings in section `[latex writers]`_.

  - New configuration setting "output_".  Obsoletes the ``<destination>``
    positional argument (cf. `future changes`__).

    __ `command-line usage pattern`_

* `utils.find_file_in_dirs()` now returns a POSIX path also on Windows;
  `utils.get_stylesheet_list()` no longer converts ``\`` to ``/``.

* docutils/languages/
  docutils/parsers/rst/languages/

  - Support Ukrainian. Patch by Dmytro Kazanzhy.

* test/coverage.sh

  - Removed. Use the coverage.py_ project instead,
    ``coverage run test/alltests.py`` and ``coverage report``.

    .. _coverage.py: https://pypi.org/project/coverage/

* tools/

  - Moved ``quicktest.py`` to ``tools/dev/``.

* Bugfixes and improvements (see HISTORY_).


Release 0.19 (2022-07-05)
=========================

* Drop support for Python 2.7, 3.5, and 3.6.

* Output changes:

  HTML5:
    Wrap groups of footnotes in an ``<aside>`` for easier styling.

    The CSS rule ``.footnote-list { display: contents; }`` can be used to
    restore the behaviour of custom CSS styles.

* After package installation, the CLI commands ``python -m docutils`` and
  ``docutils`` start the `generic command line front end tool`_.

* Support parsing "Markdown" input with 3rd party parsers
  myst_, pycmark_, or recommonmark_.

* The default values for the "pep-references", "rfc-base-url",
  and "python-home" `configuration settings`_ now use the "https:" scheme.
  The PEP-writer template's header is updated to fix links and
  resemble the header of official PEPs.

* Various bugfixes and improvements (see HISTORY_).

.. _myst: https://pypi.org/project/myst-docutils
.. _pycmark: https://pypi.org/project/pycmark/
.. _recommonmark: https://pypi.org/project/recommonmark/


Release 0.18.1 (2021-12-23)
===========================

.. Note::

   Docutils 0.18.1 is the last version supporting Python 2.7, 3.5, and 3.6.

* ``nodes.Node.traverse()`` returns a list again to restore backwards
  compatibility (fixes bug #431).
  Use ``nodes.Node.findall()`` to get an iterator.

* re-add module `parsers.rst.directives.html`
  (stub, emits deprecation warning and loads "Meta" directive
  from its new place at `parsers.rst.directives.misc`.)

* Small bugfixes (see HISTORY_).


Release 0.18 (2021-10-26)
=========================

* Output changes:

  Identifiers:
    - During `identifier normalization`_, leading number and hyphen
      characters are no longer stripped from a `reference name`_, if the
      id_prefix_ setting is non-empty.

      Example:
        with ``--id-prefix="DU-"``, a section with title "34. May"
        now gets the identifier key ``DU-34-may`` instead of ``DU-may``.

    - The default value for the auto_id_prefix_ setting changed to ``%``:
      "use the tag name as prefix for auto-generated IDs".
      Set auto_id_prefix_ to ``id`` for unchanged auto-IDs.

  HTML5:
    - Use the semantic tag <aside> for footnote text and citations, topics
      (except abstract and toc), admonitions, and system messages.
      Use <nav> for the Table of Contents.

    - Make "auto" table column widths the default: Only specify column
      widths, if the `"widths" option`_ is set and not "auto".
      The table-style__ setting "colwidths-grid" restores the current default.

      __ docs/user/config.html#table-style

    - Items of a definition list with class argument "details" are
      converted to `details disclosure elements`__. Example::

        ..class:: details

        Summary
          This additional information should be hidden.

      __ https://www.w3.org/TR/html52/interactive-elements.html
         #the-details-element

    - Do not add "compound-first", "compound-middle", or "compound-last" to
      elements nested in a compound. Use child selector and ":first-child",
      ":last-child" pseudo classes instead.

    - Use class value "backrefs" instead of "fn-backref" for a span of
      back-references.

    - Write footnote brackets and field term colons to HTML, so that they
      are present also without CSS and when copying text.

    - Move space character between section number and heading into
      "sectnum" span.

  `math_output`_: html
    - Support more commands, fix mapping of commands to Unicode characters.
    - Scale variable sized operators and big delimiters with CSS.
    - Don't use <tt> element (deprecated in HTML5).
    - Use STIX fonts if available.

  LaTeX:
     `legacy_class_functions`_ setting default changed to "False",
     admonitions are now environments.

* New standard Docutils doctree node: <meta__>.

  __ docs/ref/doctree.html#meta

* New configuration settings:

  - [latex writers] legacy_column_widths_ and
  - [html5 writer] image_loading_.

* Removed files:
  ``iepngfix.htc`` and ``blank.gif`` (IE 6 workaround for `s5_html`).

* Removed sub-module:
  `parsers.rst.directives.html` (reversed in release 0.18.1).

* Removed function: `utils.unique_combinations()`
  (obsoleted by `itertools.combinations()`).

* Removed attributes:

  - `HTMLTranslator.topic_classes`: check `node.parent.classes` instead.
  - `nodes.Text.rawsource`: we store the null-escaped text in Text
    nodes since 0.16 so there is no additional information in the
    rawsource.

* Major refactoring and fixes/additions in
  ``docutils/utils/math/math2html.py`` and
  ``docutils/utils/math/latex2mathml.py``
  (mathematical notation in HTML, cf. `LaTeX syntax for mathematics`_).

* nodes.Node.traverse() returns an iterator instead of a list
  (reversed in release 0.18.1).

* Various bugfixes and improvements (see HISTORY_).

  Fix spelling errors in documentation and docstrings.
  Thanks to Dimitri Papadopoulos.


Release 0.17.1 (2021-04-16)
===========================

* Bug fixes (for details see the Docutils `HISTORY`_).

Release 0.17 (2021-04-03)
=========================

* Numerous bug fixes and improvements
  (for details see the Docutils `HISTORY`_).

* Installing with ``setup.py`` now requires setuptools_.
  Alternatively, install with pip_.

* The generic command line front end tool docutils-cli.py_ allows
  the free selection of reader, parser, and writer components.

* Support Arabic language.

* New, **experimental** wrapper to integrate the `recommonmark`__
  Markdown parser for use with Docutils.
  Currently only tested with recommonmark version 0.4.0.

  __ https://pypi.org/project/recommonmark/

* HTML5 writer:

  - New option embed_images_.

  - Use semantic tags (for details see the Docutils `HISTORY`_).

  - Change the `initial_header_level`_ setting's default to "2", as browsers
    use the `same style for <h1> and <h2> when nested in a section`__.

    __ https://stackoverflow.com/questions/39547412/
       same-font-size-for-h1-and-h2-in-article

  - New optional style ``responsive.css``, adapts to different screen
    sizes.

  - Move non-essential styling from ``minimal.css`` to ``plain.css``
    rsp. ``responsive.css``.

  - Show code line numbers as pseudo-elements so they are skipped when
    copying the code block from the page.

* LaTeX writer:

  - New configuration setting `legacy_class_functions`_.

  - The special value "auto" for the `graphicx_option`_ setting
    is no longer supported (it never worked for xetex/luatex).

  - `Styling commands`__ using the legacy ``\docutilsrole`` prefix are
    now ignored. Use ``\DUrole``.

    __ docs/user/latex.html#classes

  - Most helper commands and element definitions are now defined in the
    LaTeX package `docutils.sty`_ and only inserted in the document
    preamble if the stylesheet__ setting does not lists "docutils".

    __ docs/user/config.html#stylesheet-latex-writers

  - Remove legacy LaTeX stylesheet ``docutils-05-compat.sty``.

.. _setuptools: https://pypi.org/project/setuptools/
.. _pip: https://pypi.org/project/pip/
.. _docutils.sty: https://ctan.org/pkg/docutils


Release 0.16 (2020-01-12)
=========================

Docutils 0.16.x supports Python 2.7 and Python >= 3.5 natively,
without the use of the ``2to3`` tool.

* reStructuredText:

  - Keep `backslash escapes`__ in the document tree. This allows, e.g.,
    escaping of author-separators in `bibliographic fields`__.

  __ https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html#escaping-mechanism
  __ docs/ref/rst/restructuredtext.html#bibliographic-fields

* LaTeX writer:

  - Informal titles of type "rubric" default to bold-italic and left aligned.
  - Deprecate ``\docutilsrole`` prefix for styling commands:
    use ``\DUrole`` instead.
  - Fix topic subtitle.
  - Add "latex writers" to the `config_section_dependencies`.
  - Ignore classes for `rubric` elements
    (class wrapper interferes with LaTeX formatting).

* tools/buildhtml.py

  - New option ``--html-writer`` allows to select "html" (default),
    "html4" or "html5" (deprecated in favour of the `"writer" setting`_
    in Docutils 0.18).

* docutils/io.py

  - Remove the `handle_io_errors` argument from io.FileInput/Output.

* docutils/nodes.py

  - If `auto_id_prefix`_ ends with "%", this is replaced with the tag name.

* Various bugfixes and improvements (see HISTORY_).


Release 0.15 (2019-07-20)
=========================

Docutils 0.15.x is compatible with Python versions 2.6, 2.7 and 3.3 to 3.5.

.. Note::

   Docutils 0.15.x is the last version supporting Python 2.6, 3.3 and 3.4.

* reStructuredText:

  - Allow embedded colons in field list field names (before, tokens like
    ``:this:example:`` were considered ordinary text).

  - Fixed a bug with the "trim" options of the "unicode" directive.

* languages: Added Korean localisation (ko).


Release 0.14 (2017-08-03)
=========================

.. Note::

   Docutils 0.14.x is the last version supporting Python 2.4, 2.5,
   3.1, and 3.2.

* docutils/docs/ref/docutils.dtd:

  - Enable validation of Docutils XML documents against the DTD.

* docutils/parsers/rst/:

  - Added functionality: escaped whitespace in URI contexts.
  - Consistent handling of all whitespace characters in inline markup
    recognition. (May break documents that relied on some whitespace
    characters (NBSP, ...) *not* to be recognized as whitespace.)

* docutils/utils/smartquotes.py:

  - Update quote definitions for et, fi, fr, ro, sv, tr, uk.
  - Add quote definitions for hr, hsb, hu, lv, sh, sl, sr.
  - Differentiate apostrophe from closing single quote (if possible).
  - Add command line interface for stand-alone use (requires 2.7).

* docutils/writers/_html_base:

  - Provide default title in metadata.
  - The MathJax CDN shut down on April 30, 2017. For security reasons, we
    don't use a third party public installation as default but warn
    if `math_output` is set to MathJax without specifying a URL.
    See math_output_ for details.

* docutils/writers/html4css1:

  - Respect automatic table column sizing.

* docutils/writers/latex2e/__init__.py

  - Handle class arguments for block-level elements by wrapping them
    in a "DUclass" environment. This replaces the special handling for
    "epigraph" and "topic" elements.

* docutils/writers/odf_odt:

  - Language option sets ODF document's default language
  - Image width, scale, ... set image size in generated ODF.

* tools/

  - New front-end ``rst2html4.py``.


Release 0.13.1 (2016-12-09)
===========================

* docutils/writers/html5_polyglot

  - New HTML writer generating `HTML 5`_.

* tools/

  - New front-end ``rst2html5.py``.

* languages: persian/farsi (fa) and latvian (la) mappings.

* change default base url for :rfc: to http://tools.ietf.org/html/

* tables accept widths, a list and align

* latex2e: Fix admonition width, remove deprecated options,
  better tablewidth auto, ...

* rst.el: The problem with ``electric-indent-mode`` has been fixed.

.. _HTML 5: https://www.w3.org/TR/html5/


Release 0.12 (2014-07-06)
=========================

Small changes only, release current state


Release 0.11 (2013-07-22)
=========================

* General

  - Apply [ 2714873 ] Fix for the overwriting of document attributes.
  - Support embedded aliases within hyperlink references.
  - Fix [ 228 ] try local import of docutils components (reader, writer, parser,
    language module) before global search.

* docutils/parsers/rst/directives/tables.py

  - Fix [ 210 ] Python 3.3 checks CVS syntax only if "strict" is True.

* docutils/writers/html4css1/__init__.py
  - Fix [ 3600051 ] for tables in a list, table cells are not compacted.
  - New setting `stylesheet_dirs` (see above).

    Now, it is easy to add a custom stylesheet to Docutils' default
    stylesheet with, e.g., ``--stylesheet_path='html4css1.css, mystyle.css'``

    Changed behaviour of the default settings:
      if there is a file ``html4css1.css`` in the working directory of the
      process at launch, it is used instead of the one provided by Docutils
      in the writer source directory.

  - New default for math_output_: ``HTML math.css``.
  - Avoid repeated class declarations in html4css1 writer
    (modified version of patch [ 104 ]).

* docutils/writers/latex2e/__init__.py

  - Drop the simple algorithm replacing straight double quotes with
    English typographic ones.
    Activate the SmartQuotes_ transform if you want this feature.
  - New setting `stylesheet_dirs`: Comma-separated list of directories
    where stylesheets are found. Used by `stylesheet_path` when expanding
    relative path arguments.

* docutils/writers/manpage.py

  - Fix [3607063] handle lines starting with a period.
  - Fix option separating comma was bold (thanks to Bill Morris).


Release 0.10 (2012-12-16)
=========================

Docutils 0.10 is compatible with Python versions from 2.4 to 3.2.

* General:

  - SmartQuotes transform for typographic quotes and dashes.

  - ``docutils/math``, ``docutils/error_reporting.py``, and
    ``docutils/urischemes.py`` moved to the utils package.
    Code importing these modules needs to adapt, e.g.::

      try:
          import docutils.math as math
      except ImportError:
          import docutils.utils.math as math

  - enhanced math and error handling.

* docutils/io.py

  - FileInput/FileOutput: no system-exit on IOError.
    The `handle_io_errors` argument is ignored.

* docutils/writers/html4css1/__init__.py

  - Use ``<code>`` tag for inline "code",
    do not drop nested inline nodes (syntax highlight tokens).
  - Customizable MathJax URL (based on patch by Dmitry Shachnev).
  - No line break after opening inline math tag.

* docutils/writers/latex2e/__init__.py, docutils/writers/xetex/__init__.py

  - Fix section numbering by LaTeX.

* docutils/writers/s5_html/__init__.py

  - Fix [ 3556388 ] Mathjax does not work with rst2s5.


Release 0.9.1 (2012-06-17)
==========================

.. Note::

   Docutils 0.9.1 is the last version supporting Python 2.3.

* General:

  Several fixes for Python 3 usage.

* docutils/setup.py

  - Fix [ 3527842 ]. Under Python 3, converted tests and tools were
    installed in the PYTHONPATH. Converted tests are now
    stored in ``docutils/test3/``, tools no longer need conversion.

    If you installed one of Docutils versions 0.7 ... 0.9 with
    ``setup.py install`` under Python 3, remove the spurious
    ``test/`` and ``tools/`` directories in the site library root.


Release 0.9 (2012-05-02)
=========================

* General:

  - reStructuredText "code" role and directive with syntax highlighting
    by Pygments_.
  - "code" option of the "include" directive.

  .. _Pygments: https://pygments.org/

  - Fix [ 3402314 ] allow non-ASCII whitespace, punctuation
    characters and "international" quotes around inline markup.

  - Fix handling of missing stylesheets.

* setup.py

  - Fix [ 2971827 ] and [ 3442827 ]
    extras/roman.py moved to docutils/utils/roman.py

* docutils/utils.py -> docutils/utils/__init__.py

  - docutils.utils is now a package (providing a place for sub-modules)

* docutils/writers/html4css1/__init__.py

  - change default for `math-output` setting to MathJax

* docutils/writers/latex2e/__init__.py

  - Support the `abbreviation` and `acronym` standard roles.
  - Record only files required to generate the LaTeX source as dependencies.
  - Use ``\setcounter{secnumdepth}{0}`` instead of ``*``-versions
    when suppressing LaTeX section numbering.


Release 0.8.1 (2011-08-30)
==========================

* General:

  - Fix [ 3364658 ] (Change last file with Apache license to BSD-2-Clause)
    and [ 3395920 ] (correct copyright info for rst.el).

* docutils/writers/latex2e/__init__.py

  - Clean up Babel language setting. Restores Sphinx compatibility.


Release 0.8 (2011-07-07)
========================

* COPYING:

  - Some additions to the Docutils core are released under the 2-Clause BSD
    license.

* General:

  - Handle language codes according to `BCP 47`_.
  - If the specified language is not supported by Docutils,
    warn and fall back to English.
  - Math support: reStructuredText "math" role and directive,
    ``math`` and ``math_block`` doctree elements.
  - Orphaned "python" reader and "newlatex2e" writer moved to the sandbox.

  .. _BCP 47: https://www.rfc-editor.org/rfc/bcp/bcp47.txt

* reStructuredText:

  - most directives now support a "name" option that attaches a
    reference name. So you can write ::

      .. image:: image.png
         :name: image name

    as a short form of ::

      .. _image name:

      .. image:: image.png

Internationalization:

* Added Lithuanian mappings.

Components:

* HTML writer:

  - New setting "math-output" with support for HTML, MathML, and LaTeX.

* LaTeX2e writer:

  - Convert image URI to a local file path.
  - Apply [ 3148141 ] fix multicolumn support when a colspanning cell
    has more than one paragraph (Wolfgang Scherer).

* XeTeX writer:

  - New writer generating LaTeX code for compiling with ``xelatex``.

    XeTeX uses unicode and modern font technologies.

* and fixes and enhancements here and there.


Release 0.7 (2010-07-07)
========================

Components:

* HTML writer:

  - Support SVG and SWF images (thanks to Stefan Rank).
  - Generate valid XHTML for centered images with targets.
    Use CSS classes instead of "align" tags for image alignment.

* LaTeX2e writer:

  - Use the ``\url`` command for URLs (breaks long URLs instead of writing
    into the margin).
  - Preserve runs of spaces in 'inline literals'.
  - Deprecate ``figure_footnotes`` setting.
  - Rename ``use_latex_footnotes`` setting to `docutils_footnotes`__.
  - New ``latex_preamble`` setting.
  - Use PDF standard fonts (Times/Helvetica/Courier) as default.
  - `hyperref` package called with ``unicode`` option (see the
    `hyperref config tips`__ for how to override).
  - Drop the special `output_encoding`__ default ("latin-1").
    The Docutils wide default (usually "UTF-8") is used instead.

  __ docs/user/config.html#docutils-footnotes
  __ docs/user/latex.html#hyperlinks
  __ docs/user/latex.html#output-encoding

* manpage writer:

  - Titles level 1, that is ``.SH``, always uppercase.
  - Apply patch from mg: literal text should be bold in man-pages.

General:

* io.FileInput opens files as text files with universal newline support
  (mode "rU", configurable with the new optional argument "mode").

* setup.py:

  - Python 3 support: copy test/ and tools/ to the build-dir
    and convert Python sources with 2to3.


Release 0.6 (2009-10-11)
========================

Docutils 0.6 is compatible with Python versions from 2.3 up to 2.6
and convertible to 3.1 code.

.. note::

   The "newlatex" writer is orphaned.

   The recommended way to generate PDF output is to use either the
   LaTeX2e writer or one of the alternatives listed at
   https://docutils.sourceforge.io/docs/user/links.html#pdf.

* reStructuredText:

  - Allow length units for all length specifications.
  - Allow percent sign in "scale" option of "figure" and "image_" directives.
  - Align images with class "align-[right|center|left]"
    (allows setting the alignment of an image in a figure).
  - Hard tabs **in literal inclusions** are replaced by spaces.
    This is configurable via the new ``:tab-width:`` option of the
    `"include"`_ directive (a negative tab-width prevents tab expansion).

* HTML writer:

  - ``--stylesheet`` and ``--stylesheet-path`` options now support a comma
    separated list of stylesheets.

* LaTeX2e writer:

  - New defaults:
    - font-encoding: "T1" (formerly implicitly set by 'ae').
    - use-latex-toc: true (ToC with page numbers).
    - use-latex-footnotes: true (no mixup with figures).
    - Float placement defaults to "here definitely" (configurable).
    - Align of image in a figure defaults to 'center'.
    - Use class defaults for page margins ('typearea' now optional).
  - Support LaTeX packages as ``--stylesheet`` arguments.
  - Use ``bp`` for lengths without unit or unit ``pt``,
    do not convert ``px`` to ``pt``.
  - Do not use 'ae' and 'aeguill' packages if font-encoding is set to ''.
  - Set sub- and superscript role argument as text not math.
  - Support custom roles based on standard roles.
  - Load packages and define macros only if required in the document.
  - All Docutils specific LaTeX macros are prefixed with ``DU``.
  - Better conformance to Docutils specifications with "use_latex_toc".
  - If 'sectnum_xform' is False, the 'sectnum' directive triggers
    section numbering by LaTeX.
  - Use default font in admonitions and sidebar.
  - Typeset generic topic as "quote with title".
  - Use template (file and configuration option).
  - Render doctest blocks as literal blocks (indented).
  - Bugfix: The "align" argument of a figure now works as documented
    (aligning the figure, not its contents).

* ODT writer:

  - moved from sandbox to Doctutils core.

* manpage writer:

  - moved from sandbox to Doctutils core.


Release 0.5 (2008-06-25)
========================

.. Note::

   Docutils 0.5 is the last version supporting Python 2.2.

Components:

* HTML writer.

  - Dropped all ``name`` attributes of ``a`` elements (``id`` is
    universally supported now).

* LaTeX2e writer:

  - Better bibTeX citation support.
  - Add ``--literal-block-env``

* PEP writer:

  - Changed to support new python.org website structure and
    pep2pyramid.py.

reStructuredText:

* Changed the directive API to a new object-oriented system.
  (Compatibility for the old, functional-style directive interface is
  retained.)  See the updated `Creating reStructuredText Directives`__
  how-to.

  __ docs/howto/rst-directives.html

* Allow ``+`` and ``:`` in reference names requested for citations.

Documentation:

* Added `Deploying Docutils Securely`__

  __ docs/howto/security.rst

Internationalization:

* Added hebrew mappings.

General:

* Configuration files are now assumed and required to be
  UTF-8-encoded.

* Added docutils/writers/html4css1/template.txt.

* Enhance emacs support.


Release 0.4 (2006-01-09)
========================

.. Note::

   Docutils 0.4 is the last version supporting Python 2.1.

   It is also the last version that will make compromises in
   its HTML output for Netscape Navigator 4.  Docutils 0.5 will
   require more up-to-date browsers (the exact definition is to be
   determined).

Components:

* Added an `S5/HTML writer`__ and the rst2s5.py__ front end:
  multi-platform, multi-browser HTML slide shows.

  __ docs/user/slide-shows.html
  __ docs/user/tools.html#rst2s5

* The newlatex2e writer is nearing completion.

* Added a DocTree reader, ``publish_doctree`` and
  ``publish_from_doctree`` convenience functions, for document tree
  extraction and reprocessing.

reStructuredText:

* Added directives: "container__" (generic block-level container),
  "default-role__" (role used for \`backtick\` syntax), "title__"
  (document title metadata), and `"date"`_ (generate the current local
  date, for substitution definitions).

  __ docs/ref/rst/directives.html#container
  __ docs/ref/rst/directives.html#default-role
  __ docs/ref/rst/directives.html#title

* Length units are now supported for image_ sizes.

* Added `standard definition files`__ for special characters etc.

  __ docs/ref/rst/definitions.html

Internationalization:

* Added Japanese and Simplified Chinese language mappings, and support
  for double-width CJK-characters in tables and section titles.

Documentation:

* Added a `guide for distributors`__ (package maintainers) and a
  `guide for developers`__.

  __ docs/dev/distributing.html
  __ docs/dev/hacking.html

General:

* Added significant `Emacs support for reST`__.

  __ docs/user/emacs.html

* Added a `--strip-comments`__ option.

  __ docs/user/config.html#strip-comments

* `--embed-stylesheet`__ is now the default for the HTML writer
  (rather than --link-stylesheet).

  __ docs/user/config.html#embed-stylesheet


Release 0.3.9 (2005-05-26)
==========================

* Added "file_insertion_enabled__" and "raw_enabled__" settings.

  __ docs/user/config.html#file-insertion-enabled
  __ docs/user/config.html#raw-enabled

* Added `auto-enumerated lists`__.

  __ docs/ref/rst/restructuredtext.html#enumerated-lists

* Added `"header" and "footer"`__ directives.

  __ docs/ref/rst/directives.html#document-header-footer

* Added "list-table__" directive.

  __ docs/ref/rst/directives.html#list-table

* Added support for `section subtitles`__.

  __ docs/user/config.html#sectsubtitle-xform

* Added "field_name_limit__" and "option_limit__" settings to HTML writer.

  __ docs/user/config.html#field-name-limit
  __ docs/user/config.html#option-limit

* Added "cloak_email_addresses__" setting to HTML writer.

  __ docs/user/config.html#cloak-email-addresses

* UTF-8 BOMs are now removed from the input stream.


Release 0.3.7 (2004-12-24)
==========================

* A special "`line block`__" syntax has been added.  (Also see the
  `quick reference`__.)

  __ docs/ref/rst/restructuredtext.html#line-blocks
  __ docs/user/rst/quickref.html#line-blocks

* Empty sections are now allowed.

* A "raw__" role has been added.

  __ docs/ref/rst/roles.html#raw

* The LaTeX writer now escapes consecutive dashes (like "--" or "---")
  so that they are no longer transformed by LaTeX to en or em dashes.
  (Please see the FAQ__ for how to represent such dashes.)

  __ FAQ.html#how-can-i-represent-esoteric-characters-e-g-character-entities-in-a-document

* A `dependency recorder`__ has been added.

  __ docs/user/config.html#record-dependencies

* A directive has been added for `compound paragraphs`__.

  __ docs/ref/rst/directives.html#compound-paragraph


Release 0.3.5 (2004-07-29)
==========================

* Improved, extended and reorganized the documentation__.

  __ docs/index.html

* Added "csv-table_" directive.


.. References
   ==========

.. _HISTORY: HISTORY.html

.. _transform: docs/api/transforms.html

.. _Docutils Document Model:
.. _Docutils XML: docs/ref/doctree.html
.. _"colwidth" attribute: docs/ref/doctree.html#colwidth
.. _<doctest_block>: docs/ref/doctree.html#doctest-block

.. _"class": docs/ref/rst/directives.html#class
.. _csv-table: docs/ref/rst/directives.html#csv-table
.. _"date": docs/ref/rst/directives.html#date
.. _doctest block: docs/ref/rst/restructuredtext.html#doctest-blocks
.. _"figure": docs/ref/rst/directives.html#figure
.. _identifier normalization:
    docs/ref/rst/directives.html#identifier-normalization
.. _image: docs/ref/rst/directives.html#image
.. _"include":
    docs/ref/rst/directives.html#including-an-external-document-fragment
.. _"widths" option: docs/ref/rst/directives.html#table

.. _Standard definition file: docs/ref/rst/definitions.html
.. _LaTeX syntax for mathematics: docs/ref/rst/mathematics.html

.. _configuration settings: docs/user/config.html
.. _auto_id_prefix: docs/user/config.html#auto-id-prefix
.. _datestamp: docs/user/config.html#datestamp
.. _embed_images: docs/user/config.html#embed-images
.. _graphicx_option: docs/user/config.html#graphicx-option
.. _legacy_class_functions: docs/user/config.html#legacy-class-functions
.. _literal_block_env: docs/user/config.html#literal-block-env
.. _id_prefix: docs/user/config.html#id-prefix
.. _image_loading: docs/user/config.html#image-loading
.. _initial_header_level: docs/user/config.html#initial-header-level
.. _input_encoding: docs/user/config.html#input-encoding
.. _[latex writers]: docs/user/config.html#latex-writers
.. _legacy_column_widths: docs/user/config.html#legacy-column-widths
.. _text_references: docs/user/config.html#text-references
.. _math_output: docs/user/config.html#math-output
.. _old-format configuration files:
    docs/user/config.html#old-format-configuration-files
.. _output:
.. _output_path: docs/user/config.html#output-path
.. _reference_label: docs/user/config.html#reference-label
.. _root_prefix: docs/user/config.html#root-prefix
.. _SmartQuotes: docs/user/config.html#smart-quotes
.. _sources: docs/user/config.html#sources
.. _use_latex_citations: docs/user/config.html#use-latex-citations
.. _validate: docs/user/config.html#validate
.. _"writer" setting: docs/user/config.html#writer-buildhtml-application

.. _front end tools: docs/user/tools.html
.. _docutils-cli.py:
.. _generic command line front end tool:
    docs/user/tools.html#generic-command-line-front-end
.. _rst2html: docs/user/tools.html#rst2html
.. _rst2html4: docs/user/tools.html#rst2html4
.. _rst2html5: docs/user/tools.html#rst2html5
.. _reference name: docs/ref/rst/restructuredtext.html#reference-names
.. _buildhtml.py: docs/user/tools.html#buildhtml-py
