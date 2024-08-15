.. include:: ../../header2.rst

=========================================
 reStructuredText Interpreted Text Roles
=========================================

:Author: David Goodger
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.
:Abstract:
  This document describes the interpreted text roles implemented in the
  reference reStructuredText parser.

Interpreted text uses backquotes (`) around the text.  An explicit
role marker may optionally appear before or after the text, delimited
with colons.  For example::

    This is `interpreted text` using the default role.

    This is :title:`interpreted text` using an explicit role.

A "default role" may be defined by applications of reStructuredText; it
is used if no explicit ``:role:`` prefix or suffix is given.  The
default "default role" is "`:title-reference:`_".  It can be changed
using the `"default-role" directive`_.

See the `Interpreted Text`_ section in the `reStructuredText Markup
Specification`_ for syntax details.  For details on the hierarchy of
elements, please see `The Docutils Document Tree`_ and the `Docutils
Generic DTD`_ XML document type definition.  For interpreted text role
implementation details, see `Creating reStructuredText Interpreted
Text Roles`_.


.. contents::


Standard Roles
==============


--------------
:abbreviation:
--------------

.. class:: field-indent-12em

:Aliases:         :ab:
:Doctree Element: `\<abbreviation>`_

An abbreviation used in the document.
An example of an abbreviation is ‘St’ being used instead of ‘Street’.


---------
:acronym:
---------

.. class:: field-indent-12em

:Aliases:         \:ac:
:Doctree Element: `\<acronym>`_

An acronym.


------
:code:
------

.. class:: field-indent-12em

:Aliases:         None
:Doctree Element: `\<literal>`_
:Customization_ options: `class <"class" option_>`__,
                         `language <"language" option_>`__

(New in Docutils 0.9.)

The :code: role marks its content as code in a formal language.

For syntax highlight of inline code, the `"role" directive`_ can
be used to create `custom roles`_ with the code language specified
in the `"language" option`_.
For example, the following creates a LaTeX-specific "latex" role::

  .. role:: latex(code)
     :language: latex

Content of the new role is parsed and tagged by the Pygments_ syntax
highlighter. See the `"code" directive`_ for more info on parsing
and display of code in reStructuredText.

.. _customization: `custom roles`_
.. _Pygments: https://pygments.org/


----------
:emphasis:
----------

.. class:: field-indent-12em

:Aliases:         None
:Doctree Element: `\<emphasis>`_

Implements emphasis.  These are equivalent::

    *text*
    :emphasis:`text`


---------
:literal:
---------

.. class:: field-indent-12em

:Aliases:         None
:Doctree Element: `\<literal>`_

Implements inline literal text.  These are equivalent::

    ``text``
    :literal:`text`

Care must be taken with backslash-escapes though.  These are *not*
equivalent::

    ``text \ and \ backslashes``
    :literal:`text \ and \ backslashes`

The backslashes in the first line are preserved (and do nothing),
whereas the backslashes in the second line escape the following
spaces.


------
:math:
------

.. class:: field-indent-12em

:Aliases:         None
:Doctree Element: `\<math>`_

(New in Docutils 0.8.)

The ``math`` role marks its content as mathematical notation (inline
formula).

The input format is LaTeX math syntax without the “math delimiters“
(``$ $``), for example::

  The area of a circle is :math:`A_\text{c} = (\pi/4) d^2`.

See the `"math" directive`_ (producing display formulas) for more info
on mathematical notation in reStructuredText.


---------------
:pep-reference:
---------------

.. class:: field-indent-12em

:Aliases:         \:PEP:
:Doctree Element: `\<reference>`_

The :pep-reference: role is used to create an HTTP reference to a
PEP (Python Enhancement Proposal).  The :PEP: alias is usually
used.  The content must be a number, for example::

    See :PEP:`287` for more information about reStructuredText.

This is equivalent to::

    See `PEP 287`__ for more information about reStructuredText.

    __ https://peps.python.org/pep-0287


---------------
:rfc-reference:
---------------

.. class:: field-indent-12em

:Aliases:         \:RFC:
:Doctree Element: `\<reference>`_

The :rfc-reference: role is used to create an HTTP reference to an
RFC (Internet Request for Comments).  The :RFC: alias is usually
used.  The content must be a number [#]_, for example::

    See :RFC:`2822` for information about email headers.

This is equivalent to::

    See `RFC 2822`__ for information about email headers.

    __ https://tools.ietf.org/html/rfc2822.html

.. [#] You can link to a specific section by saying
   ``:rfc:`number#anchor```. (New in Docutils 0.15.)

   .. Warning:: The anchor (anything following a ``#``) is appended to
      the reference without any checks and not shown in the link text.

      It is recommended to use `hyperlink references`_ for
      anything more complex than a single RFC number.


--------
:strong:
--------

.. class:: field-indent-12em

:Aliases:         None
:Doctree Element: `\<strong>`_

Implements strong emphasis.  These are equivalent::

    **text**
    :strong:`text`


-----------
:subscript:
-----------

.. class:: field-indent-12em

:Aliases:         \:sub:
:Doctree Element: `\<subscript>`_

Implements subscripts.

.. Tip::

   Whitespace or punctuation is required around interpreted text, but
   often not desired with subscripts & superscripts.
   Backslash-escaped whitespace can be used; the whitespace will be
   removed from the processed document::

       H\ :sub:`2`\ O
       E = mc\ :sup:`2`

   In such cases, readability of the plain text can be greatly
   improved with substitutions [#]_::

       The chemical formula for pure water is |H2O|.

       .. |H2O| replace:: H\ :sub:`2`\ O

   See `the reStructuredText spec`__ for further information on
   `character-level markup`__ and `the substitution mechanism`__.

   .. [#] Alternatives are the `:math:`_ role (:math:`E=mc^2`)
      or literal Unicode characters (H₂O)

   __ restructuredtext.html
   __ restructuredtext.html#character-level-inline-markup-1
   __ restructuredtext.html#substitution-references


-------------
:superscript:
-------------

.. class:: field-indent-12em

:Aliases:         \:sup:
:Doctree Element: `\<superscript>`_

Implements superscripts.  See the tip in `:subscript:`_ above.


-----------------
:title-reference:
-----------------

.. class:: field-indent-12em

:Aliases:         \:title:, :t:
:Doctree Element: `\<title_reference>`_

The :title-reference: role is used to describe the titles of
books, periodicals, and other materials.  It is the equivalent of the
HTML "cite" element, and it is expected that HTML writers will
typically render "title_reference" elements using "cite".

Since title references are typically rendered with italics, they are
often marked up using ``*emphasis*``, which is misleading and vague.
The "title_reference" element provides accurate and unambiguous
descriptive markup.

Let's assume :title-reference: is the default interpreted text
role (see below) for this example::

    `Design Patterns` [GoF95]_ is an excellent read.

The following document fragment (pseudo-XML_) will result from
processing::

    <paragraph>
        <title_reference>
            Design Patterns

        <citation_reference refname="gof95">
            GoF95
         is an excellent read.

`:title-reference:`_ is the default interpreted text role in the
standard reStructuredText parser.  This means that no explicit role is
required.  Applications of reStructuredText may designate a different
default role, in which case the explicit :title-reference: role
must be used to obtain a ``title_reference`` element.


Specialized Roles
=================

-----
:raw:
-----

.. class:: field-indent-12em

:Aliases:         None
:Doctree Element: `\<raw>`_
:Customization_ options: `class <"class" option_>`__,
                         `format <"format" option_>`__

The "raw" role indicates non-reStructuredText data that is to be
passed untouched to the Writer.  It is the inline equivalent of the
`"raw" directive`_; see its documentation for details on the
semantics.

**The "raw" role cannot be used directly.**  The `"role" directive`_ must
first be used to create `custom roles`_ based on the "raw" role.  One or
more formats (Writer names) must be provided in a `"format" option`_.

.. WARNING::
   The "raw" role is a stop-gap measure allowing the author to bypass
   reStructuredText's markup.  It is a "power-user" feature that
   should not be overused or abused.  The use of "raw" ties documents
   to specific output formats and makes them less portable.

   If you often need to use "raw"-derived interpreted text roles or
   the "raw" directive, that is a sign either of overuse/abuse or that
   functionality may be missing from reStructuredText.  Please
   describe your situation in a message to the Docutils-users_ mailing
   list.

   .. _Docutils-users: ../../user/mailing-lists.html#docutils-user

For example, the following creates an HTML-specific "raw-html" role::

    .. role:: raw-html(raw)
       :format: html

This role can now be used directly to pass data untouched to the HTML
Writer.  For example::

    If there just *has* to be a line break here,
    :raw-html:`<br />`
    it can be accomplished with a "raw"-derived role.
    But the line block syntax should be considered first.

.. Tip:: Roles based on "raw" should clearly indicate their origin, so
   they are not mistaken for reStructuredText markup.  Using a "raw-"
   prefix for role names is recommended.


Custom Roles
============

Custom interpreted text roles can be defined in a document with the
`"role" directive`_. The new role may be based on an existing role.
The "role" directive may be called with options_ to customize the
generated roles.


.. References

.. _reStructuredText Markup Specification: restructuredtext.html
.. _Interpreted Text: restructuredtext.html#interpreted-text
.. _hyperlink references: restructuredtext.html#hyperlink-references

.. _The Docutils Document Tree: ../doctree.html
.. _class names: ../doctree.html#classname
.. _classes attribute: ../doctree.html#classes
.. _format attribute: ../doctree.html#format
.. _pseudo-XML: ../doctree.html#pseudo-xml

.. _Docutils Generic DTD: ../docutils.dtd
.. _Creating reStructuredText Interpreted Text Roles:
    ../../howto/rst-roles.html
.. _writer names: ../../user/config.html#writer-docutils-application

.. _"class" directive:        directives.html#role-class
.. _"code" directive:         directives.html#code
.. _"default-role" directive: directives.html#default-role
.. _"role" directive:         directives.html#role
.. _"math" directive:         directives.html#math
.. _"raw" directive:          directives.html#raw
.. _options:                  directives.html#role-options
.. _"class" option:           directives.html#role-directive-class-option
.. _"format" option:          directives.html#format
.. _"language" option:        directives.html#language

.. _<abbreviation>:    ../doctree.html#abbreviation
.. _<acronym>:         ../doctree.html#acronym
.. _<emphasis>:        ../doctree.html#emphasis
.. _<literal>:         ../doctree.html#literal
.. _<math>:            ../doctree.html#math
.. _<reference>:       ../doctree.html#reference
.. _<raw>:             ../doctree.html#raw
.. _<strong>:          ../doctree.html#strong
.. _<subscript>:       ../doctree.html#subscript
.. _<superscript>:     ../doctree.html#superscript
.. _<title_reference>: ../doctree.html#title-reference
