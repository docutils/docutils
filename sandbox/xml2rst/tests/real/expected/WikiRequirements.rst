#FORMAT rst

Requirements for an Oekonux Wiki
================================

.. contents:: Contents


.. ############################################################################

.. ############################################################################

Introduction
============

In `Project Oekonux`_ there has been a long discussion about setting
up a Wiki for the project. Also several attempts to establish a Wiki
have been made. Most of these discussions took place on `the projekt mailing list`_
which is the organizing body of the project.

One of the most important questions was which Wiki software to use.
Finally a decision has been made based upon the requirements being
raised.

The following lists all the requirements which have been found and
lists the alternative solutions for them. One reason for this list is
to document the design decisions / choices taken and their basis in
the requirements. This way it is easier to

* understand the decisions

* change decisions while not forgetting about reasons for the original
  decision

.. ############################################################################

.. ############################################################################

Requirements
============

Non-functional requirements
---------------------------

1. The software must have an active developer community and must be
   Free

   :Rationale: Only an active developer community of a Free Software
       project guarantees for a constant development.

   Discussion came down to two candidates which met this requirement:
   MediaWiki_ and MoinMoin_. The following requirements list
   alternative solutions only for these two Wiki implementations.

2. The software must be easy to use for people used to Wikipedia

   :Rationale: Because of the overwhelming success of Wikipedia many
       people are used to the way how Wikipedia is operated.

   :MediaWiki: Given by definition.

   :MoinMoin: The typical use cases need to be identified and a
       special help page must be created for these users.

.. ############################################################################

Functional requirements
-----------------------

1.  Email notification must be possible sending diffs and other changes

    :Rationale: Email notification is a basic requirement for any
        offline processing.

    :MediaWiki: Rumor has it that there is a plug-in for email
        notification but nobody saw it yet (2005-03-26).

    :MoinMoin: Supports email notification as diffs for arbitrary pages
        and page groups (regular expression). Only notifies on moves and
        deletion of pages.

2.  It must be possible to limit changeability of a page (for instance
    for user's pages)

    :Rationale: Some pages are private pages or should not be changes
        for other reasons.

    :MediaWiki: Don't know whether this can be enforced technically.

    :MoinMoin: Implements access control lists (ACL) allowing for
        arbitrary rights.

3.  Categories must be possible

    :Rationale: With categories can form arbitrary non-hierarchical
        structures

    :MediaWiki: Available.

    :MoinMoin: Available.

4.  Navigation panels must be available

    :Rationale: Useful to create overviews.

    :MediaWiki: Available.

    :MoinMoin: Available as ``Navigation`` macro for hierarchical
        structures and as ``PageList`` macro for structures based logical
        expressions on regular expressions over page names.

5.  A version history must be available

    :Rationale: Needed to get an overview of the change history of a
        page.

    :MediaWiki: Available.

    :MoinMoin: Available.

6.  List of changes for a given user must be available

    :Rationale: Nice to have.

    :MediaWiki: Available.

    :MoinMoin: Not available.

7.  A watch list must be possible reporting about the latest changes
    for a given set of pages

    :Rationale: Nice to have.

    :MediaWiki: Available.

    :MoinMoin: Can be simulated by email notification.

8.  Automatic links by using CamelCase (aka WikiWords) must not be the
    default

    :Rationale: CamelCase makes sense only in certain language. In
        particular German is a language where automatic CamelCase links
        are not too useful.

    :MediaWiki: CamelCase words never create automatic links.

    :MoinMoin: The NoCamelCase_ plug-in makes this the default.

9.  Support for other than the standard Wiki syntaxes like
    reStructuredText_ must be available

    :Rationale: All Wiki languages suck so support for more sane
        syntaxes is a very useful thing. In particular support for
        reStructuredText_ being a powerful language and a good candidate
        for a standard ASCII based markup language.

    :MediaWiki: There is no support for any other syntax than the
        MediaWiki syntax.

    :MoinMoin: Offers several syntaxes by the concept of parsers. Among
        the available syntaxes are reStructuredText_ and LaTeX_.

10. Arbitrary attachments must be possible

    :Rationale: Nice to have to attach arbitrary data to a page. In
        particular it makes possible to include material not marked up in
        the Wiki syntax.

    :MediaWiki: Unknown but probably available.

    :MoinMoin: Available.

11. Shortcuts for links must be available

    :Rationale: In particular it is useful to be able to reference
        entries in the Oekonux mailing list archive as easy as possible.

    :MediaWiki: Implements this by templates.

    :MoinMoin: Implements by InterWiki links.

12. Page templates must be available

    :Rationale: Page templates are a good way to support policy
        decisions by offering standard templates for all page types.

    :MediaWiki: Not available.

    :MoinMoin: Available.

13. It must be possible to revert page changes

    :Rationale: Useful to undo unwanted changes.

    :MediaWiki: Available.

    :MoinMoin: Available.

14. It must be possible to know who did a change in a page

    :Rationale: Nice to have.

    :MediaWiki: Available.

    :MoinMoin: Available.

15. Offline usage must be possible as far as possible

    :Rationale: Not everyone is always online so offline facilities are
        generally useful. Offline usage includes

        * a push feature for change notification

        * browsing

        * editing

    :MediaWiki: Not available. At most email notification is available.

    :MoinMoin: Email notification allows for monitoring changes and in
        principle the diffs can be used to update a local copy. A local
        installation of MoinMoin_ only needs Python and MoinMoin_ so it
        is easy to accomplish. Because MoinMoin_ uses a file based
        storage scheme it is at least easy to update a local copy in
        short online phases for offline use so at least offline browsing
        is available.

16. Pages must be locked during they are edited

    :Rationale: Page locking prevents parallel editing of a page which
        is useful in a highly frequented Wiki.

    :MediaWiki: Not available.

    :MoinMoin: Editing a page locks it for 10 minutes and the lock can
        be renewed.

17. It must be possible to edit sections of a page

    :Rationale: Makes parallel editing of a page less dangerous which
        is useful in a highly frequented Wiki.

    :MediaWiki: Available.

    :MoinMoin: Not available.

.. ############################################################################

Requirements for policy choices
-------------------------------

1. Pages standing in a certain close relation to a certain other page
   must be possible in a sane way

   :Rationale: There are several ways in which a page can have closely
       related pages (e.g. discussion pages). It makes sense to have a
       uniform way to express this relation.

   :MediaWiki: Implements discussion pages as one a special type of
       closely related pages by suffixing ``Discussion`` to the
       namespace of a page. For other closely related pages there is no
       fixed implementation.

   :MoinMoin: By making sub-pages possible all types of closely
       related pages can be implemented by some fixed names being part
       of a policy and supported by page templates.

2. Structuring of content must be possible by a page hierarchy

   :Rationale: The maintenance policy may decide to have a page
       hierarchy as a structuring principle.

   :MediaWiki: Implements a top level structure by namespaces and
       allows for structuring otherwise.

   :MoinMoin: With sub-pages arbitrary hierarchies can be build on any
       level.

3. Sub-Wikis must be possible

   :Rationale: The maintenance policy may decide this makes sense.

   :MediaWiki: Not available.

   :MoinMoin: Could be done in a page hierarchy on any level.

4. Underscores and white-space in page names must be possible but
   insignificant

   :Rationale: If the maintenance policy decides that underscores and
       white-space may be used in arbitrary ways the software must be
       able to reflect this.

   :MediaWiki: Unknown.

   :MoinMoin: Underscores and white-space is significant.

.. ############################################################################

.. ############################################################################

.. |projekt| replace:: `the projekt mailing list`_

.. |NoCamelCase| replace:: NoCamelCase_

.. _project oekonux: http://www.oekonux.de/

.. _the projekt mailing list: http://www.oekonux.de/projekt/liste/

.. _mediawiki: http://wikipedia.sourceforge.net/

.. _moinmoin: http://moinmoin.wikiwikiweb.de/

.. _restructuredtext: http://docutils.sourceforge.net/rst.html

.. _latex: http://www.latex-project.org/

.. _nocamelcase: http://moinmoin.wikiwikiweb.de/ParserMarket/NoCamelCase

.. LocalWords:  diffs CamelCase InterWiki MoinMoin rst projekt MediaWiki aka

.. LocalWords:  Wikipedia PageList WikiWords reStructuredText

.. LocalWords:  Wikis NoCamelCase
