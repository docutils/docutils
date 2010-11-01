#format rst

==========================================================
FAQ for people used to Wikipedia and other MediaWiki wikis
==========================================================

.. contents::

Introduction
============

This page is a FAQ page for users used to Wikipedia and other
MediaWiki_ [#]_ wikis. In Project Oekonux we chose MoinMoin_ because
at the time of the decision there was a slight technological
superiority opening more options for its use. If you are interested in
the reasons you may want to consult the requirements_ which have been
found then.

.. [#] MediaWiki_ is the name of the software used for Wikipedia and
       other Wikis. The software running this Wiki is called
       MoinMoin_.

However, MediaWiki_ and it's use is widely known and thus Project
Oekonux strives to make the use of this MoinMoin_ wiki as easy to
MediaWiki_ users as possible. This FAQ is one attempt to do so. If you
are a MediaWiki_ user and have questions not covered here feel free to
contact projekt@oekonux.de.

.. ############################################################################
.. ############################################################################

Question and answers
====================

Of all users
------------

What about the namespace concept?
.................................

Most of the namespaces in MediaWiki_ are nothing else than a
structuring facility establishing structure on a top level.

In MoinMoin_ there is no concept of namespaces in the sense of
MediaWiki_. However, there is the concept of `sub pages`_ [#]_ which
have names containing one or more slashes (``/``).

.. [#] There are also sub pages in MediaWiki_ but they are rarely used
       in any area but the ``Users:`` namespace.

In MoinMoin_ we map the concept of namespaces to the top level `sub
pages`_ of the wiki. So if you have a page named ``Oekonux:Project``
in MediaWiki_ it would be named ``Oekonux/Project`` in MoinMoin_.

Please note that `sub pages`_ are more flexible than namespaces
because they allow for more than one level or semantic hierarchy.

What about the ``Users:`` namespace?
....................................

Unfortunately the ``Users:`` namespace as such can not be easily
modeled into MoinMoin_. The homepages of users need to stay on the
top level so the MediaWiki_ page ``Users:JohnDoe`` is mapped to
``JohnDoe`` in MoinMoin_.

What about the ``Special:`` namespace?
......................................

The ``Special:`` namespace in MediaWiki_ does not lead to normal pages
but to pages which are generated dynamically by the software.

In MoinMoin_ such pages (and a lot more) are modeled by macros_. So
what in MediaWiki_ reads ``Special:RecentChanges`` in MoinMoin_ is
written ``[[RecentChanges]]``.

What about the ``Talk:`` namespace?
.........................................

Actually the ``Talk:`` namespace in MediaWiki_ reachable by the
``Discussion`` tab is not a namespace in the sense of the other
namespaces. Instead it is an add-on for pages in all namespaces.

In MoinMoin_ this is modeled by `sub pages`_. However, instead of
tagging the term ``Talk`` in the very front of the name of a page in
MoinMoin_ this is added to the end. So for the MediaWiki_ page name
``Oekonux_Talk:Project`` in MoinMoin_ the name
``Oekonux/Project/Talk`` is used.

To foster similarity to MediaWiki_ wikis most of the page templates
for this wiki contain a link to a ``/Talk`` `sub page`_.

.. ############################################################################

Of editors
----------

I'm used to `MediaWiki syntax`_. Can I use it in this Wiki?
...........................................................

By default MoinMoin_ has a slightly `different syntax`_ then
MediaWiki_. However, MoinMoin_ can support many languages if only a
parser is available for it. There is a parser for MediaWiki_ syntax
which can be used for this Wiki. All you have to do is to put ::

  #format media

in the very first line of your document. From that point on `MediaWiki
syntax`_ is recognized.

Currently the parser does not support the full `MediaWiki syntax`_ but
a considerable subset of it. For most pages this subset is completely
sufficient. For an extensive list of supported and unsupported
features please check the `documentation of the parser`_.

Though I'm used to `MediaWiki syntax`_ I think it sucks. Any alternatives?
..........................................................................

Because in principle MoinMoin_ supports different syntaxes the general
answer is: Yes, you can use other syntaxes if and when the parser
plug-in for it exists and is installed. The default parser for
MoinMoin_ parses the `MoinMoin syntax`_.

In this Wiki by default we use a variant without automatic CamelCase
links and without InterWiki_ links without special markup. If you want
CamelCase and InterWiki_ links put ::

  #format wiki

in the first line of your page. The MoinMoin wiki markup language has
more options than the MediaWiki markup language but still is quite
illogical.

Otherwise a very promising ASCII based markup is reStructuredText_. It
is far more logical than any wiki markup language and it has a set of
nice features. If you want to use it put ::

  #format rst

in the first line of your page.

I tried to create a new page and ended up on this page offering templates. What's this?
.......................................................................................

In MoinMoin_ it is possible to have page templates. A page templates
are an easy way to help people to conform to some style guide lines
established for the given wiki. They are created by the maintainers of
the wiki.

The name of a page template is build from the type of page the page
template is for and the suffix ``Template`` (in English). So for
instance a page template meant for talk pages is named
``TalkTemplate``.

In a sense a page template bundles some general knowledge about the
wiki ready to be used by the users of the wiki. Of course you can
start a new page from scratch but usually it makes more sense to use
an existing page template.

Page templates, ok. What about the MediaWiki_ templates?
........................................................

In MoinMoin_ the term "template" is used differently than in
MediaWiki_. In MoinMoin_ a template is a *page* template [#]_ while in
MediaWiki_ it is more like a function call in a programming language.

.. [#] The concept of page templates is probably not known in
       MediaWiki_.

In general there are two ways to get the functionality of MediaWiki_
templates in MoinMoin_. MediaWiki_ templates which are used as
shortcuts for links are mapped to InterWiki_ links. For instance a
link to the posting containing the requirements_ for this wiki in the
archive of the project mailing list ``[pox]`` is named
``poxArchive:04957`` which expands to the rather long link
http://www.oekonux.de/projekt/liste/archive/msg04957.html. You
probably agree that the former is easier to type :-) . Such a link can
be used as any other link. In particular it can be used together with
a display text. If your favorite link target is not yet supported by
this wiki please contact projekt@oekonux.de about it.

MediaWiki_ templates not building shortcuts for links generally can be
mapped to MoinMoin_ macros_. If the functionality you need is not yet
supported by this wiki please contact projekt@oekonux.de about it.

.. ############################################################################
.. ############################################################################

.. _MediaWiki: http://wikipedia.sourceforge.net/

.. _MoinMoin: http://moinmoin.wikiwikiweb.de/

.. _requirements: Oekonux/Project/Wiki/Requirements_

.. _MediaWiki syntax: http://meta.wikimedia.org/wiki/Help:Editing#The_wiki_markup


.. _different syntax: HelpOnEditing_
.. _MoinMoin syntax: HelpOnEditing_

.. _documentation of the parser: http://www.merten-home.de/FreeSoftware/media4moin/manual.html

.. _sub page:
.. _sub pages: HelpOnEditing/SubPages_

.. _macros: HelpOnMacros_

.. _reStructuredText: http://docutils.sourceforge.net/rst.html

..  LocalWords:  rst Wikipedia MediaWiki MoinMoin homepages JohnDoe CamelCase
..  LocalWords:  RecentChanges InterWiki reStructuredText TalkTemplate SubPages
..  LocalWords:  poxArchive HelpOnEditing HelpOnMacros
