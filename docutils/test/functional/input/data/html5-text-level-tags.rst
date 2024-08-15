Text-Level Semantics
--------------------

This section describes the `HTML 5 tags for representation of text-level
semantics`__ and their reStructuredText equivalents.

__ https://html.spec.whatwg.org/#text-level-semantics

.. class:: description

a
  Hyperlinks

    Visit my `drinks <example.org:drinks.html>`_ page.

em
  Stress emphasis

    I must say I *adore* lemonade. I's :emphasis:`sooo` sweet.

strong
  Importance

    **Warning**: This tea is :strong:`very hot`.

small
  Side comments

    .. role:: small

    These grapes are made into wine.
    :small:`Alcohol is addictive.`

s
  Inaccurate text

    .. role:: strikeout
       :class: s

    Price: :strikeout:`£4.50` £2.00!

cite
  Titles of works

    The case `Hugo v. Danielle` is relevant here.
    I recommend reading :title:`Harry Potter`.

q
  Quotations

    .. role:: quotation
       :class: q

    The judge said :quotation:`You can drink water from the fish tank`
    but advised against it.

dfn
  Defining instance

    .. role:: dfn

    The term :dfn:`organic food` refers to food produced without synthetic
    chemicals.

abbr
  Abbreviations [#attribute-optional]_

    Organic food in Ireland is certified by the :acronym:`IOFGA` [*]_

  In rST there are separate roles for `abbreviations` :abbreviation:`rsp.`
  `acronymes`. In HTML, the <acronym> tag is obsolete and authors are
  advised to use <abbr> instead. The HTML5 writer uses <abbr> for Docutil's
  <abbreviation> element.

  .. [*] Irish Organic Farmers and Growers Association

ruby, rt, rp
  Ruby annotations

  Require inline nesting, currently not supported in rST.

  ..
    <ruby> OJ <rp>(</rp><rt>Orange Juice</rt><rp>)</rp></ruby>

data
  Machine-readable equivalent [#attribute-required]_

  ..
    Available starting today! <data value="UPC:022014640201">North Coast
    Organic Apple Cider</data>


time
  Machine-readable equivalent of date- or time-related data [#attribute-required]_

  ..
    Available starting on <time datetime="2011-11-18">November 18th</time>!

code
  Computer code

    The :code:`fruitdb` program can be used for tracking fruit production.

var
  Variables

    .. role:: var

    If there are :var:`n` fruit in the bowl, at least :var:`n`\ ÷2 will be
    ripe.

samp
  Computer output

    .. role:: samp(literal)

    The computer said :samp:`Unknown error -3`.

kbd
  User input

    .. role:: kbd

    Hit :kbd:`F1` to continue.

sub
  Subscripts

    Water is H\ :sub:`2`\O.

sup
  Superscripts

    The Hydrogen in heavy water is usually :sup:`2`\H.

i
  Alternative voice

    .. role:: alternative-voice
       :class: i, language-la

    Lemonade consists primarily of :alternative-voice:`Citrus limon`.

b
  Keywords

    .. role:: b

    Take a :b:`lemon` and squeeze it with a :b:`juicer`.

u
  Annotations

    .. role:: spelling
       :class: u, spelling

    The mixture of apple juice and :spelling:`eldeflower` juice is very
    pleasant.

mark
  Highlight

    .. role:: mark

    Elderflower cordial, with one :mark:`part` cordial to ten
    :mark:`part`\ s water, stands a\ :mark:`part` from the rest.

bdi
  Text directionality isolation [#attribute-optional]_

    .. role:: bdi

    The recommended restaurant is :bdi:`My Juice Café (At The Beach)`.

  The `dir` global attribute defaults to "auto" (not "inherit") on this
  element.

bdo
  Text directionality formatting [#attribute-required]_

  ..
    The proposal is to write English, but in reverse order. "Juice" would
    become "<bdo dir=rtl>Juice</bdo>">

  Authors *must* specify the dir attribute on this element.

span
  Other

    .. role:: language-fr

    In French we call it :language-fr:`sirop de sureau`.

br
  Line break

  For complete paragraphs, use a line-block:

    | Simply Orange Juice Company
    | Apopka, FL 32703
    | U.S.A.

  rST does not natively support an exceptional hard line break in a floating
  paragraph. You may use a HTML-only substitution.

    .. |br| raw:: html

      <br />

    I want to break this line after the colon: |br| but allow other
    linebreaks in this paragraph according to the width of the containing
    block.

wbr
  Line breaking opportunity

  Not supported by rST. You may use a ZWSP character (u200B), literal or via
  a substitution.

    .. |wbr| unicode:: 0x200B

    www.simply​​\ |wbr|\orange\ |wbr|\juice.com

.. [#attribute-optional] Would gain from support for attributes/arguments
   to inline roles. See TODO_

.. [#attribute-required] Requires support for attributes to inline
   roles to make sense.

.. _TODO: https://docutils.sourceforge.io/docs/dev/todo.html#interpreted-text


Indicating Edits
----------------

The `HTML tags for representation of edits to the document`__ and their
reStructuredText equivalents are:

.. class:: description

ins
  Additions [#attribute-optional]_

    .. role:: ins

    This text has "always" been here. :ins:`This text has been inserted.`

    .. container:: ins

      This paragraph has been inserted.

del
  Removed content [#attribute-optional]_

    .. role:: del

    :del:`This text has been deleted`, here is the rest of the paragraph.

    .. container:: del

      This paragraph has been deleted.

__ https://html.spec.whatwg.org/multipage/edits.html
