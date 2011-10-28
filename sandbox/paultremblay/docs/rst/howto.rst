
^^^^^^^^^^^^^^^^^^^^^^^^
HOWTO: Docutils2fo 0.6
^^^^^^^^^^^^^^^^^^^^^^^^

..  $Id: howto.rst 7131 2011-09-26 19:27:15Z paultremblay $ 

.. contents::

================
Convert to PDF 
================

You will need a FO processor to convert to PDF. An open source,
complete implementation, fop, is available at:

http://ci.apache.org/projects/xmlgraphics/fop/snapshots/

In addition, if you want to inclue any math in your document, download jeuclid:

http://jeuclid.sourceforge.net/

If you use ASCII math in your document, you will also have to download
the  asciimathml.py and install it.

https://github.com/favalex/python-asciimathml/blob/master/asciimathml.py

Here is an example of converting file ``file.rst`` to ``out.pdf``.

::


 rst2xml.py --strip-comments --trim-footnote-reference-space file.rst | fop -xml - -xsl xsl_fo/docutils_to_fo.xsl -pdf out.pdf

In this case, rst converts file.rst to XML, and the fop processor both
converts the XML file to FO, and then processes that file. You can
achieve the same result in several steps:

1. ``rst2xml.py --strip-comments --trim-footnote-reference-space
   file.rst file.xml``

2. ``xsltproc xsl_fo/docutils_to_fo.xsl file.xml > file.fo``

3. ``fop -fo file.fo -pdf out.pdf``

The script ``docutils_to_fo.sh`` combines several of these steps to make
converstion easier, and adds a few nice features, but it functions
more of a convenience than as a necessary tool; one does not need it
to convert. It is a work in progress, and serves as an example of how
one could writes his own simple script to convert to PDF. The
essential conversion utilities are the FO processor, and the XSL
stylesheets themselves.

===========================
Post Processing a Document
===========================

You may need to further process the XML document created
by rst2xml.py before converting it to FO in two cases: to include raw
XML, and to include MathML. In both cases, use the ``rstxml2xml.py``
script included in the scripts folder::

 rst2xml.py --strip-comments --trim-footnote-reference-space file.rst\
 |rstxml2xml.py | fop -xml - -xsl xsl_fo/docutils_to_fo.xsl -pdf out.pdf

Install the script in the normal way::

 python setup.py install

The script can also be used if you wish to convert the document in
steps.

1. ``rst2xml.py --strip-comments --trim-footnote-reference-space
   file.rst file.xml``

2. ``rstxml2xml.py file.xml > file_fixed.xml``

3. ``xsltproc xsl_fo/docutils_to_fo.xsl file_fixed.xml > file.fo``

4. ``fop -fo file.fo -pdf out.pdf``

Including Raw XML
------------------

If you have included raw XML, such as::

.. raw:: XML

 <math ....

 <!--etc-->

The XML will be converted to plain text, unless you use the
``rstxml2xml.py`` script.


Converting ASCII MathML to MathML
----------------------------------

If you have included ASCII MathML in your document, you should also
use the ``rstxml2xml.py`` script. If you do not post process the
document, the math will appear as ASCII text. If you post process it,
the equations will appear formatted. 

===========
Customize
===========

In order to change the defaults of the stylesheets, you will need to create a
new stylesheet which imports the standard stylesheet, and then process this
stylesheet. 

As an example, consider that you might want to change your top and
bottom margins from the default fo 1 inch to 1.5 inches. First, create
the following stylesheet::

 <xsl:stylesheet 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     xmlns:fo="http://www.w3.org/1999/XSL/Format"
     version="1.1"
     >
     <xsl:import href="xsl_fo/docutils_to_fo.xsl"/>
     <xsl:attribute-set name="page-margins">
         <xsl:attribute name="margin-top">1.5in</xsl:attribute>
         <xsl:attribute name="margin-bottom">1.5in</xsl:attribute>
     </xsl:attribute-set>
 </xsl:stylesheet>

Save this stylesheet with any name you want. In this case, I've saved
as custom.xsl. Next, I process the document in the normal way, except
that I used ``custom.xsl`` as my stylesheet instead of
``docutils_to_fo.xsl`` ::

 rst2xml.py --strip-comments --trim-footnote-reference-space <file.rst> | fop -xml - -xsl custom.xsl -pdf out.pdf

Attribute Sets
---------------

When you import the docutils_to_fo.xsl stylesheet, you also import all
of its rules, except the ones you re-write. In the example above, the
attribute set in docutils_to_fo.xsl looks like this::

  <xsl:attribute-set name="page-margins">
      <xsl:attribute name="margin-left">1.0in</xsl:attribute>
      <xsl:attribute name="margin-right">1.0in</xsl:attribute>
      <xsl:attribute name="margin-top">1.0in</xsl:attribute>
      <xsl:attribute name="margin-bottom">1.0in</xsl:attribute>
  </xsl:attribute-set>

I changed the attribute for ``margin-top`` and ``margin-bottom``, but
left the ``margin-right`` and ``margin-left`` unchanged. You can also
add to attribute sets. In this case, the ``page-margins`` attribute
gets applied to the sequence of pages--practically the whole document.
So in order to change the font size to 14 points,  I could
create the attribute set as::

     <xsl:attribute-set name="page-margins">
         <xsl:attribute name="margin-top">1.5in</xsl:attribute>
         <xsl:attribute name="margin-bottom">1.5in</xsl:attribute>
         <xsl:attribute name="font-size">14pt</xsl:attribute>
     </xsl:attribute-set>

Most of the customization occurs  through attribute sets. If you are
familiar with CSS, then the syntax will look very similar, and is in
many cases exactly the same. If you have doubts, check out the FO
specs online.

Other Ways to Customize
------------------------

The other ways to customize are to re-write an entire template rule,
change a parameter, or, in one case, change a variable. All these
methods are explained below.


====================
Headers and Footers
====================

Create a header
---------------

In the *document* (not the config file), use the following::

 .. header:: 

    A Christmas Carol 

The header will appear on every page.

Create a footer
---------------

In the *document* (not the config file), use the following::

 .. footer:: 

    Charles Dickens 

Create a page number in a header or footer
--------------------------------------------

(NOT SURE I WANT TO IMPLEMENT THIS)

In the *document* (not the config file), use the following::

 .. role:: page-num
    
 .. footer:: 

    Charles Dickens.
    
    :page-num:`1`

Page numbers will be generated automatically.

Create space for a header or footer
------------------------------------

The defaults should create enough space for headers or footers. If you want to
change the defaults, use::


    <xsl:attribute-set name="region-before">
        <xsl:attribute name="extent">1in</xsl:attribute>
    </xsl:attribute-set>



Change space between header and body text
------------------------------------------


::

    <xsl:variable name="region-body-margin-top">.5in<xsl:variable>
    <xsl:variable name="region-body-margin-bottom">.5in<xsl:variable>

=======================
Footnotes and Endnotes
=======================

Changing the default footnote separator
----------------------------------------

Rewrite the named template `make-footnote-separator`::


    <!--gets rid of separator--> 
    <xsl:template name="make-footnote-separator"/>

Changing the size of the label
--------------------------------

The attribute set "default-footnote-label-inline" sets the formatting of the
label in the text::

    <!--changes size form 8 to 10 pts-->
    <xsl:attribute-set name="default-footnote-label-inline">
        <xsl:attribute name="font-size">10pt</xsl:attribute>
    </xsl:attribute-set>

Changing the space between label and footnote at bottom of page
----------------------------------------------------------------

::

    <xsl:attribute-set name="footnote-list-block">
        <xsl:attribute name="provisional-distance-between-starts">18pt</xsl:attribute>
    </xsl:attribute-set>

Changing space between footnotes
---------------------------------

::

    <xsl:attribute-set name="footnote-list-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

Note: Use space-before.conditionality ="retain" to set space between first
footnote and text. Or, set `space-after` in the footnote separator text, and
use space-after.conditionality ="retain".

Changing space between paragraphs in footnotes
----------------------------------------------
::

    <xsl:attribute-set name="footnote-paragraph-block">
        <xsl:attribute name="space-before">15pt</xsl:attribute>
    </xsl:attribute-set>

==========
Dedication
==========

Putting the dedication on its own page
---------------------------------------

The default template for the dedication is::

    <xsl:template match="topic[@classes='dedication']">
        <xsl:apply-templates/>
    </xsl:template>

Change this to::

    <xsl:template match="topic[@classes='dedication']">
        <fo:block break-before = "page" break-after="page">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

Dedication Title
------------------

Change the `dedication-title-block` attribute set::

    <xsl:attribute-set name="dedication-title-block">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

Dedication paragraphs
---------------------

To change the characteristics of the paragraphs of the dedication, use the
`dedication-paragraph-block` and `dedication-first-paragraph-block`.

::


    <xsl:attribute-set name="dedication-paragraph-block">
        <xsl:attribute name="font-style">italic</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="dedication-first-paragraph-block"
        use-attribute-sets = "dedication-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>


==========
Abstract
==========

Putting the abstract on its own page
---------------------------------------

The default template for the abstract is::

    <xsl:template match="topic[@classes='abstract']">
        <xsl:apply-templates/>
    </xsl:template>

Change this to::

    <xsl:template match="topic[@classes='abstract']">
        <fo:block break-before = "page" break-after="page">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

Abstract Title
------------------

Change the `abstract-title-block` attribute set::

    <xsl:attribute-set name="abstract-title-block">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

Abstract paragraphs
---------------------

To change the characteristics of the paragraphs of the abstract, use the
`abstract-paragraph-block` and `abstract-first-paragraph-block`.

::


    <xsl:attribute-set name="abstract-paragraph-block">
        <xsl:attribute name="font-style">italic</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="abstract-first-paragraph-block"
        use-attribute-sets = "abstract-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>


====
TOC
====

Putting toc on its own page
-----------------------------

For a break before, use the break-before = page in the attribute set
`toc-title-block`::

    <xsl:attribute-set name="toc-title-block">
        <xs:attribute name="break-before">page</xs:attribute>
    </xsl:attribute-set>


Use a page break in the element that comes after it. 

Another way to put the TOC on its own page (besides using a whole new page
run, the preferred method for more involved documents), is to rewrite the
matching template. The default is::

    <xsl:template match="topic[@classes='contents']">
        <xsl:apply-templates/>
    </xsl:template>

Rewrite this to::

    <xsl:template match="topic[@classes='contents']">
        <fo:block break-before = "page" break-after="page">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


Formatting the title
-----------------------

Use the attribue set `toc-title-block`:: 

    <xsl:attribute-set name="toc-title-block">
        <xs:attribute name="text-align">left</xs:attribute>
    </xsl:attribute-set>

Setting the defaults on each entry
-------------------------------------

Use the `toc-entry-defaults-block` to set properties for all of the toc entries
at once::

    <xsl:attribute-set name="toc-entry-defaults-block">
        <xsl:attribute name="space-after">3pt</xsl:attribute>
        <xsl:attribute name="text-align-last">justify</xsl:attribute>
    </xsl:attribute-set>


Formatting the entries
------------------------

Use the attribute-set `toc-level1/2...-block`::


    <xsl:attribute-set name="toc-level1-block" >
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-level2-block" >
        <xsl:attribute name="start-indent">10mm</xsl:attribute>
    </xsl:attribute-set>

    <!--etc-->

Format the toc numbers
-----------------------

The format of the numbers for toc entry takes the same format as the
section numbers. See section numbers.

=========
Sections
=========

Formatting titles
-------------------

Use the attribute-sets ``'title-level1-block'``, ``'title-level1-block'``,  etc, to
format the titles for each section. Docutils to fo allows sections to go 7
levels deep. Headings are blocks and can take any property of a block. 

::

    <xsl:attribute-set name="default-section-title-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="keep-with-next">always</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level1-block" use-attribute-sets="default-section-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">16</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level2-block" use-attribute-sets="default-section-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">14</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>


Creating section numbers
-------------------------

At the start of the document, put::

 .. sectnum::

Formatting section numbers
---------------------------

Use the ``'parm#'`` identifier plus the ``'number-format'`` to format the
section numbers. The value for formatting can take a combination of
punctuation and numbers, letters, or Roman numberals

::


 <xsl:param name="number-section1">1</xsl:param>
 <xsl:param name="number-section2">.1</xsl:param>
 <xsl:param name="number-section3">.1</xsl:param>
 <xsl:param name="number-section4">.1</xsl:param>
 <xsl:param name="number-section5">.1</xsl:param>
 <xsl:param name="number-section6">.1</xsl:param>
 <xsl:param name="number-section7">.1</xsl:param>
 <xsl:param name="number-section8">.1</xsl:param>
 <xsl:param name="number-section9">.1</xsl:param>

 # (I.), (II.), (III.), etc
 heading1.number-format = (I.)

 # i.), ii.), etc 
 heading2.number-format = i.)

 # .1., .2., .3., etc
 heading3.number-format = .1.

 # a, b, c, etc
 heading4.number-format = a

 # A, B, C., etc
 heading5.number-format = A

=============
Body Elements
=============

Changing characteristics of paragraphs
---------------------------------------

::


    <xsl:attribute-set name="paragraph-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>


Changing characteristics of first paragraphs
--------------------------------------------

::

    <xsl:attribute-set name="first-paragraph-block" use-attribute-sets="paragraph-block">
    </xsl:attribute-set>

Block quote
-----------

The default template is:

::

    <xsl:template match="block_quote[not(@classes)]">
        <xsl:apply-templates/>
    </xsl:template>

To change, (to put space ater, for example)::

    <xsl:template match="block_quote[not(@classes)]">
        <xsl:apply-templates/>
        <fo:block space-after="24pt"/>
    </xsl:template>

    <!--or-->

    <xsl:template match="block_quote[not(@classes)]">
        <fo:block space-after="24pt">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

Changing characteristics of pargraphs in a Quote 
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the `block-quote-paragraph-block` attribute set::

    <xsl:attribute-set name="block-quote-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="start-indent">20mm</xsl:attribute>
        <xsl:attribute name="end-indent">20mm</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="block-quote-first-paragraph-block" use-attribute-sets="block-quote-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

Changing characteristics of the attribution for a quote
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the `block-quote-attributeion-block` attribute.

::


    <xsl:attribute-set name="block-quote-attribution-block">
        <xsl:attribute name="text-align">right</xsl:attribute>
    </xsl:attribute-set>


Literal Block
-------------

To change the characteristics of a literal block, use the 
`literal-block` attribute set::

    <xsl:attribute-set name="literal-block">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
        <xsl:attribute name="font-size">8</xsl:attribute>
        <xsl:attribute name="white-space">pre</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

Transition Block
------------------

To change the characteristics of a transition block, use the 
`transition-block` attribute set::

    <xsl:attribute-set name="transition-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

Document Title
--------------

To change the characteristics of the document title, use the 
`document-title-block` attribute set::


    <xsl:attribute-set name="document-title-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="font-size">24pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

Document Subtitle
-------------------

To change the characteristics of the document subtitle, use the 
`document-subtitle-block` attribute set::


    <xsl:attribute-set name="document-subtitle-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="font-size">18pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

============
Bullet List
============


Formatting the bullet list
----------------------------

Use the attribute set  ``'bullet-list-block'`` and
``'bullet-level2-list-block'`` property to format the space after and before,
the left and right indent, and any other property you want to set on the list,
such as font for font-size::

    <xsl:attribute-set name="bullet-list-block" >
        <xsl:attribute name="start-indent">5mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">5mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="bullet-level2-list-block" >
        <xsl:attribute name="start-indent">15mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">5mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

To format space between bullets and text, change the attribute
``'provisional-distance-between-starts'``.


To format space between items
-------------------------------

Use the ``'bullet-list-item'`` and ``'bullet-level2-list-item'`` attribute set.

::


    <xsl:attribute-set name="bullet-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>


    <xsl:attribute-set name="bullet-level2-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>

Choosing the text for the bullet
-----------------------------------

Use the parameter ``'bullet-list'`` and ``'bullet-list-level2'`` to change the default bullet::

 <xsl:param name="bullet-text">&#x2022;</xsl:param>
 <xsl:param name="bullet-text-level2">&#x00B0;</xsl:param>


Formatting the  paragraphs
----------------------------

Use the ``'bullet-list-item-body-block'`` attribute set to format the text of
the bullet list. This identifier can take any block property::


    <xsl:attribute-set name="bullet-list-item-body-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>


================
Enumerated List
================


Formatting the enumerated list
--------------------------------

Use the attribute set  ``'enumerated-list-block'`` and
``'enumerated-level2-list-block'`` property to format the space after and
before, the left and right indent, and any other property you want to set on
the list, such as font for font-size::

    <xsl:attribute-set name="enumerated-list-block" >
        <xsl:attribute name="start-indent">5mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">5mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-level2-list-block" >
        <xsl:attribute name="start-indent">15mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">10mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

To format space between enumerateds and text, change the attribute
``'provisional-distance-between-starts'``.


To format space between items
-------------------------------

Use the ``'enumerated-list-item'`` and ``'enumerated-level2-list-item'`` attribute set.

::

    <xsl:attribute-set name="enumerated-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>


    <xsl:attribute-set name="enumerated-level2-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>


Formatting the  paragraphs
----------------------------

Use the ``'enumerated-list-item-body-block'`` attribute set to format the text of
the enumerated list. This identifier can take any block property::


    <xsl:attribute-set name="enumerated-list-item-body-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

===========
Line Blocks
===========

Formatting the entire line block
---------------------------------

Use the ``'outer-line-block'`` attribute-set to format the entire line block.
This identifier can take any block property::

    <xsl:attribute-set name="outer-line-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the lines
----------------------

The lines have the attribute-set ``'level1-line-block'``, ``'level2-line-block'`` and
so fourth. Each level indicates how many levels the line is nested.
Lines may be nesed up to 5 levels deep. It makes sense to set overall
properties with the ``'outer-line-block'`` attribute-set, and to use the
``'evel#-line-block'`` to set the indents of for each level::

    <xsl:attribute-set name="level1-line-block">
        <xsl:attribute name="start-indent">10mm</xsl:attribute>
    </xsl:attribute-set>
    
    <xsl:attribute-set name="level2-line-block">
        <xsl:attribute name="start-indent">20mm</xsl:attribute>
    </xsl:attribute-set>

Create a stanza title
----------------------

In order to create a title for a stanza, in the *document* (not the
configuration file) include the line in a title_reference::

 | `stanza title 1` 
 | A one, two, a one two three four
 |
 | `stanza title 2`
 | Half a bee, philosophically,
 |     must, *ipso facto*, half not be.
 | But half the bee has got to be,
 |     *vis a vis* its entity.  D'you see?
 |
 | `stanza title 3`
 | But can a bee be said to be
 |     or not to be an entire bee,
 |         when half the bee is not a bee,
 |             due to some ancient injury?
 |
 | Singing...

Formatting the stanza title
-----------------------------

Use the ``'stanza-title-block'`` attribute-set to format the stanza title::

    <xsl:attribute-set name="stanza-title-block">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="space-before">12</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

You cannot do any formatting with a title reference (the text between
the \`\`). If you need to do inline markup on part of a stanza title,
only put the \`\` around the part that does not need the markup::

 
 | *stanza title* `3` 
 | But can a bee be said to be

If you need to format the entire stanza title, use the following work
around::


 .. role:: title
 
 | *stanza title 3* :title:`x` 
 | But can a bee be said to be
 |     or not to be an entire bee,
 |         when half the bee is not a bee,
 |             due to some ancient injury?

Number lines
------------

In order to number the lines in a verse, import the number_verse.xsl
stylehseet, and set the parameter ``'number-verse'`` to the appropriate
increment.

::

    <!--numbers every 5th line-->
    <xsl:import href="xsl_fo/docutils_to_fo.xsl"/>
    <xsl:import href="xsl_fo/custom/number_verse.xsl"/>
    <xsl:param name="number-verse">5</xsl:param>

Make numers closer to line
----------------------------

By default, docutils to fo puts the number to the very right of the
margin. Set the attribute ``'right-indent'`` to a positive number to make the
numbers appear closer to the lines::

    <xsl:attribute-set name="outer-line-block">
        <xsl:attribute name="right-indent">20mm</xsl:attribute>
    </xsl:attribute-set>

Keeping the lines on the same page
-----------------------------------

If the line block is relatively short, use the ``'keep-on-same-page'``
property.

::

    <xsl:attribute-set name="outer-line-block">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>


If the line block is long, using this property could lead to
huge space on a page.

.. note a work around is to create a completely new stanza, and use
.. keep-on-same-page property. 

Creating space between stanzas
-------------------------------

Use a blank line to control the space between stanzas. There is no
othe way to control space. The rst2xml.py utility marks a new set of
line blocks when it detects a new indentation. In contrast, real verse
is marked by the space between stanzas.


=================
Definition List
=================

Formatting the definition list
-------------------------------

Use the attribute set ``'definition-list-block'`` to change the
characteristics of the entire definition.

::

    <xsl:attribute-set name="definition-list-block" >
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting space between items
-------------------------------

An item consists of both the term and definition. Use the
``'definition-list-item-block'`` attribute set.

::

    <xsl:attribute-set name="definition-list-item-block" >
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the term
---------------------


Use the ``'definition-term-block'`` to change the properties of the term, such
as the space below::

    <xsl:attribute-set name="definition-term-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute> 
    </xsl:attribute-set>


Formatting the definition
--------------------------

The definition can consist of more than one paragraph. To format each of these
paragraphs, and the space before or after, use the ``'definition-block'`` attribute set::

    <xsl:attribute-set name="definition-block">
    </xsl:attribute-set>

Formatting the classifier
-------------------------

Use the ``'classifier-inline'`` attribute set to format the classifier::

    <xsl:attribute-set name="classifier-inline">
	<xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

Formatting the  paragraphs
---------------------------

The ``'definition-list-paragraph'`` attribute-set formats the parapgraphs in
the definition::

    <xsl:attribute-set name="definition-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
        <xsl:attribute name="start-indent">30pt</xsl:attribute>
    </xsl:attribute-set>

============
Field List
============


Formatting the field list
----------------------------

Use the ``'field-list-block'`` attribute set to format the space after and
before, the left and right indent, and any other property you want to
set on the list, such as font for font-size::

    <xsl:attribute-set name="field-list-block" >
        <xsl:attribute name="start-indent">0mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">30mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

To format space between field and text change the
``'provisional-distance-between-starts'`` attribute::


        <xsl:attribute name="provisional-distance-between-starts">40mm</xsl:attribute>


format items
-------------------------------

Use the ``'field-list-items'`` attribute set.

::


    <xsl:attribute-set name="field-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>

Formatting the field names
---------------------------

Use the ``'field-list-item-label-block'`` attribute-set, which can take any inline
properties::


    <xsl:attribute-set name="field-list-item-label-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute> 
    </xsl:attribute-set>


Formatting the  paragraphs
----------------------------

Use the ``'field-body-block'`` attribute-set to format the text of
the bullet list. This identifier can take any block property::

    <xsl:attribute-set name="field-body-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

Note that using the ``'space-before'`` property has the same effect as
controlling the space between each paragraph, without putting unwated space
before the first paragraph. 

============
Option List
============

Formatting the option list format
-----------------------------------

The option list can either be formatted as a list, with the options as
labels to the left of the description; or as a definition list, with
the options serving as the terms, and the descriptions in a paragraph
right below. For an option list with lenghty options, a definition
list may work better.

By default, the stylesheets process the options list as a list. If you wish to
format them as a definition list, use the following::

    <xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

        <xsl:import href="xsl_fo/docutils_to_fo.xsl"/>
        <xsl:import href="xsl_fo/custom/option_list_as_definition.xsl"/>
    
    </xsl:stylesheet>

Default (as list)
------------------


Choosing the options separator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, the docutils to FO convertor uses a comma to separate
options. To change the default, use the ``'options-separator'``
parameter::

    <xsl:param name="options-separator">| </xsl:param>


Formatting the option list
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the ``'option-list-block'`` attribute-set to format the space after and
before, the left and right indent, and any other property you want to
set on the list, such as font for font-size::

    <xsl:attribute-set name="option-list-block">
        <xsl:attribute name="start-indent">0mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">50mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

To format space between option and text, chance the
``'provisional-distance-between-starts'`` attribute::

 <xsl:attribute name="provisional-distance-between-starts">40mm</xsl:attribute>


Formating items
^^^^^^^^^^^^^^^^

Use the ``'option-list-item'`` attribute set.

::


    <xsl:attribute-set name="option-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>
 

Formatting the options without the argument
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the ``'option-inline'`` attribute-set to format only the option without the
arguments of the options. This identifier  can take any inline
properties::


    <xsl:attribute-set name="option-inline">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
    </xsl:attribute-set>

.. option-group-block

Formatting the arguments of the options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the ``'option-argument-inline'`` attribute-set to format just the option of
the arugment. This identifier  can take any inline properties::


    <xsl:attribute-set name="option-argument-inline">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
	<xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>


Formatting the description
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the ``'option-list-item-body'`` attribue set to format the text of
the option list.


Formatting the paragraphs
^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the ``'option-list-item-body-block'`` attribute-set to format the text of
the list. This identifier can take any block property::

    <xsl:attribute-set name="option-list-item-body-block">
    </xsl:attribute-set>

Note that using the ``'space-before'`` property has the same effect as
controlling the space between each paragraph, without putting unwated space
before the first paragraph. 


As definition list
------------------

Formatting the option list
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the ``'option-list-definition-block'`` to format the entire option list::

    <xsl:attribute-set name="option-list-definition-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting each item
^^^^^^^^^^^^^^^^^^^^^

Use the attribute-set ``'option-list-item-block'``.

::

    <xsl:attribute-set name="option-list-item-block">
        <xsl:attribute name="space-before">8pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the options
^^^^^^^^^^^^^^^^^^^^^^^^

Use the ``'option-group-block'`` attribute set::

    <xsl:attribute-set name="option-group-block">
        <xsl:attribute name="keep-with-next">always</xsl:attribute>
    </xsl:attribute-set>

Formatting the description
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The description contains one or more paragraphs. To format all of these at
once, use the ``'option-list-description-block'`` attribute set::

    <xsl:attribute-set name="option-list-description-block">
        <xsl:attribute name="start-indent">16pt</xsl:attribute>
        <xsl:attribute name="space-before">8pt</xsl:attribute>
    </xsl:attribute-set>

Fomratting the paragraphs in the description
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the ``'option-list-paragraph-block'``::

    <xsl:attribute-set name="option-list-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the options without the argument
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the ``'option-inline'`` attribute-set. (See above for the default
section.)



Formatting the arguments of the options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the ``'option-argument-inline'`` attribute-set. (See above in the default
section.)

=======
Inline
=======

emphasis
---------

Use the `emphasis-inline` attribute set to change the default behavior of the
emphsis element.

::

    <xsl:template match="emphasis">
        <fo:inline xsl:use-attribute-sets="emphasis-inline">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

strong
-------

::

    <xsl:attribute-set name="strong-inline" >
	<xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

reference
----------


::

    <xsl:attribute-set name="basic-link-inline" >
	<xsl:attribute name="text-decoration">underline</xsl:attribute>
        <xsl:attribute name="color">blue</xsl:attribute>
    </xsl:attribute-set>

literal
--------


::

    <xsl:attribute-set name="literal-inline">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
        <xsl:attribute name="font-size">8</xsl:attribute>
        <xsl:attribute name="white-space">pre</xsl:attribute>
    </xsl:attribute-set>

title-reference
---------------

::

    <xsl:attribute-set name="title-reference-inline" >
	<xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

inline-links
------------

Change the parameter `internalo-link-type` to change the type of hyper link::

 <!--whether to make internal links clickable ('link'), 
 refer to a page number ('page'), or both ('page-link')-->
 <xsl:param name="internal-link-type">link</xsl:param>

============
Admonitions
============

Titles
-------

Each admonishment has its own title formed by the following parameters:

::


    <xsl:param name="attention-title">Attention!</xsl:param>
    <xsl:param name="caution-title">Caution!</xsl:param>
    <xsl:param name="danger-title">!Danger!</xsl:param>
    <xsl:param name="error-title">Error</xsl:param>
    <xsl:param name="hint-title">Hint</xsl:param>
    <xsl:param name="important-title">Important</xsl:param>
    <xsl:param name="note-title">Note</xsl:param>
    <xsl:param name="tip-title">Tip</xsl:param>
    <xsl:param name="warning-title">Warning!</xsl:param>

To change the default titles, simply change the parameters.

Formating the the title blocks
------------------------------------

Each title block inherits its property from the `default-admonition-title-block`
attribute-set.

::

    <xsl:attribute-set name="default-admonition-title-block">
        <xsl:attribute name="space-after">10pt</xsl:attribute>
        <xsl:attribute name="font-size">larger</xsl:attribute>
        <xsl:attribute name="color">red</xsl:attribute>
    </xsl:attribute-set>

The attribute sets for each admonition tite, are logically the name, followed
by the string 'title-block'.

::

    <xsl:attribute-set name="attention-title-block" use-attribute-sets="default-admonition-title-block">
    </xsl:attribute-set>

The exact names are:

 - attention-title-block
 - caution-title-block
 - danger-title-block
 - error-title-block
 - hint-title-block
 - important-title-block
 - note-title-block
 - tip-title-block
 - admonition-custom-title-block
 - warning-title-block

Formating the the admonition blocks
------------------------------------

Each admonition is wrapped in a block. Each block inherits its characteristics
from the default::



    <xsl:attribute-set name="default-admonition-outer-block">
        <xsl:attribute name="border-style">solid</xsl:attribute>
        <xsl:attribute name="border-width">1px</xsl:attribute>
        <xsl:attribute name="padding">6pt</xsl:attribute>
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

Changing the attributes in this attribute set changes them for all
admonitions. 

Each admonition has its own attribute-set, the name of the admonition followed
by the string '-block.'::

    <xsl:attribute-set name="attention-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="caution-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>

    <!--etc-->

The names of these blocks are:

 - attention-block
 - caution-block
 - danger-block
 - error-block
 - hint-block
 - important-block
 - note-block
 - tip-block
 - admonition-custom-block
 - warning-block


Formatting the paragraphs within the admonition block
-------------------------------------------------------
    
Use the attribute-set `admonition-paragraphs-block` to format paragraphs::

    <xsl:attribute-set name="admonition-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

================
Body Directives
================

topic
------

Formatting the complete block
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::


    <xsl:attribute-set name="topic-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the title
^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="topic-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the paragraphs
^^^^^^^^^^^^^^^^^^^^^^^^^

sidebar
-------


Unfortunately, sidebars can't be produced in XSLFO, at least not easily with
the curren renderers. For that reason, the stylesheets render a sidebar
similar to a block quote, not on the side of the page.

Formatting the complete block
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="sidebar-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="background-color">#FFFFF0</xsl:attribute>
        <xsl:attribute name="padding">6pt</xsl:attribute>
        <xsl:attribute name="start-indent">10mm</xsl:attribute>
        <xsl:attribute name="end-indent">40mm</xsl:attribute>
    </xsl:attribute-set>

Formatting the title
^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="sidebar-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the subtitle
^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="sidebar-subtitle-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the paragraphs
^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="sidebar-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

Rubric
------

::

    <xsl:attribute-set name="rubric-block">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="font-size">larger</xsl:attribute>
        <xsl:attribute name="color">red</xsl:attribute>
    </xsl:attribute-set>

Epigraph
--------

Formatting the complete block
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="epigraph-outer-block">
        <xsl:attribute name="start-indent">20mm</xsl:attribute>
        <xsl:attribute name="end-indent">20mm</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="text-align">right</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

Formatting the paragraphs
^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="epigraph-paragraph-block">
        <xsl:attribute name="start-indent">inherit</xsl:attribute>
        <xsl:attribute name="end-indent">inherit</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>


Formatting the attribution 
^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="epigraph-attribution-block">
        <xsl:attribute name="text-align">right</xsl:attribute>
    </xsl:attribute-set>


Highlights
-----------

Formatting the complete block
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="highlights-outer-block">
        <xsl:attribute name="start-indent">20mm</xsl:attribute>
        <xsl:attribute name="end-indent">20mm</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the paragraphs
^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="highlights-paragraph-block">
        <xsl:attribute name="start-indent">inherit</xsl:attribute>
        <xsl:attribute name="end-indent">inherit</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

Pull Quotes
------------

Formatting the complete block
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="pull-quote-paragraph-block">
        <xsl:attribute name="start-indent">inherit</xsl:attribute>
        <xsl:attribute name="end-indent">inherit</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the paragraphs
^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="pull-quote-first-paragraph-block" use-attribute-sets="block-quote-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the attribution
^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="pull-quote-attribution-block">
        <xsl:attribute name="text-align">right</xsl:attribute>
    </xsl:attribute-set>

Container
---------

Formatting the complete block
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
::

    <xsl:attribute-set name="container-outer-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the paragraphs
^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    <xsl:attribute-set name="container-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

Math
-----

Formatting the complete block
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The math block does not consist of paragraphs. It either consists of literal
text, or MathML markup. To format the contents in either case::

    <xsl:attribute-set name="mathml-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>


====================
Images and Figures
====================

An image conssits of just the image. A figure consists of the image, with an
option for alternative text, a caption, and a legend. The legend can consist
of body elements, such as paragraphs or a table.


Attribute sets for Images and Figures. The image property of
``alt`` and ``target`` are ignored by the stylesheets, since they
have no use in PDF. In addtion, if the ``align`` is set to
``top`` or ``bottom``, both properties that have no meaning for
PDF, the stylesheets will report an error, and if ``strict`` is
set to ``true``, quit.

Likwise, the figure ``figwidth`` property will be ignored, since
there is not way to implement this property directy in FO.

In order to control the scaling, alignment, and width of images
and figures, it is better to use the attribute sets than to try
to set these properties in RST. The regions of 'image', 'figure',
'caption', and 'legend' are all wrapped in blocks. Use the
attribute sets for these blocks to control the properties.

Formatting the entire figure block
-----------------------------------

::

    <xsl:attribute-set name="figure-block">
    </xsl:attribute-set>

Formatting the entire caption block
------------------------------------

::

    <xsl:attribute-set name="figure-caption-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">smaller</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

Formatting the entire legend block
-----------------------------------

::

    <xsl:attribute-set name="figure-legend-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the paragraphs in the legend block
----------------------------------------------

::

    <xsl:attribute-set name="legend-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the first paragraph in the legend block
---------------------------------------------------

::

    <xsl:attribute-set name="legend-first-paragraph-block" use-attribute-sets="legend-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

Formatting the entire image block
----------------------------------

The image block contains no text or othe elements. None-the-less, it is
wrapped in a block that allows formatting, including space before or after,
alignment, and borders.

::
    <xsl:attribute-set name="image-block">
    </xsl:attribute-set>

=======
Tables
=======

Use the attribute-set `table-block-container` to format all the
characteristics of a table.

::

    <xsl:attribute-set name="table-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

Changing the widths of the columns
------------------------------------

The formatter uses the columns values generated by the rst2xml.py
script to determine the width of the columns. In order to change this
default, re-write the table template::

    <!--sets the second two columns to twice the first column-->
    <xsl:template match="table[not(@classes)]/tgroup">
        <fo:table-column column-number="1" column-width="proportional-column-width(5)"/>
        <fo:table-column column-number="2" column-width="proportional-column-width(10)"/>
        <fo:table-column column-number="3" column-width="proportional-column-width(10)"/>
        <xsl:apply-templates/>
    </xsl:template>

Formatting captions
----------------------

::

    <xsl:attribute-set name="caption-block">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="space-before">6pt</xsl:attribute>
        <xsl:attribute name="space-after">6pt</xsl:attribute>
    </xsl:attribute-set>

Placement of the caption
--------------------------

Use the parameter to place the the caption either at the top or bottom of the
table::

 <!--changes placement of caption to top of table-->
 <xsl:param name="table-title-placement">bottom</xsl:param>

Formatting the table header
----------------------------

To format all of the cells in the header, use the `table-header`
attribute-set.

::

    <xsl:attribute-set name="table-header">
        <!--increases font size-->
        <xsl:attribute name="font-size">14/xsl:attribute>
    </xsl:attribute-set>

Formatting the table header cells
-----------------------------------

Use the ``'table-header-cell'`` attribute-set to format the cells in the
heading. Note that this attribute set inherits the propertis of the
default-cell attribute set (shown below).

::


    <xsl:attribute-set name="table-header-cell" use-attribute-sets="default-cell">
        <xsl:attribute name="border-bottom">solid black 2px</xsl:attribute>
    </xsl:attribute-set>


Formatting the paragraphs in the table header
----------------------------------------------

.. you can probably do all the formatting in table-header-cell

::

    <xsl:attribute-set name="table-header-block">
    </xsl:attribute-set>

Formatting the table body
-------------------------

Use the `table-body` attribute-set to format all of the table body::

    <xsl:attribute-set name="table-body">
    </xsl:attribute-set>


Fromatting the rows
-------------------

The attribute-set `table-row` formats the table row. Note that the defaults
has the attribute "keep-together.within-page" set to "always" so that rows do
not split across pages. 

::

    <xsl:attribute-set name="table-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

Formatting the cells in the body
---------------------------------

The cells are formatted with the `table-cell` attribute-set. This attribute
set inherits its properties from the `default-table-cell`.

::

    <xsl:attribute-set name="table-cell" use-attribute-sets="default-cell">
    </xsl:attribute-set>

    <xsl:attribute-set name="default-cell">
        <xsl:attribute name="border">solid black 1px</xsl:attribute>
        <xsl:attribute name="padding">1em</xsl:attribute>
        <xsl:attribute name="border-collapse">collapse</xsl:attribute>
    </xsl:attribute-set>

.. |RST| replace:: reStructuredText
