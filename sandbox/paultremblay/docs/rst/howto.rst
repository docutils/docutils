
^^^^^^^^^^^^^^^^^^^^^^^^
HOWTO: Docutils2fo 0.6
^^^^^^^^^^^^^^^^^^^^^^^^

..  $Id: howto.rst 7131 2011-09-26 19:27:15Z paultremblay $ 

.. contents::

================
Convert to XML
================

In one line::

 rst2xml.py --strip-comments --trim-footnote-reference-space <file.rst> | fop -xml - -xsl xsl_fo/docutils_to_fo.xsl -pdf out.pdf

===========
Customize
===========

In order to change the defaults of the stylesheets, you will need to create a
new stylesheet which imports the standard stylesheet, and then process this
stylesheet. 

This new stylesheet changes the margins of the top and bottom to 1.5 inches.::

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

Save this stylesheet with any name you want, and then process it with fop. ::

 rst2xml.py --strip-comments --trim-footnote-reference-space <file.rst> | fop -xml - -xsl custom.xsl -pdf out.pdf


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

=======
Inline
=======

emphasis
=========

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

