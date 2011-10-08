
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

