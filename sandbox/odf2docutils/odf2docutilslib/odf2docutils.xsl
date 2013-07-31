<?xml version="1.0" encoding="utf-8"?>

<!--
     Copyright (C) 2013 Stefan Merten

     odf2docutils.xsl is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published
     by the Free Software Foundation; either version 2 of the License,
     or (at your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
     General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
     02111-1307, USA.
-->

<!-- ********************************************************************** -->
<!-- ********************************************************************** -->

<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns="http://docutils.sourceforge.net/docs/ref/docutils.dtd"
    xmlns:dom="http://www.w3.org/2001/xml-events"
    xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
    xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"
    xmlns:presentation="urn:oasis:names:tc:opendocument:xmlns:presentation:1.0"
    xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0"
    xmlns:smil="urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0"
    xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"
    xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"
    xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"
    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0">

  <xsl:output
      method="xml"
      version="1.0"
      indent="no"
      doctype-public="+//IDN docutils.sourceforge.net//DTD Docutils Generic//EN//XML"
      doctype-system="http://docutils.sourceforge.net/docs/ref/docutils.dtd"
      encoding="utf-8"/>

  <!-- Top level of some document -->
  <xsl:template
      match="office:text|office:drawing|office:presentation|office:spreadsheet">
    <xsl:element
    	name="document"
    	namespace="">
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- ********************************************************************* -->
  <!-- Presentation -->

  <!-- A page in a presentation. -->
  <xsl:template
      match="draw:page">
    <xsl:element
	name="section"
    	namespace="">
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- The title text in a presentation. -->
  <xsl:template
      match="draw:page/draw:frame[1]/draw:text-box">
    <xsl:apply-templates
	select="text:p">
      <xsl:with-param
	  name="element"
	  select="'title'"/>
    </xsl:apply-templates>
  </xsl:template>

  <!-- The content part of a presentation. -->
  <xsl:template
      match="draw:page/draw:frame[position() &gt; 1]/draw:text-box">
    <xsl:apply-templates
	select="text:list"/>
  </xsl:template>

  <!-- ********************************************************************* -->
  <!-- Lists -->

  <!-- Powerpoint slides employ a wrong nesting. It closes a nested list and
       starts a new one with empty inner nodes. LibreOffice compensates for
       this but sometimes fails.

       This seems to be the result of changing the format in the middle of the
       list.

       Also the ODF specification says "If a list item starts another list
       immediately and does not contain any text, no bullet or number is
       displayed.". Insofar such separated lists are valid.

       Since this is a legal feature of ODF it is output that way. `xml2rst`
       supports such wrongly nested lists starting with V1.1.1 -->

  <xsl:template
      match="text:list">
    <xsl:element
	name="bullet_list"
	namespace="">
      <xsl:attribute
	  name="bullet">
	<xsl:value-of
	    select="'*'"/>
      </xsl:attribute>
      <xsl:apply-templates
	  select="text:list-item"/>
    </xsl:element>
  </xsl:template>

  <xsl:template
      match="text:list-item">
    <xsl:element
	name="list_item"
    	namespace="">
      <xsl:apply-templates
	  select="text:p|text:list"/>
    </xsl:element>
  </xsl:template>  

  <!-- ********************************************************************* -->
  <!-- Text -->

  <!-- Paragraph which may result in various docutils elements. -->
  <xsl:template
      match="text:p">
    <!-- The target docutils element. -->
    <xsl:param
	name="element"
	select="'paragraph'"/>
    <xsl:element
	name="{$element}"
    	namespace="">
      <xsl:value-of
	  select="."/>
    </xsl:element>
  </xsl:template>

  <!-- Suppress all other text content -->
  <xsl:template
      match="text()">
  </xsl:template>

</xsl:stylesheet>
