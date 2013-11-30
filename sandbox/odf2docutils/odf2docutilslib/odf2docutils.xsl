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

  <!-- Name of the input file -->
  <xsl:param
      name="_source_name"/>

  <xsl:output
      method="xml"
      version="1.0"
      indent="no"
      doctype-public="+//IDN docutils.sourceforge.net//DTD Docutils Generic//EN//XML"
      doctype-system="http://docutils.sourceforge.net/docs/ref/docutils.dtd"
      encoding="utf-8"/>

  <!-- Top level of an ODF document besides text -->
  <xsl:template
      match="office:drawing | office:presentation | office:spreadsheet">
    <xsl:element
    	name="document"
    	namespace="">
      <xsl:attribute
	  name="source"
	  namespace="">
	<xsl:value-of
	    select="$_source_name"/>
      </xsl:attribute>
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
  <!-- Text -->

  <!-- Section headers in ODF are flat whereas in Docutils titled sections
       build a structure. This is somewhat complicated to convert and uses some
       modes.

       topHeader
         This mode is used for possible top level headers.
  -->

  <!-- Top level of an ODF text document. -->
  <xsl:template
      match="office:text">
    <xsl:element
    	name="document"
    	namespace="">
      <xsl:attribute
	  name="source"
	  namespace="">
	<xsl:value-of
	    select="$_source_name"/>
      </xsl:attribute>
      <!-- All headers in the main document. -->
      <xsl:variable
	  name="allHeaders"
	  select="text:h"/>
      <!-- Generate context containing only the very first document node. -->
      <xsl:for-each
	  select="*[1]">
	<!-- Output content before the first header. -->
	<xsl:call-template
	    name="generateSectionContent">
	  <!-- Direct content before the first section ends with the first
               header. -->
	  <xsl:with-param
	      name="sectionEndId"
	      select="generate-id($allHeaders[1])"/>
	</xsl:call-template>
      </xsl:for-each>
      <!-- Apply only to the section headers. They care for applying to
           their content. -->
      <xsl:apply-templates
	  select="$allHeaders"
	  mode="topHeader"/>
    </xsl:element>
  </xsl:template>

  <!-- Section header called directly from top level. These section headers may
       have a level > 1 so this needs to be checked. The reason for this is
       that headers of higher levels are used for layout purposes sometimes.
       -->
  <xsl:template
      match="text:h"
      mode="topHeader">
    <!-- Level of the current header. -->
    <xsl:variable
	name="level">
      <xsl:call-template
	  name="outlineLevel"/>
    </xsl:variable>
    <!-- Nodes which logically embed this header. -->
    <xsl:variable
	name="embedders"
	select="preceding-sibling::text:h[@text:outline-level &lt; $level]"/>
    <xsl:if
	test="$level = 1 or not($embedders)">
      <xsl:apply-templates
	  select="."/>
    </xsl:if>
    <!-- If there are embedders do nothing because this node will be called by
         its embedder. -->
  </xsl:template>

  <!-- Convert a header. -->
  <xsl:template
      match="text:h">
    <!-- Level of the current header. -->
    <xsl:variable
	name="level">
      <xsl:call-template
	  name="outlineLevel"/>
    </xsl:variable>
    <!-- Enders of this section are all following headers with same or lower
         level. -->
    <xsl:variable
	name="enders"
	select="following-sibling::text:h[not(@text:outline-level) or @text:outline-level &lt;= $level]"/>
    <!-- Emebdees are all following headers with higher level. -->
    <xsl:variable
	name="embeddees"
	select="following-sibling::text:h[@text:outline-level &gt; $level]"/>
    <xsl:element
	name="section"
    	namespace="">
      <xsl:element
	  name="title"
	  namespace="">
	<xsl:value-of
	    select="."/>
      </xsl:element>
      <!-- Generate context containing the next node. -->
      <xsl:for-each
	  select="following-sibling::*[1]">
	<xsl:call-template
	    name="generateSectionContent">
	  <xsl:with-param
	      name="firstSubsectionId"
	      select="generate-id($embeddees[1])"/>
	  <xsl:with-param
	      name="sectionEndId"
	      select="generate-id($enders[1])"/>
	  <xsl:with-param
	      name="level"
	      select="$level"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>

  <!-- Output section content. Current node is first node of remaining section
       content. -->
  <xsl:template
      name="generateSectionContent">
    <!-- Id of first subsection node after the direct section content or empty
         string for no subsection. Node may be after the one with
         `sectionEndId` in which case it is synonymous to an empty string. -->
    <xsl:param
        name="firstSubsectionId"
	select="''"/>
    <!-- Id of first node after the section or empty string for end of
         document. -->
    <xsl:param
        name="sectionEndId"
	select="''"/>
    <!-- Level of the section. -->
    <xsl:param
        name="level"
	select="0"/>
    <!-- All elements in section and after it. -->
    <xsl:variable
	name="allNodes"
	select=". | following-sibling::*"/>
    <xsl:variable
	name="sectionEndPosition">
      <xsl:call-template
	  name="id2Position">
	<xsl:with-param
	    name="contextNodes"
	    select="$allNodes"/>
	<xsl:with-param
	    name="id"
	    select="$sectionEndId"/>
      </xsl:call-template>
    </xsl:variable>
    <!-- Position of first subsection in all nodes. -->
    <xsl:variable
	name="firstSubsectionPositionAll">
      <xsl:call-template
	  name="id2Position">
	<xsl:with-param
	    name="contextNodes"
	    select="$allNodes"/>
	<xsl:with-param
	    name="id"
	    select="$firstSubsectionId"/>
	<xsl:with-param
	    name="isEndDefault"
	    select="false()"/>
      </xsl:call-template>
    </xsl:variable>
    <!-- Position of first subsection or <= 0 when no subsection exists. -->
    <xsl:variable
	name="firstSubsectionPosition">
      <xsl:choose>
	<xsl:when
	    test="$firstSubsectionPositionAll &lt;= $sectionEndPosition">
	  <xsl:value-of
	      select="$firstSubsectionPositionAll"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of
	      select="0"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!-- The position of the first node after the content. -->
    <xsl:variable
	name="contentEndPosition">
      <xsl:choose>
	<xsl:when
	    test="$firstSubsectionPosition &gt; 0">
	  <!-- Subsection exists - direct content reaches until there. -->
	  <xsl:value-of
	      select="$firstSubsectionPosition"/>
	</xsl:when>
	<xsl:otherwise>
	  <!-- No subsection - direct content reaches until end. -->
	  <xsl:value-of
	      select="$sectionEndPosition"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!-- Output direct content. -->
    <xsl:apply-templates
	select="$allNodes[position() &lt; $contentEndPosition]"/>
    <!-- Output subsections by applying to all headers of subsections. -->
    <xsl:apply-templates
	select="$allNodes[position() &lt; $sectionEndPosition and name() = 'text:h' and @text:outline-level = $level + 1]"/>
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
	  select="text:p | text:list"/>
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
      <xsl:apply-templates
	  select="text() | *"
	  mode="text"/>
    </xsl:element>
  </xsl:template>

  <!-- Consider spans. -->
  <xsl:template
      match="text:span"
      mode="text">
    <xsl:variable
	name="rstStyle">
      <xsl:call-template
	  name="textSpan2RstStyle"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when
	  test="$rstStyle != ''">
	<xsl:element
	    name="{$rstStyle}"
	    namespace="">
	  <xsl:apply-templates
	      select="text() | *"
	      mode="text"/>
	</xsl:element>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates
	    select="text() | *"
	    mode="text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Evaluate a reST style from a span. -->
  <xsl:template
      name="textSpan2RstStyle">
    <xsl:variable
	name="odfStyleName"
	select="@text:style-name"/>
    <!-- TODO Consider @text:class-names -->
    <xsl:if
	test="$odfStyleName != ''">
      <xsl:apply-templates
	  select="//style:style[@style:family = 'text' and @style:name = $odfStyleName]"
	  mode="textSpan2RstStyle"/>
    </xsl:if>
  </xsl:template>

  <!-- Evaluate a reST style from a named style. -->
  <xsl:template
      match="style:style[@style:family = 'text']"
      mode="textSpan2RstStyle">
    <xsl:choose>
      <xsl:when
	  test="style:text-properties[@fo:font-style != 'normal']">
	<xsl:value-of
	    select="'emphasis'"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <!-- Ignore other elements in paragraphs. -->
  <xsl:template
      match="*"
      mode="text">
    <xsl:apply-templates
	select="text() | *"
	mode="text"/>
  </xsl:template>

  <!-- Output text content from paragraphs. -->
  <xsl:template
      match="text()"
      mode="text">
    <xsl:value-of
	select="."/>
  </xsl:template>

  <!-- Suppress all other text content. -->
  <xsl:template
      match="text()">
  </xsl:template>

  <!--**********************************************************************-->
  <!-- Common functions -->

  <!-- Compute the outline level of the current node considering defaults. -->
  <xsl:template
      name="outlineLevel">
    <xsl:choose>
      <xsl:when
	  test="@text:outline-level">
	<xsl:value-of
	    select="@text:outline-level"/>
      </xsl:when>
      <xsl:otherwise>
	<!-- Outline level defaults to 1 according to ODF specification. -->
	<xsl:value-of
	    select="1"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Returns the position of a node in a given context. Returns -1 if
       node is not contained in context. -->
  <xsl:template
      name="id2Position">
    <!-- Context in which the position is to be determined. -->
    <xsl:param
	name="contextNodes"/>
    <!-- Id of the node to get the position of. Must be created by
         `generate-id()`. -->
    <xsl:param
	name="id"/>
    <!-- If `id` is empty the first position after the context is returned
         if this is true. Otherwise 0 will be returned. -->
    <xsl:param
	name="isEndDefault"
	select="true()"/>
    <xsl:choose>
      <xsl:when
	  test="$id != ''">
	<xsl:variable
	    name="position">
	  <xsl:for-each
	      select="$contextNodes">
	    <xsl:if
		test="generate-id(.) = $id">
	      <!-- Node found - return its position. -->
	      <xsl:value-of
		  select="position()"/>
	    </xsl:if>
	  </xsl:for-each>
	</xsl:variable>
	<xsl:choose>
	  <xsl:when
	      test="$position != ''">
	    <xsl:value-of
		select="$position"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <!-- Node not found. -->
	    <xsl:value-of
		select="-1"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:when
	  test="$isEndDefault">
	<xsl:value-of
	    select="count($contextNodes) + 1"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of
	    select="0"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
