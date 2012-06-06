<?xml version="1.0"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:exsl="http://exslt.org/common" xmlns:d="http://docbook.org/ns/docbook" version="1.0" exclude-result-prefixes="exsl d">

<!-- This stylesheet was created by template/titlepage.xsl; do not edit it by hand. -->

<xsl:template name="article.titlepage.recto">
  <xsl:choose>
    <xsl:when test="d:articleinfo/d:title">
      <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:articleinfo/d:title"/>
    </xsl:when>
    <xsl:when test="d:artheader/d:title">
      <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:artheader/d:title"/>
    </xsl:when>
    <xsl:when test="d:info/d:title">
      <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:info/d:title"/>
    </xsl:when>
    <xsl:when test="d:title">
      <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:title"/>
    </xsl:when>
  </xsl:choose>

  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:articleinfo/d:author"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:artheader/d:author"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:info/d:author"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:articleinfo/d:address"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:artheader/d:address"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:info/d:address"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:articleinfo/d:email"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:artheader/d:email"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:info/d:email"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:articleinfo/d:abstract"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:artheader/d:abstract"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="d:info/d:abstract"/>
</xsl:template>

<xsl:template name="article.titlepage">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" font-family="{$title.fontset}">
    <xsl:variable name="recto.content">
      <xsl:call-template name="article.titlepage.before.recto"/>
      <xsl:call-template name="article.titlepage.recto"/>
    </xsl:variable>
    <xsl:variable name="recto.elements.count">
      <xsl:choose>
        <xsl:when test="function-available('exsl:node-set')"><xsl:value-of select="count(exsl:node-set($recto.content)/*)"/></xsl:when>
        <xsl:when test="contains(system-property('xsl:vendor'), 'Apache Software Foundation')">
          <!--Xalan quirk--><xsl:value-of select="count(exsl:node-set($recto.content)/*)"/></xsl:when>
        <xsl:otherwise>1</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="(normalize-space($recto.content) != '') or ($recto.elements.count &gt; 0)">
      <fo:block start-indent="0pt" text-align="center"><xsl:copy-of select="$recto.content"/></fo:block>
    </xsl:if>
    <xsl:variable name="verso.content">
      <xsl:call-template name="article.titlepage.before.verso"/>
      <xsl:call-template name="article.titlepage.verso"/>
    </xsl:variable>
    <xsl:variable name="verso.elements.count">
      <xsl:choose>
        <xsl:when test="function-available('exsl:node-set')"><xsl:value-of select="count(exsl:node-set($verso.content)/*)"/></xsl:when>
        <xsl:when test="contains(system-property('xsl:vendor'), 'Apache Software Foundation')">
          <!--Xalan quirk--><xsl:value-of select="count(exsl:node-set($verso.content)/*)"/></xsl:when>
        <xsl:otherwise>1</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="(normalize-space($verso.content) != '') or ($verso.elements.count &gt; 0)">
      <fo:block><xsl:copy-of select="$verso.content"/></fo:block>
    </xsl:if>
    <xsl:call-template name="article.titlepage.separator"/>
  </fo:block>
</xsl:template>

<xsl:template match="*" mode="article.titlepage.recto.mode">
  <!-- if an element isn't found in this mode, -->
  <!-- try the generic titlepage.mode -->
  <xsl:apply-templates select="." mode="titlepage.mode"/>
</xsl:template>

<xsl:template match="*" mode="article.titlepage.verso.mode">
  <!-- if an element isn't found in this mode, -->
  <!-- try the generic titlepage.mode -->
  <xsl:apply-templates select="." mode="titlepage.mode"/>
</xsl:template>

<xsl:template match="d:title" mode="article.titlepage.recto.auto.mode">
<fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" xsl:use-attribute-sets="article.titlepage.recto.style" keep-with-next.within-column="always" space-before="2in" font-size="20.736pt" space-before.conditionality="retain">
<xsl:call-template name="component.title">
<xsl:with-param name="node" select="ancestor-or-self::d:article[1]"/>
</xsl:call-template>
</fo:block>
</xsl:template>

<xsl:template match="d:author" mode="article.titlepage.recto.auto.mode">
<fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" xsl:use-attribute-sets="article.titlepage.recto.style" space-before="0.5em" font-size="12pt">
<xsl:apply-templates select="." mode="article.titlepage.recto.mode"/>
</fo:block>
</xsl:template>

<xsl:template match="d:address" mode="article.titlepage.recto.auto.mode">
<fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" xsl:use-attribute-sets="article.titlepage.recto.style" text-align="center" font-size="14.4pt">
<xsl:apply-templates select="." mode="article.titlepage.recto.mode"/>
</fo:block>
</xsl:template>

<xsl:template match="d:email" mode="article.titlepage.recto.auto.mode">
<fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" xsl:use-attribute-sets="article.titlepage.recto.style">
<xsl:apply-templates select="." mode="article.titlepage.recto.mode"/>
</fo:block>
</xsl:template>

<xsl:template match="d:abstract" mode="article.titlepage.recto.auto.mode">
<fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" xsl:use-attribute-sets="article.titlepage.recto.style" space-before=".5in" space-before.conditionality="retain" text-align="start" margin-left="0.5in" margin-right="0.5in" font-family="{$body.fontset}">
<xsl:apply-templates select="." mode="article.titlepage.recto.mode"/>
</fo:block>
</xsl:template>

</xsl:stylesheet>

