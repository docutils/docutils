<?xml version="1.0" encoding="utf-8"?>
<!--
    Author: David Priest
    File: DocUtils.xsl
    Date: 2003-Jun-13
    Purpose: DocUtils XML -> DocUtils FO

    Special Frontmatter includes safeguards, copyright, and such.
    These are may be uniquely formatted; page formatting is handled in pdf.xsl,
    special tag formatting handled here based on mode or predicate.
 -->

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:fox="http://xml.apache.org/fop/extensions"
    xmlns:saxon="http://icl.com/saxon">

<!-- LAYOUT DEFAULTS -->
    <xsl:attribute-set name="admonition-titles" use-attribute-sets="title-defaults">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="font-family">OfficinaSansBold,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="line-height">12pt</xsl:attribute>
        <xsl:attribute name="space-before">7pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">1.0</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">1.0</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="copyright-title" use-attribute-sets="title-defaults">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="font-family">OfficinaSansBold,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="line-height">14pt</xsl:attribute>
        <xsl:attribute name="space-before">10pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">0.75</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">0.75</xsl:attribute>
    </xsl:attribute-set>


<!-- SAFEGUARDS -->
    <!-- special handling of safeguard section title -->
    <xsl:template match="//document/section/title" mode="safeguards">
        <fo:block id="{../@id}" xsl:use-attribute-sets="title-chapter" text-indent="0pt" span="all">
            <fo:marker marker-class-name="chapter"><fo:inline xsl:use-attribute-sets="marker_text-defaults"><xsl:value-of select="./text()" /></fo:inline></fo:marker>
            <xsl:value-of select="text()" />
        </fo:block>
    </xsl:template>

    <!-- special handling of safeguard admonitions -->
    <xsl:template match="admonition" mode="safeguards">
        <fo:block xsl:use-attribute-sets="admonition-titles">
            <xsl:value-of select="./title" />
        </fo:block>
        <xsl:apply-templates select="child::*[name()!='title']" />
    </xsl:template>

<!-- COPYRIGHT -->
    <!-- special handling of copyright title -->
    <xsl:template match="//document/section/title" mode="copyright">
        <fo:block id="{../@id}" xsl:use-attribute-sets="copyright-title">
            <fo:marker marker-class-name="chapter"><fo:inline xsl:use-attribute-sets="marker_text-defaults"><xsl:value-of select="./text()" /></fo:inline></fo:marker>
            <xsl:value-of select="text()" />
        </fo:block>
        <xsl:apply-templates select="../child::*" />
    </xsl:template>

    <!-- everything else handled normally by base xsl templates -->

</xsl:stylesheet>
