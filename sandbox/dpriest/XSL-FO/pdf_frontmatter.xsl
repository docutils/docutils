<?xml version="1.0" encoding="utf-8"?>
<!--
    Author: David Priest
    File: DocUtils.xsl
    Date: 2003-Jun-13
    Purpose: DocUtils XML -> DocUtils FO

    Generated frontmatter (TOC, etc)
    Frontmatter chapters are handled in pdf_chapters.xsl
 -->

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:fox="http://xml.apache.org/fop/extensions"
    xmlns:saxon="http://icl.com/saxon"
    xmlns:u="U">  <!-- for local utility functions -->

<!-- LAYOUT DEFAULTS -->
    <xsl:attribute-set name="book_toc_chapterblock">
        <xsl:attribute name="space-before">3pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="book_toc_chapterentry">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align-last">justify</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="book_toc_sectionentry">
        <xsl:attribute name="font-family">OfficinaSansBook,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">normal</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>

        <xsl:attribute name="font-size">10pt</xsl:attribute>
        <xsl:attribute name="margin-left">2em</xsl:attribute>
    </xsl:attribute-set>

<!-- BOOK TABLE OF CONTENTS -->
    <xsl:template name="frontmatter-toc">
        <fo:block xsl:use-attribute-sets="title-chapter" text-indent="0pt" span="all">
            <xsl:text>Table of Contents</xsl:text>
        </fo:block>
        <xsl:apply-templates select="//document/section/title" mode="book_toc" />
    </xsl:template>

    <!-- title when making a Book's TOC (chapter titles) -->
    <xsl:template match="//document/section/title" mode="book_toc">
        <fo:block xsl:use-attribute-sets="book_toc_chapterblock">
            <fo:marker marker-class-name="chapter"><fo:inline xsl:use-attribute-sets="marker_text-defaults"><xsl:value-of select="./text()" /></fo:inline></fo:marker>
                <fo:block xsl:use-attribute-sets="book_toc_chapterentry">
                    <fo:basic-link internal-destination="{../@id}" xsl:use-attribute-sets="internal_link-defaults">
                        <xsl:value-of select="./text()" />
                        <fo:leader leader-pattern="space" /><fo:page-number-citation ref-id="{../@id}"/>
                    </fo:basic-link>
                </fo:block>
                <xsl:apply-templates select="../section/title" mode="book_toc" />
        </fo:block>
    </xsl:template>

    <!-- title when making a Book's TOC (subsection titles) -->
    <xsl:template match="//document/section/section/title" mode="book_toc">
        <fo:block xsl:use-attribute-sets="book_toc_sectionentry">
            <xsl:value-of select="./text()" />
        </fo:block>
    </xsl:template>
</xsl:stylesheet>
