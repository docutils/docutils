<?xml version="1.0" encoding="utf-8"?>
<!--
    Author: David Priest
    Date: 2003-Jul-22
    Purpose: DocUtils XML -> DocUtils FO

    Chapters
 -->

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:fox="http://xml.apache.org/fop/extensions"
    xmlns:saxon="http://icl.com/saxon"
    xmlns:u="U"> <!-- for local utility functions -->

<!-- PROCESS FRONTMATTER SECTIONS -->
    <xsl:template match="//document/section" mode="frontmatter">
        <!-- title -->
        <xsl:apply-templates select="title" mode="fm_ttl_pg" />
        <!-- chapter table of contents -->
        <fo:block xsl:use-attribute-sets="chapter-toc">
            <xsl:apply-templates select="./section/title" mode="chapter_toc" />
        </fo:block>
        <!-- rule -->
        <fo:block xsl:use-attribute-sets="chapter-rule">
            <fo:leader rule-thickness="0pt" leader-pattern="rule" />
        </fo:block>
        <!-- process the rest of the frontmatter section -->
        <fo:block>
            <xsl:apply-templates select="child::*[name()!='title']"/>
        </fo:block>
    </xsl:template>

<!-- PROCESS CHAPTERS -->
    <xsl:template match="//document/section">
        <!-- title -->
        <xsl:apply-templates select="title" mode="ch_ttl_pg" />
        <!-- chapter table of contents -->
        <fo:block xsl:use-attribute-sets="chapter-toc">
            <xsl:apply-templates select="./section/title" mode="chapter_toc" />
        </fo:block>
        <!-- rule -->
        <fo:block xsl:use-attribute-sets="chapter-rule">
            <fo:leader rule-thickness="0pt" leader-pattern="rule" />
        </fo:block>
        <!-- introductory paragraphs -->
        <xsl:if test="paragraph">
            <fo:block break-after="page">
                <xsl:apply-templates select="paragraph" />
            </fo:block>
        </xsl:if>
        <!-- process the rest of the chapter -->
        <fo:block>
            <xsl:apply-templates select="section"/>
        </fo:block>
    </xsl:template>

    <!-- title when making a Frontmatter/Chapter's TOC (subsection titles) -->
    <xsl:template match="title" mode="chapter_toc">
        <fo:block>
            <fo:list-block>
                <fo:list-item>
                    <fo:list-item-label end-indent="body-start()">
                        <fo:block xsl:use-attribute-sets="chapter_toc_entry">
                            <fo:basic-link internal-destination="{../@id}" xsl:use-attribute-sets="internal_link-defaults">
                                <xsl:value-of select="./text()" />
                            </fo:basic-link>
                        </fo:block>
                    </fo:list-item-label>
                    <fo:list-item-body>
                        <fo:block xsl:use-attribute-sets="chapter_toc_pagenum">
                            <fo:page-number-citation ref-id="{../@id}"/>
                        </fo:block>
                    </fo:list-item-body>
                </fo:list-item>
            </fo:list-block>
        </fo:block>
    </xsl:template>

</xsl:stylesheet>
