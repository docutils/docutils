<?xml version="1.0" encoding="utf-8"?>
<!--
    Author: David Priest
    File: DocUtils.xsl
    Date: 2003-Jun-13
    Purpose: DocUtils XML -> DocUtils FO

    Generated endmatter (versions list)
 -->

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:fox="http://xml.apache.org/fop/extensions"
    xmlns:saxon="http://icl.com/saxon"
    xmlns:u="U">  <!-- for local utility functions -->

<!-- Versions List -->
    <xsl:template name="endmatter-versions">
        <fo:block xsl:use-attribute-sets="title-level2">
            <xsl:text>Documentation Version Reference</xsl:text>
        </fo:block>
        <xsl:for-each select="//document">
            <xsl:call-template name="printfilename">
                <xsl:with-param name="FName">
                    <xsl:call-template name="u:docinfo-filename" />
                </xsl:with-param>
            </xsl:call-template>

            <!-- process all sections/subsections/etc of document -->
            <xsl:for-each select="//section">
                <xsl:call-template name="printfilename">
                    <xsl:with-param name="FName">
                        <xsl:call-template name="u:docinfo-filename" />
                    </xsl:with-param>
                </xsl:call-template>
            </xsl:for-each>
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="printfilename">
        <xsl:param name="FName">(filename not known)</xsl:param>

        <!-- discover filename of ancestor -->
        <xsl:variable name="ancestorFName">
            <xsl:call-template name="u:docinfo-filename" >
                <xsl:with-param name="thisNode" select="ancestor::*" />
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="level">
            <xsl:value-of select="count(ancestor::section) - count(self::document) + 1" />
        </xsl:variable>

        <xsl:variable name="revision">
            <xsl:call-template name="u:docinfo-revision" />
        </xsl:variable>

        <!-- show top-level sections OR those that were in a separate file -->
        <xsl:if test="($level &lt; 2) or ($FName != $ancestorFName)">
            <fo:block xsl:use-attribute-sets="docrev_block-defaults">

                <xsl:attribute name="text-indent"><xsl:value-of select="$level*12" />pt</xsl:attribute>

                <!-- title text -->
                <xsl:value-of select="./title/text()" />

                <!-- filename & rev if different from ancestor's -->
                <fo:leader />
                <xsl:if test="$FName != $ancestorFName">
                    <xsl:value-of select="$FName" />
                    <xsl:if test="string-length($revision) != 0">
                        <fo:leader leader-length="1em" />
                        <xsl:text>Rev. </xsl:text>
                        <xsl:value-of select="$revision" />
                    </xsl:if>
                </xsl:if>
            </fo:block>
        </xsl:if>
    </xsl:template>

</xsl:stylesheet>
