<?xml version="1.0" encoding="utf-8"?>
<!--
    Author: David Priest
    File: DocUtils.xsl
    Date: 2003-Jun-13
    Purpose: DocUtils XML -> DocUtils FO

    Titlepage is a special page.
    It has no page number.
    It has absolutely-positioned elements.
    Various docinfo-tagged information is displayed on the titlepage.
    To do: short index on title-page.
 -->

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:fox="http://xml.apache.org/fop/extensions"
    xmlns:saxon="http://icl.com/saxon"
    xmlns:u="U">

<!-- PAGE LAYOUT DEFAULTS -->
    <xsl:attribute-set name="swirlie_position">
        <xsl:attribute name="position">absolute</xsl:attribute>
        <xsl:attribute name="left">0in</xsl:attribute>
        <xsl:attribute name="top">0in</xsl:attribute>
        <xsl:attribute name="height">6in</xsl:attribute>
        <xsl:attribute name="width">8.5in</xsl:attribute>
        <xsl:attribute name="overflow">visible</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="logo_position">
        <xsl:attribute name="position">absolute</xsl:attribute>
        <xsl:attribute name="left">6.5in</xsl:attribute>
        <xsl:attribute name="top">9in</xsl:attribute>
        <xsl:attribute name="height">1in</xsl:attribute>
        <xsl:attribute name="width">1in</xsl:attribute>
        <xsl:attribute name="overflow">visible</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="main_title_position">
        <!-- padding-top because margin-top doesn't work. -->
        <xsl:attribute name="font-family">OfficinaSerifBold</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="font-size">32pt</xsl:attribute>
        <xsl:attribute name="margin-left">1.5in</xsl:attribute>
        <xsl:attribute name="padding-top">2in</xsl:attribute>
        <xsl:attribute name="background-color">transparent</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="sub_title_position">
        <xsl:attribute name="font-family">OfficinaSerifBold</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="font-size">24pt</xsl:attribute>
        <xsl:attribute name="margin-left">1.5in</xsl:attribute>
        <xsl:attribute name="background-color">transparent</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="docinfo_position">
        <xsl:attribute name="font-family">OfficinaSerifBook</xsl:attribute>
        <xsl:attribute name="font-weight">normal</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="overflow">visible</xsl:attribute>
        <xsl:attribute name="margin-bottom">1in</xsl:attribute>
        <xsl:attribute name="margin-left">1.5in</xsl:attribute>
    </xsl:attribute-set>

<!-- DOCUMENT: title -->
    <xsl:template match="title" mode="titlepage">
        <xsl:if test="../image[@class='spirographic-decoration']/@uri">
            <fo:block-container xsl:use-attribute-sets="swirlie_position">
                <fo:block>
                    <fo:external-graphic src="{../image[@class='spirographic-decoration']/@uri}" scaling="uniform" />
                </fo:block>
            </fo:block-container>
        </xsl:if>
        <fo:block xsl:use-attribute-sets="main_title_position">
            <xsl:apply-templates mode="titlepage"/>
        </fo:block>
    </xsl:template>

<!-- DOCUMENT: subtitle -->
    <xsl:template match="subtitle" mode="titlepage">
        <fo:block xsl:use-attribute-sets="sub_title_position">
            <xsl:apply-templates mode="titlepage"/>
        </fo:block>
    </xsl:template>

<!-- DOCUMENT: Swirlie decoration  -->
    <xsl:template match="image[@class='spirographic-decoration']" mode="titlepage" />
    <!-- z-index not yet supported; can't push graphic behind text. Must fiddlefart with title element instead. -->

<!-- DOCUMENT: Logo -->
    <xsl:template match="image[@class='logo']" mode="titlepage">
        <fo:block-container xsl:use-attribute-sets="logo_position">
            <fo:block>
                <fo:external-graphic src="{@uri}" height="2in" width="2in" scaling="uniform" />
            </fo:block>
        </fo:block-container>
    </xsl:template>

<!-- DOCUMENT: docinfo components -->
    <xsl:template match="docinfo" mode="titlepage">
        <!-- hack to force information to bottom-align -->
        <fo:block xsl:use-attribute-sets="docinfo_position">
            <fo:footnote>
                <fo:inline />
                <fo:footnote-body>
                    <xsl:apply-templates mode="titlepage" />
                </fo:footnote-body>
            </fo:footnote>
        </fo:block>
    </xsl:template>
    <xsl:template match="docinfo/address" mode="titlepage">
        <fo:block>
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>
    <xsl:template match="docinfo/author" mode="titlepage">
        <fo:block>
            <xsl:text>Author: </xsl:text>
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>
    <xsl:template match="docinfo/authors" mode="titlepage">
        <fo:block>
            <xsl:text>Authors: </xsl:text>
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>
    <xsl:template match="docinfo/contact" mode="titlepage">
        <fo:block>
            <xsl:text>Contact </xsl:text>
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>
    <xsl:template match="docinfo/copyright" mode="titlepage">
        <fo:block>
            <xsl:text>Copyright </xsl:text>
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>
    <xsl:template match="docinfo/date" mode="titlepage">
        <fo:block>
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>
    <xsl:template match="docinfo/field" mode="titlepage">
        <fo:block>
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>
    <xsl:template match="docinfo/organization" mode="titlepage">
        <fo:block>
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>
    <xsl:template match="docinfo/revision" mode="titlepage">
        <fo:block>
            <xsl:text>Revision </xsl:text>
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>
    <xsl:template match="docinfo/status" mode="titlepage">
        <fo:block>
            <xsl:text>Status: </xsl:text>
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>
    <xsl:template match="docinfo/version" mode="titlepage">
        <fo:block>
            <xsl:text>Version </xsl:text>
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>

<!-- toss out anything else, because it can't be in a title page
     (if only 'cause I don't think it should be on the title page) -->
<xsl:template match="*" mode="titlepage" />

</xsl:stylesheet>
