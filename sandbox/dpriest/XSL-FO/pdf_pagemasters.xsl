<?xml version="1.0" encoding="utf-8"?>
<!--
    Author: David Priest
    Date: 2003-Jul-22
    Purpose: DocUtils XML -> DocUtils FO

    A collection of attributes that define
    automatic page layout and page order.
 -->

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:fox="http://xml.apache.org/fop/extensions"
    xmlns:saxon="http://icl.com/saxon"
    xmlns:u="U"> <!-- for local utility functions -->

<!-- PAGE LAYOUT DEFAULTS -->
    <xsl:attribute-set name="page-defaults">
        <xsl:attribute name="page-width">8.5in</xsl:attribute>
        <xsl:attribute name="page-height">11in</xsl:attribute>
        <xsl:attribute name="margin">0.5in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="header-defaults">
        <xsl:attribute name="margin-bottom">1in</xsl:attribute>
        <xsl:attribute name="margin-top">1in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="body-defaults">
        <xsl:attribute name="margin-bottom">1in</xsl:attribute>
        <xsl:attribute name="margin-top">1in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="header_text-defaults">
        <xsl:attribute name="font-family">OfficinaSerifBook,serif</xsl:attribute>
        <xsl:attribute name="font-weight">normal</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="font-size">10pt</xsl:attribute>
        <xsl:attribute name="line-height">12pt</xsl:attribute>
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="border-bottom">1.5pt solid darkgreen</xsl:attribute>
        <xsl:attribute name="padding">0pt</xsl:attribute>
        <xsl:attribute name="padding-bottom">3pt</xsl:attribute>
        <xsl:attribute name="text-align-last">justify</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="footer_text-defaults">
        <xsl:attribute name="font-family">OfficinaSerifBook,serif</xsl:attribute>
        <xsl:attribute name="font-weight">normal</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="font-size">10pt</xsl:attribute>
        <xsl:attribute name="line-height">12pt</xsl:attribute>
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="border-top">1.5pt solid darkgreen</xsl:attribute>
        <xsl:attribute name="padding">0pt</xsl:attribute>
        <xsl:attribute name="padding-top">3pt</xsl:attribute>
        <xsl:attribute name="text-align-last">justify</xsl:attribute>
    </xsl:attribute-set>

<!-- ACTUAL PAGE LAYOUT -->
<xsl:template match="/">
    <fo:root>
        <!-- PAGE LAYOUT MASTERS -->
        <fo:layout-master-set>
            <!-- Title Page -->
                <fo:simple-page-master master-name="Title Page"
                                       page-width="8.5in"
                                       page-height="11in"
                                       margin="0in"
                                       margin-bottom="1in">
                    <fo:region-body margin="0in" />
                </fo:simple-page-master>
            <!-- (unknown) Special Frontmatter/Endmatter -->
                <fo:simple-page-master master-name="SpecialFM"
                                       page-width="8.5in"
                                       page-height="11in"
                                       margin="0.25in">
                    <fo:region-body margin-left="1.5in"
                                    margin-right="1.5in"
                                    margin-top="0.5in"
                                    margin-bottom="0.5in" />
                </fo:simple-page-master>
                <fo:simple-page-master master-name="SpecialEM"
                                       page-width="8.5in"
                                       page-height="11in"
                                       margin="0.25in">
                    <fo:region-body margin-left="1.5in"
                                    margin-right="1.5in"
                                    margin-top="0.5in"
                                    margin-bottom="0.5in" />
                </fo:simple-page-master>
            <!-- Safeguards -->
                <fo:simple-page-master master-name="Safeguards"
                                       xsl:use-attribute-sets="page-defaults"
                                       margin-top="0.25in">
                    <fo:region-body margin-left="0.5in"
                                    margin-right="0.5in"
                                    margin-top="0.5in"
                                    margin-bottom="1.2in"
                                    column-count="2"
                                    column-gap="2em" />
                    <fo:region-start region-name="xsl-region-start"
                                     extent="0.5in - 2em"
                                     precedence="true" />
                    <fo:region-end region-name="xsl-region-end"
                                     extent="0.5in - 2em"
                                     precedence="true" />
                    <fo:region-before region-name="xsl-region-before"
                                      extent="0.5in"
                                      display-align="before" />
                    <fo:region-after region-name="xsl-region-after"
                                     extent="0.5in"
                                     display-align="after" />
                </fo:simple-page-master>
            <!-- Copyright &c -->
                <fo:simple-page-master master-name="Copyright"
                                       xsl:use-attribute-sets="page-defaults"
                                       margin-top="0.25in">
                    <fo:region-body margin-left="1.5in"
                                    margin-right="1.5in"
                                    margin-top="0.5in"
                                    margin-bottom="0.5in" />
                    <fo:region-start region-name="xsl-region-start"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-end region-name="xsl-region-end"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-before region-name="xsl-region-before"
                                      extent="0.5in"
                                      display-align="before" />
                    <fo:region-after region-name="xsl-region-after"
                                     extent="0.5in"
                                     display-align="after" />
                </fo:simple-page-master>
            <!-- Table of Contents -->
                <fo:simple-page-master master-name="ToC"
                                       xsl:use-attribute-sets="page-defaults"
                                       margin-top="0.25in">
                    <fo:region-body margin-left="1.5in"
                                    margin-right="1.5in"
                                    margin-top="0.5in"
                                    margin-bottom="0.5in" />
                    <fo:region-start region-name="xsl-region-start"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-end region-name="xsl-region-end"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-before region-name="xsl-region-before"
                                      extent="0.5in"
                                      display-align="before" />
                    <fo:region-after region-name="xsl-region-after"
                                     extent="0.5in"
                                     display-align="after" />
                </fo:simple-page-master>
            <!-- Front Matter -->
                <fo:simple-page-master master-name="Frontmatter Title Page"
                                       xsl:use-attribute-sets="page-defaults">
                    <fo:region-body margin-left="1.5in"
                                    margin-right="1.5in"
                                    margin-top="0.25in"
                                    margin-bottom="1in" />
                    <fo:region-start region-name="xsl-region-start-first"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-end region-name="xsl-region-end-first"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-before region-name="xsl-region-before-first"
                                      extent="0.5in"
                                      display-align="before" />
                    <fo:region-after region-name="xsl-region-after-first"
                                     extent="0.5in"
                                     display-align="after" />
                </fo:simple-page-master>

                <fo:simple-page-master master-name="Frontmatter Body"
                                       xsl:use-attribute-sets="page-defaults">
                    <fo:region-body margin-left="1.5in"
                                    margin-right="1.5in"
                                    margin-top="0.25in"
                                    margin-bottom="1in" />
                    <fo:region-start region-name="xsl-region-start"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-end region-name="xsl-region-end"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-before region-name="xsl-region-before"
                                      extent="0.5in"
                                      display-align="before" />
                    <fo:region-after region-name="xsl-region-after"
                                     extent="0.5in"
                                     display-align="after" />
                </fo:simple-page-master>
            <!-- Chapter -->
                <fo:simple-page-master master-name="Chapter Title Page"
                                       xsl:use-attribute-sets="page-defaults">
                    <fo:region-body margin-left="1.5in"
                                    margin-right="1.5in"
                                    margin-top="0.25in"
                                    margin-bottom="1in" />
                    <fo:region-start region-name="xsl-region-start-first"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-end region-name="xsl-region-end-first"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-before region-name="xsl-region-before-first"
                                      extent="0.5in"
                                      display-align="before" />
                    <fo:region-after region-name="xsl-region-after-first"
                                     extent="0.5in"
                                     display-align="after" />
                </fo:simple-page-master>

                <fo:simple-page-master master-name="Chapter Body"
                                       xsl:use-attribute-sets="page-defaults">
                    <fo:region-body margin-left="1.5in"
                                    margin-right="1.5in"
                                    margin-top="0.25in"
                                    margin-bottom="1in" />
                    <fo:region-start region-name="xsl-region-start"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-end region-name="xsl-region-end"
                                     extent="1.5in - 2em"
                                     precedence="true" />
                    <fo:region-before region-name="xsl-region-before"
                                      extent="0.5in"
                                      display-align="before" />
                    <fo:region-after region-name="xsl-region-after"
                                     extent="0.5in"
                                     display-align="after" />
                </fo:simple-page-master>

                <fo:page-sequence-master master-name="Frontmatter">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference="Frontmatter Title Page" page-position="first" />
                        <fo:conditional-page-master-reference master-reference="Frontmatter Body" page-position="rest" />
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master>

                <fo:page-sequence-master master-name="Chapter">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference="Chapter Title Page" page-position="first" />
                        <fo:conditional-page-master-reference master-reference="Chapter Body" page-position="rest" />
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master>
            <!-- Glossary -->
            <!-- Index -->
            <!-- Blank -->
        </fo:layout-master-set>

    <!-- CONTENT FLOW -->
        <!-- TITLE PAGE -->
        <fo:page-sequence master-reference="Title Page" force-page-count="no-force">
            <fo:flow flow-name="xsl-region-body">
                <xsl:apply-templates select="//document/*" mode="titlepage" />
            </fo:flow >
        </fo:page-sequence >

        <!-- SPECIAL FRONTMATTER -->
        <xsl:for-each select="//document/section[@class = 'specialfrontmatter']">
            <xsl:choose>
                <!-- IMPORTANT SAFEGUARDS -->
                <xsl:when test="@id='important-safeguards'">
                    <fo:page-sequence master-reference="Safeguards"
                                      format="i"
                                      initial-page-number="1"
                                      force-page-count="no-force">
                        <fo:static-content flow-name="xsl-region-after">
                    <fo:block xsl:use-attribute-sets="footer_text-defaults">
                        <fo:retrieve-marker retrieve-class-name="chapter" />
                        <fo:leader leader-pattern="space" />
                        <fo:page-number />
                    </fo:block>
                        </fo:static-content>
                        <fo:flow flow-name="xsl-region-body">
                            <xsl:apply-templates mode="safeguards" />
                        </fo:flow>
                    </fo:page-sequence>
                </xsl:when>
                <!-- COPYRIGHT -->
                <xsl:when test="@id='copyright-and-trademark-information'">
                    <fo:page-sequence master-reference="Copyright"
                                      format="i"
                                      force-page-count="no-force">
                        <fo:static-content flow-name="xsl-region-after">
                            <fo:block xsl:use-attribute-sets="footer_text-defaults">
                                <xsl:text>Copyright</xsl:text>
                                <fo:leader leader-pattern="space" />
                                <fo:page-number />
                            </fo:block>
                        </fo:static-content>
                        <fo:flow flow-name="xsl-region-body">
                            <xsl:apply-templates mode="copyright" />
                        </fo:flow>
                    </fo:page-sequence>
                </xsl:when>
                <!-- UNKNOWN SPECIAL FRONTMATTER -->
                <xsl:otherwise>
                    <fo:page-sequence master-reference="SpecialFM"
                                      force-page-count="no-force">
                        <fo:flow flow-name="xsl-region-body">
                            <fo:block xsl:use-attribute-sets="unknown_tag-defaults">
                                <xsl:text> Special Frontmatter: Section </xsl:text>
                                <xsl:value-of select="@id" />
                                <xsl:text> (</xsl:text><xsl:value-of select="@name" /><xsl:text>) not implemented; see PDF.XSL page sequencing templates.</xsl:text>
                            </fo:block>
                        </fo:flow>
                    </fo:page-sequence>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>  <!-- special front matter -->

        <!-- TABLE OF CONTENTS -->
        <fo:page-sequence master-reference="ToC"
                          format="i"
                          force-page-count="no-force">
            <fo:static-content flow-name="xsl-region-after">
                    <fo:block xsl:use-attribute-sets="footer_text-defaults">
                        <xsl:text>Contents</xsl:text>
                        <fo:leader leader-pattern="space" />
                        <fo:page-number />
                    </fo:block>
            </fo:static-content>
            <fo:flow flow-name="xsl-region-body">
                <xsl:call-template name="frontmatter-toc" />
            </fo:flow >
        </fo:page-sequence>

        <!-- FRONT MATTER -->
        <xsl:for-each select="//document/section[@class = 'frontmatter']">
            <fo:page-sequence master-reference="Frontmatter"
                              format="i"
                              force-page-count="no-force">
                <!-- first page: include revision number -->
                <fo:static-content flow-name="xsl-region-after-first">
                    <fo:block xsl:use-attribute-sets="footer_text-defaults">
                        <!-- dammit, it doesn't work
                        <xsl:text>Revision </xsl:text>
                        <xsl:call-template name="u:docinfo-revision">
                            <xsl:with-param name="whichNodes">all</xsl:with-param>
                        </xsl:call-template>
                        -->
                        <fo:leader leader-pattern="space" />
                        <fo:page-number />
                    </fo:block>
                </fo:static-content>
                <!-- remaining pages: include front-matter chapter name -->
                <fo:static-content flow-name="xsl-region-before">
                    <fo:block xsl:use-attribute-sets="header_text-defaults" />
                </fo:static-content>
                <fo:static-content flow-name="xsl-region-after">
                    <fo:block xsl:use-attribute-sets="footer_text-defaults">
                        <fo:retrieve-marker retrieve-class-name="chapter" />
                        <fo:leader leader-pattern="space" />
                        <fo:page-number />
                    </fo:block>
                </fo:static-content>
                <fo:flow flow-name="xsl-region-body">
                    <fo:block>
                        <xsl:apply-templates select="." mode="frontmatter" />
                    </fo:block>
                </fo:flow>
            </fo:page-sequence>
        </xsl:for-each>  <!-- front matter -->

        <!-- CHAPTERS -->
        <xsl:for-each select="//document/section[not(@class = 'frontmatter' or @class = 'specialfrontmatter' or @class = 'endmatter')]">
            <xsl:choose>
                    <xsl:when test="position() != 1">
                    <!-- not the first chapter, page numbering follows previous chapter -->
                    <fo:page-sequence master-reference="Chapter"
                                      format="1"
                                      force-page-count="no-force">
                        <!-- first page: include revision number -->
                        <fo:static-content flow-name="xsl-region-after-first">
                            <fo:block xsl:use-attribute-sets="footer_text-defaults">
                                <!-- dammit, it doesn't work
                                <xsl:text>Revision </xsl:text>
                                <xsl:call-template name="u:docinfo-revision">
                                    <xsl:with-param name="whichNodes">all</xsl:with-param>
                                </xsl:call-template>
                                -->
                                <fo:leader leader-pattern="space" />
                                <fo:page-number />
                            </fo:block>
                        </fo:static-content>
                        <!-- remaining pages: include chapter name -->
                        <fo:static-content flow-name="xsl-region-before">
                            <fo:block xsl:use-attribute-sets="header_text-defaults" />
                        </fo:static-content>
                        <fo:static-content flow-name="xsl-region-after">
                            <fo:block xsl:use-attribute-sets="footer_text-defaults">
                                <fo:retrieve-marker retrieve-class-name="chapter" />
                                <fo:leader leader-pattern="space" />
                                <fo:page-number />
                            </fo:block>
                        </fo:static-content>
                        <fo:flow flow-name="xsl-region-body">
                            <fo:block>
                                <xsl:apply-templates select="." />
                            </fo:block>
                        </fo:flow>
                    </fo:page-sequence>
                </xsl:when>
                <xsl:otherwise>
                    <!-- first chapter, start page numbering at "1" -->
                    <fo:page-sequence master-reference="Chapter"
                                      format="1"
                                      initial-page-number="1"
                                      force-page-count="no-force">
                        <!-- first page: include revision number -->
                        <fo:static-content flow-name="xsl-region-after-first">
                            <fo:block xsl:use-attribute-sets="footer_text-defaults">
                                <!-- dammit, it doesn't work
                                <xsl:text>Revision </xsl:text>
                                <xsl:call-template name="u:docinfo-revision">
                                    <xsl:with-param name="whichNodes">all</xsl:with-param>
                                </xsl:call-template>
                                -->
                                <fo:leader leader-pattern="space" />
                                <fo:page-number />
                            </fo:block>
                        </fo:static-content>
                        <!-- remaining pages: include chapter name -->
                        <fo:static-content flow-name="xsl-region-before">
                            <fo:block xsl:use-attribute-sets="header_text-defaults" />
                        </fo:static-content>
                        <fo:static-content flow-name="xsl-region-after">
                            <fo:block xsl:use-attribute-sets="footer_text-defaults">
                                <fo:retrieve-marker retrieve-class-name="chapter" />
                                <fo:leader leader-pattern="space" />
                                <fo:page-number />
                            </fo:block>
                        </fo:static-content>
                        <fo:flow flow-name="xsl-region-body">
                            <fo:block>
                                <xsl:apply-templates select="." />
                            </fo:block>
                        </fo:flow>
                    </fo:page-sequence>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>  <!-- chapters -->

        <!-- Document Versioning -->
        <fo:page-sequence master-reference="SpecialEM"
                          format="1"
                          force-page-count="no-force">
            <fo:static-content flow-name="xsl-region-before">
                <fo:block xsl:use-attribute-sets="header_text-defaults" />
            </fo:static-content>
            <fo:static-content flow-name="xsl-region-after">
                <fo:block xsl:use-attribute-sets="footer_text-defaults">
                    <fo:leader leader-pattern="space" />
                    <fo:page-number />
                </fo:block>
            </fo:static-content>
            <fo:flow flow-name="xsl-region-body">
                <fo:block>
                    <xsl:call-template name="endmatter-versions" />
                </fo:block>
            </fo:flow>
        </fo:page-sequence>

    </fo:root>
</xsl:template>
</xsl:stylesheet>
