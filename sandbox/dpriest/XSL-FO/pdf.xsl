<?xml version="1.0" encoding="utf-8"?>
<!--
    Author: David Priest
    Date: 2003-Jun-13
    Purpose: DocUtils XML -> DocUtils FO

    pdf.xsl is the root transformation for doc-utils
    transformed documentation.
 -->

<xsl:stylesheet version="1.1"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:fox="http://xml.apache.org/fop/extensions"
    xmlns:saxon="http://icl.com/saxon"
    xmlns:u="U"> <!-- for local utility functions -->

<xsl:include href="./pdf_defaults.xsl" />
<xsl:include href="./pdf_pagemasters.xsl" />
<xsl:include href="./pdf_titlepage.xsl" />
<xsl:include href="./pdf_special_frontmatter.xsl" />
<xsl:include href="./pdf_frontmatter.xsl" />
<xsl:include href="./pdf_chapters.xsl" />
<xsl:include href="./pdf_special_endmatter.xsl" />

<!-- WARNING: NO USER-SERVICABLE PARTS INSIDE! -->
<!-- DOCUMENT (ROOT) -->
    <xsl:template match="document">
        <xsl:apply-templates />
    </xsl:template>

    <!-- DOCUMENT: decorations -->
        <xsl:template match="/document/decoration/header" />
        <xsl:template match="/document/decoration/footer" />

<!-- STRUCTURAL ELEMENTS -->
    <xsl:template match="topic" >
        <fo:block xsl:use-attribute-sets="topic_block-defaults">
            <fo:block>
                <xsl:value-of select="./title" />
            </fo:block>
            <xsl:apply-templates select="child::*" />
        </fo:block>
    </xsl:template>


    <xsl:template match="sidebar" />
    <xsl:template match="transition" />

    <xsl:template match="section">
        <xsl:choose>
            <xsl:when test="@class = 'breakbefore'">
                <fo:block break-before="page-even">
                    <xsl:apply-templates />
                </fo:block>
            </xsl:when>
            <xsl:otherwise>
                <fo:block>
                    <xsl:apply-templates />
                </fo:block>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- SECTION Sub-Elements -->
        <!-- Title page title/subtitle handled in pdf_titlepage.xsl
             Special Frontmatter (safeguards, warnings, etc) handled in pdf_specialfrontmatter.xsl
             Book's Table of Contents chapter/section listings handled in pdf_frontmatter.xsl
             Chapter's Table of Contents chapter/section listings handled in pdf_chapters.xsl
             All other titles handled below; judicious use of modes -->

        <!-- when titling a Frontmatter Section -->
        <xsl:template match="title" mode="fm_ttl_pg">
            <!-- start of a chapter. fiddlefart around to get nice numbering -->
            <fo:block id="{../@id}" xsl:use-attribute-sets="title-chapter">
                <fo:marker marker-class-name="chapter"><fo:inline xsl:use-attribute-sets="marker_text-defaults"><xsl:value-of select="./text()" /></fo:inline></fo:marker>
                <fo:list-block>
                    <fo:list-item>
                        <fo:list-item-label>
                            <fo:block xsl:use-attribute-sets="chapter-number">
                            </fo:block>
                        </fo:list-item-label>
                        <fo:list-item-body>
                            <fo:block xsl:use-attribute-sets="chapter-title">
                                <xsl:apply-templates />
                            </fo:block>
                        </fo:list-item-body>
                    </fo:list-item>
                </fo:list-block>
            </fo:block>
        </xsl:template>

        <!-- when titling a Chapter -->
        <xsl:template match="title" mode="ch_ttl_pg">
            <!-- start of a chapter. fiddlefart around to get nice numbering -->
            <fo:block id="{../@id}" xsl:use-attribute-sets="title-chapter">
                <fo:marker marker-class-name="chapter"><fo:inline xsl:use-attribute-sets="marker_text-defaults"><xsl:value-of select="./text()" /></fo:inline></fo:marker>
                <fo:list-block>
                    <fo:list-item>
                        <fo:list-item-label>
                            <fo:block xsl:use-attribute-sets="chapter-number">
                                <xsl:value-of select="count(../preceding-sibling::*[name()='section' and not(@class = 'frontmatter' or @class = 'specialfrontmatter')]) + 1" />
                            </fo:block>
                        </fo:list-item-label>
                        <fo:list-item-body>
                            <fo:block xsl:use-attribute-sets="chapter-title">
                                <xsl:apply-templates />
                            </fo:block>
                        </fo:list-item-body>
                    </fo:list-item>
                </fo:list-block>
            </fo:block>
        </xsl:template>

        <!-- when referencing a title for cross-referencing -->
        <xsl:template match="title" mode="xref">
            <xsl:apply-templates />
        </xsl:template>

        <!-- SECTION TITLE in all other cases -->
        <xsl:template match="section/title">
            <xsl:variable name="level" select="count(ancestor::section)" />
            <xsl:choose>
                <xsl:when test="$level = 1">
                    <!-- special case, handled above as chapter title page -->
                </xsl:when>
                <xsl:when test="$level = 2">
                    <fo:block id="{../@id}" xsl:use-attribute-sets="title-level1">
                        <xsl:apply-templates />
                    </fo:block>
                </xsl:when>
                <xsl:when test="$level = 3">
                    <fo:block id="{../@id}" xsl:use-attribute-sets="title-level2">
                        <xsl:apply-templates />
                    </fo:block>
                </xsl:when>
                <xsl:when test="$level = 4">
                    <fo:block id="{../@id}" xsl:use-attribute-sets="title-level3">
                        <xsl:apply-templates />
                    </fo:block>
                </xsl:when>
                <xsl:otherwise>
                    <fo:block id="{../@id}" xsl:use-attribute-sets="title-level4up">
                        <xsl:apply-templates />
                    </fo:block>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:template>

    <!-- TOPIC Sub-Elements -->
        <!-- Topic title -->
        <xsl:template match="topic/title" />

    <!-- SIDEBAR Sub-Elements -->
        <!-- Sidebar title -->
            <xsl:template match="sidebar/title" />
        <!-- Sidebar subtitle -->
            <xsl:template match="sidebar/subtitle" />

<!-- SIMPLE BODY ELEMENTS -->
    <!-- BODY: comment -->
        <xsl:template match="comment">
            <xsl:variable name="is_filenotes">
                <xsl:value-of select="contains(text(),'file notes')" />
            </xsl:variable>
            <xsl:choose>
                <xsl:when test="($publish_filenotes = 'enabled') and ($is_filenotes = 'true')">
                    <fo:block xsl:use-attribute-sets="comment-defaults">
                        <xsl:text>Comment: </xsl:text>
                        <xsl:apply-templates />
                    </fo:block>
                </xsl:when>
                <xsl:when test="($publish_comments = 'enabled') and ($is_filenotes = 'false')">
                    <fo:block xsl:use-attribute-sets="comment-defaults">
                        <xsl:text>Comment: </xsl:text>
                        <xsl:apply-templates />
                    </fo:block>
                </xsl:when>
            </xsl:choose>
        </xsl:template>

    <!-- BODY: doctest_block -->
    <!-- BODY: line_block -->
        <xsl:template match="line_block">
            <fo:block xsl:use-attribute-sets="line_block-defaults">
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: literal_block -->
        <xsl:template match="literal_block">
            <fo:block xsl:use-attribute-sets="literal_block-defaults">
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: paragraph -->
        <!-- document-level paragraphs handled in pdf_titlepage.xsl -->
        <!-- special frontmatter (warnings, copyright, etc) handled in pdf_specialfrontmatter.xsl -->
        <!-- frontmatter section/chapter introductory paragraphs handled in pdf_chapters.xsl -->
        <!-- other paragraphs handled here -->
        <xsl:template match="paragraph">
            <fo:block xsl:use-attribute-sets="paragraph-defaults">
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>
        <xsl:template match="list_item/paragraph[1]">
            <!-- if we apply paragraph-defaults to the first paragraph
                 of bullet content, we get a nasty case of the label not
                 aligning with the text -->
            <fo:block xsl:use-attribute-sets="list_item_1stparagraph-defaults">
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>
        <xsl:template match="thead/row/entry/paragraph">
            <!-- table headers are special -->
            <fo:block xsl:use-attribute-sets="table_header_paragraph-defaults">
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>
        <xsl:template match="tbody/row/entry/paragraph[1]">
            <!-- table cells don't need the extra space above -->
            <fo:block xsl:use-attribute-sets="table_entry_1stparagraph-defaults">
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>
        <xsl:template match="definition/paragraph[1]">
            <!-- definitions don't need the extra space above -->
            <fo:block xsl:use-attribute-sets="definition_defn_1stparagraph-defaults">
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: pending -->
    <!-- BODY: raw -->
    <!-- BODY: rubric -->
    <!-- BODY: substitution_definition -->
        <!-- these just disappear; they serve no purpose -->
        <xsl:template match="substitution_definition" />

    <!-- BODY: target -->
        <!-- TO DO: targets always target the next element; copy the target id to the target element
                    (change the references to use "contains" when this is done)
                    //target/following-sibling::*[1] -->
        <xsl:template match="target">
            <fo:block id="{@id}">
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

<!-- COMPLEX BODY ELEMENTS -->
    <!-- BODY: admonition -->
        <xsl:template match="admonition">
            <fo:block xsl:use-attribute-sets="admonition_block-defaults">
                <fo:block>
                    <xsl:value-of select="substring-after(./@class, ' ')" />:
                    <xsl:value-of select="./title" />
                </fo:block>
                <xsl:apply-templates select="child::*[name()='paragraph']" />
            </fo:block>
        </xsl:template>

    <!-- BODY: attention -->
        <xsl:template match="attention">
            <fo:block xsl:use-attribute-sets="attention_block-defaults">
                <xsl:text>Attention: </xsl:text>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: block_quote -->
        <xsl:template match="block_quote">
            <fo:block xsl:use-attribute-sets="list_block-defaults">
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>
    <!-- BODY: bullet_list -->
        <xsl:template match="bullet_list[not(contains(name(..),'list'))]">
            <!-- is not a list within a list -->
            <fo:list-block xsl:use-attribute-sets="list_block-defaults">
                <xsl:apply-templates />
            </fo:list-block>
        </xsl:template>
        <xsl:template match="bullet_list[contains(name(..),'list')]">
            <!-- is a list within a list -->
            <fo:list-block>
                <xsl:apply-templates />
            </fo:list-block>
        </xsl:template>
        <!-- bullet_list subelement -->
            <xsl:template match="bullet_list/list_item">
                <xsl:variable name="bullet" select="../@bullet" />
                <fo:list-item xsl:use-attribute-sets="list_item-defaults">
                    <fo:list-item-label end-indent="label-end()">
                        <fo:block>
                            <xsl:choose>
                                <xsl:when test="$bullet='-'">
                                    <fo:inline font-family="Symbol">&#x2022;</fo:inline>
                                </xsl:when>
                                <xsl:when test="$bullet='+'">
                                    <fo:inline font-family="Symbol">&#x2022;</fo:inline>
                                </xsl:when>
                                <xsl:when test="$bullet='*'">
                                    <fo:inline font-family="Symbol">&#x2022;</fo:inline>
                                </xsl:when>
                            </xsl:choose>
                        </fo:block>
                    </fo:list-item-label>
                    <fo:list-item-body start-indent="body-start()">
                        <fo:block>
                            <xsl:apply-templates />
                        </fo:block>
                    </fo:list-item-body>
                </fo:list-item>
            </xsl:template>

    <!-- BODY: caution -->
        <xsl:template match="caution">
            <fo:block xsl:use-attribute-sets="caution_block-defaults">
                <!-- hack: would rather select title, but none is available
                     ergo, 1st para = title; 2nd para = classification; remaining = body text -->
                <xsl:text>Caution:</xsl:text>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: citation -->
    <!-- BODY: caption -->
        <xsl:template match="caption" mode="figure-pdf">
            <fo:block xsl:use-attribute-sets="paragraph-defaults">
                <xsl:text>Figure </xsl:text>
                <xsl:choose>
                    <xsl:when test="ancestor::*[name()='section' and not(@class = 'frontmatter' or @class = 'specialfrontmatter')]">
                        <!-- get the chapter number.  Ugh! -->
                        <xsl:value-of select="count((ancestor::*[last()-1])/preceding-sibling::*[name()='section' and not(@class = 'frontmatter' or @class = 'specialfrontmatter')]) + 1" />
                        <xsl:text>.</xsl:text>
                        <xsl:number level="any" count="figure[@class='pdf']" from="/document/section" />
                    </xsl:when>
                    <xsl:otherwise>
                        <!-- the figure is a special frontmatterish chapter, no chapter number -->
                        <xsl:text>.</xsl:text>
                        <xsl:number level="any" count="figure[@class='pdf']" from="/document/section" />
                    </xsl:otherwise>
                </xsl:choose>
                <xsl:text>: </xsl:text>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: danger -->
        <xsl:template match="danger">
            <fo:block xsl:use-attribute-sets="danger_block-defaults">
                <xsl:text>Danger: </xsl:text>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: definition_list -->
        <xsl:template match="definition_list">
            <fo:block>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>
        <!-- definition_list_item subelement -->
            <xsl:template match="definition_list_item">
                <fo:block xsl:use-attribute-sets="definition_item_block-defaults">
                    <xsl:apply-templates />
                </fo:block>
            </xsl:template>
            <!-- term subelement -->
                <xsl:template match="term">
                    <fo:block xsl:use-attribute-sets="definition_term-defaults">
                        <xsl:apply-templates />
                    </fo:block>
                </xsl:template>
            <!-- classifier subelement -->
                <xsl:template match="classifier">
                    <fo:block xsl:use-attribute-sets="definition_classifier-defaults">
                        <xsl:apply-templates />
                    </fo:block>
                </xsl:template>
            <!-- definition subelement -->
                <xsl:template match="definition">
                    <fo:block xsl:use-attribute-sets="definition_defn-defaults">
                        <xsl:apply-templates />
                    </fo:block>
                </xsl:template>

    <!-- BODY: enumerated_list -->
        <xsl:template match="enumerated_list[not(contains(name(..),'list'))]">
            <!-- is not a list within a list -->
            <fo:list-block xsl:use-attribute-sets="list_block-defaults">
                <xsl:apply-templates />
            </fo:list-block>
        </xsl:template>
        <xsl:template match="enumerated_list[(contains(name(..),'list'))]">
            <!-- is a list within a list -->
            <fo:list-block xsl:use-attribute-sets="list_item-defaults">
                <xsl:apply-templates />
            </fo:list-block>
        </xsl:template>
        <!-- enumerated_list subelement -->
            <xsl:template match="enumerated_list/list_item">
                <fo:list-item xsl:use-attribute-sets="list_item-defaults">
                    <fo:list-item-label end-indent="label-end()">
                        <fo:block>
                            <xsl:choose>
                                <xsl:when test="../@enumtype = 'arabic'">
                                    <xsl:number format="1" /><xsl:text>.</xsl:text>
                                </xsl:when>
                                <xsl:when test="../@enumtype = 'loweralpha'">
                                    <xsl:number format="a" /><xsl:text>.</xsl:text>
                                </xsl:when>
                                <xsl:when test="../@enumtype = 'upperalpha'">
                                    <xsl:number format="A" /><xsl:text>.</xsl:text>
                                </xsl:when>
                                <xsl:when test="../@enumtype = 'lowerroman'">
                                    <xsl:number format="i" /><xsl:text>.</xsl:text>
                                </xsl:when>
                                <xsl:when test="../@enumtype = 'upperroman'">
                                    <xsl:number format="I" /><xsl:text>.</xsl:text>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:text>unrecognized enumtype</xsl:text>
                                    <xsl:number /><xsl:text>.</xsl:text>
                                </xsl:otherwise>
                            </xsl:choose>
                        </fo:block>
                    </fo:list-item-label>
                    <fo:list-item-body start-indent="body-start()">
                        <fo:block>
                            <xsl:apply-templates />
                        </fo:block>
                    </fo:list-item-body>
                </fo:list-item>
            </xsl:template>

    <!-- BODY: error -->
        <xsl:template match="error">
            <fo:block xsl:use-attribute-sets="error_block-defaults">
                <xsl:text>Error: </xsl:text>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: field_list -->
        <xsl:template match="field_list" />
        <!-- field -->
            <xsl:template match="field" />
            <!-- field name -->
            <!-- field body -->

    <!-- BODY: figure -->
        <xsl:template match="figure">
            <xsl:if test="(@class='pdf')">
                <fo:block>
                    <xsl:apply-templates mode="figure-pdf" />
                </fo:block>
            </xsl:if>
        </xsl:template>

    <!-- BODY: footnote -->
        <xsl:template match="footnote">
            <fo:block id="{@id}" xsl:use-attribute-sets="list_block-defaults">
                <fo:inline xsl:use-attribute-sets="footnote_ref-defaults">
                    <xsl:value-of select="@name" />
                </fo:inline>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>
        <!-- footnote label -->
            <xsl:template match="footnote/label" />
        <!-- footnote para -->
            <xsl:template match="footnote/paragraph">
                <xsl:apply-templates />
            </xsl:template>

    <!-- BODY: hint -->
        <xsl:template match="hint">
            <fo:block xsl:use-attribute-sets="hint_block-defaults">
                <xsl:text>Hint: </xsl:text>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: important -->
        <xsl:template match="important">
            <fo:block xsl:use-attribute-sets="important_block-defaults">
                <xsl:text>Important! </xsl:text>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: note -->
        <xsl:template match="note">
            <fo:block xsl:use-attribute-sets="note_block-defaults">
                <xsl:text>Note: </xsl:text>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: option_list -->
    <!-- BODY: system_message -->
        <xsl:template match="system_message">
            <xsl:if test="$system_messages = 'enabled'">
                <fo:block xsl:use-attribute-sets="system_message-defaults">
                    <xsl:text>DocUtils Warning:</xsl:text>
                    <xsl:apply-templates />
                </fo:block>
            </xsl:if>
        </xsl:template>

    <!-- BODY: table -->
        <!-- to be tweaked: base table width on sum of column widths
        <xsl:variable name="twidth">
            <xsl:value-of select="sum(.    group/colspec/@colwidth)*.66" />
        </xsl:variable>
        <fo:table xsl:use-attribute-sets="table_block-defaults" width="{$twidth}em"> -->
        <xsl:template match="table">
            <fo:table xsl:use-attribute-sets="table_block-defaults">
                <xsl:apply-templates />
            </fo:table>
        </xsl:template>
        <!-- table tgroup -->
            <xsl:template match="tgroup">
                <xsl:apply-templates />
            </xsl:template>
        <!-- table column specs -->
            <xsl:template match="colspec">
                <fo:table-column column-width="proportional-column-width({./@colwidth})">
                    <xsl:apply-templates />
                </fo:table-column>
            </xsl:template>
            <!-- table head -->
                <xsl:template match="thead">
                    <fo:table-header xsl:use-attribute-sets="table_head-defaults">
                        <xsl:apply-templates />
                    </fo:table-header>
                </xsl:template>
                    <xsl:template match="thead/row">
                        <fo:table-row xsl:use-attribute-sets="table_row-defaults">
                            <xsl:apply-templates />
                        </fo:table-row>
                    </xsl:template>
                        <xsl:template match="thead/row/entry">
                            <fo:table-cell xsl:use-attribute-sets="table_entry-defaults">
                                <xsl:apply-templates />
                            </fo:table-cell>
                        </xsl:template>
            <!-- table body -->
                <xsl:template match="tbody">
                    <fo:table-body xsl:use-attribute-sets="table_body-defaults">
                        <xsl:apply-templates />
                    </fo:table-body>
                </xsl:template>
                <!-- table row -->
                    <xsl:template match="tbody/row[position() mod 2 = 0]">
                        <fo:table-row background-color="{$even_row-color}" xsl:use-attribute-sets="table_row-defaults">
                            <xsl:apply-templates />
                        </fo:table-row>
                    </xsl:template>
                    <xsl:template match="tbody/row[position() mod 2 != 0]">
                        <fo:table-row background-color="{$odd_row-color}" xsl:use-attribute-sets="table_row-defaults">
                            <xsl:apply-templates />
                        </fo:table-row>
                    </xsl:template>
                    <!-- table entry -->
                        <xsl:template match="entry">
                            <fo:table-cell xsl:use-attribute-sets="table_entry-defaults">
                                <xsl:apply-templates />
                            </fo:table-cell>
                        </xsl:template>

    <!-- BODY: tip -->
        <xsl:template match="tip">
            <fo:block xsl:use-attribute-sets="tip_block-defaults">
                <xsl:text>Tip: </xsl:text>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

    <!-- BODY: warning -->
        <xsl:template match="warning">
            <fo:block xsl:use-attribute-sets="warning_block-defaults">
                <xsl:text>Warning: </xsl:text>
                <xsl:apply-templates />
            </fo:block>
        </xsl:template>

<!-- INLINE ELEMENTS -->
    <!-- INLINE: abbreviation -->
    <!-- INLINE: acronym -->
    <!-- INLINE: citation_reference -->
    <!-- INLINE: emphasis -->
        <xsl:template match="emphasis">
            <fo:inline xsl:use-attribute-sets="emphasis-defaults">
                <xsl:apply-templates />
            </fo:inline>
        </xsl:template>

    <!-- INLINE: footnote_reference -->
        <xsl:template match="footnote_reference">
            <fo:basic-link internal-destination="{@refid}" xsl:use-attribute-sets="internal_link-defaults">
                <fo:inline xsl:use-attribute-sets="footnote_ref-defaults">
                    <xsl:value-of select="@refid" />
                </fo:inline>
            </fo:basic-link>
        </xsl:template>

    <!-- INLINE: generated -->
        <!-- throw them away -->
        <xsl:template match="title/generated" />

    <!-- INLINE: image -->
    <!-- BODY: image -->
        <xsl:template match="image" mode="figure-pdf">
            <fo:external-graphic src="{@uri}">
                <xsl:attribute name="width">
                    <xsl:value-of select="@width div @scale" />
                    <xsl:text>in</xsl:text>
                </xsl:attribute>
                <xsl:attribute name="height">
                    <xsl:value-of select="@height div @scale" />
                    <xsl:text>in</xsl:text>
                </xsl:attribute>
            </fo:external-graphic>
        </xsl:template>

        <xsl:template match="image">
            <fo:block>
                <fo:external-graphic src="{@uri}" />
            </fo:block>
        </xsl:template>

    <!-- INLINE: inline -->
    <!-- INLINE: literal -->
        <xsl:template match="literal">
            <fo:inline xsl:use-attribute-sets="literal-defaults">
                <xsl:apply-templates />
            </fo:inline>
        </xsl:template>

    <!-- INLINE: problematic -->
        <xsl:template match="problematic">
            <xsl:if test="$publish_problematics = 'enabled'">
                <fo:block xsl:use-attribute-sets="problem-defaults">
                    <xsl:text>DocUtils Problematic: </xsl:text>
                    <xsl:apply-templates />
                </fo:block>
            </xsl:if>
        </xsl:template>

    <!-- INLINE: reference -->
        <xsl:key name="sectionkey" match="section" use="@id" />
        <xsl:key name="targetkey" match="target" use="@id" />
        <!-- anonymous internal reference -->
        <xsl:template match="reference[@refid and @anonymous]">
            <xsl:choose>
                <!-- references a section, use the section title -->
                <xsl:when test="key('sectionkey', @refid)">
                    <fo:inline xsl:use-attribute-sets="internal_link-defaults">
                        <xsl:apply-templates />
                    </fo:inline>
                </xsl:when>
                <!-- references a target where the first thing following it is a new section, use the section title -->
                <xsl:when test="((key('targetkey', @refid)/following::*)[1])[name()='section']">
                    <fo:basic-link internal-destination="{(key('targetkey', @refid)/following::*[name()='section'])[1]/@id}" xsl:use-attribute-sets="internal_link-defaults">
                        <fo:inline xsl:use-attribute-sets="internal_link-defaults">
                            <xsl:apply-templates />
                        </fo:inline>
                    </fo:basic-link>
                </xsl:when>
                <!-- references a target that follows a title -->
                <xsl:when test="key('targetkey', @refid)/preceding-sibling::*[name()='title']">
                    <fo:basic-link internal-destination="{key('targetkey', @refid)/../@id}" xsl:use-attribute-sets="internal_link-defaults">
                        <fo:inline xsl:use-attribute-sets="internal_link-defaults">
                            <xsl:apply-templates />
                        </fo:inline>
                    </fo:basic-link>
                </xsl:when>
                <!-- use the text provided by the reference; may not actually match the target's text -->
                <xsl:otherwise>
                    <fo:inline xsl:use-attribute-sets="internal_link-defaults">
                        <xsl:apply-templates />
                    </fo:inline>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:template>



        <!-- not-anonymous internal reference -->
        <xsl:template match="reference[@refid and not(@anonymous)]">
            <xsl:choose>
                <!-- references a section, use the section title -->
                <xsl:when test="key('sectionkey', @refid)">
                    <fo:inline xsl:use-attribute-sets="internal_link-defaults">
                        <xsl:apply-templates select="key('sectionkey', @refid)/title[1]" mode="xref" />
                    </fo:inline>
                </xsl:when>
                <!-- references a target where the first thing following it is a new section, use the section title -->
                <xsl:when test="((key('targetkey', @refid)/following::*)[1])[name()='section']">
                    <fo:basic-link internal-destination="{(key('targetkey', @refid)/following::*[name()='section'])[1]/@id}" xsl:use-attribute-sets="internal_link-defaults">
                        <fo:inline xsl:use-attribute-sets="internal_link-defaults">
                            <xsl:apply-templates select="(key('targetkey', @refid)/following::*[name()='section'])[1]/title[1]" mode="xref" />
                        </fo:inline>
                    </fo:basic-link>
                </xsl:when>
                <!-- references a target that follows a title -->
                <xsl:when test="key('targetkey', @refid)/preceding-sibling::*[name()='title']">
                    <fo:basic-link internal-destination="{key('targetkey', @refid)/../@id}" xsl:use-attribute-sets="internal_link-defaults">
                        <fo:inline xsl:use-attribute-sets="internal_link-defaults">
                            <xsl:apply-templates select="key('targetkey', @refid)/preceding-sibling::*[name()='title']" mode="xref" />
                        </fo:inline>
                    </fo:basic-link>
                </xsl:when>
                <!-- use the text provided by the reference; may not actually match the target's text -->
                <xsl:otherwise>
                    <fo:inline xsl:use-attribute-sets="internal_link-defaults">
                        <xsl:apply-templates />
                    </fo:inline>
                </xsl:otherwise>
            </xsl:choose>
            <xsl:text> (p. </xsl:text>
            <xsl:choose>
                <!-- use the target's title text content -->
                <xsl:when test="key('targetkey', @refid)/preceding-sibling::*[name()='title']">
                    <fo:page-number-citation ref-id="{key('targetkey', @refid)/../@id}" />
                </xsl:when>
                <xsl:otherwise>
                    <fo:page-number-citation ref-id="{@refid}" />
                </xsl:otherwise>
            </xsl:choose>
            <xsl:text>)</xsl:text>
        </xsl:template>

        <!-- http &c reference -->
        <xsl:template match="reference[@refuri]">
            <fo:basic-link external-destination="{@refuri}" xsl:use-attribute-sets="external_link-defaults">
                <fo:inline xsl:use-attribute-sets="external_link-defaults">
                    <xsl:call-template name="u:make-breakable">
                        <xsl:with-param name="string" select="text()" />
                    </xsl:call-template>
                </fo:inline>
            </fo:basic-link>
        </xsl:template>

        <!-- dunno what type of reference -->
        <xsl:template match="reference">
            <fo:inline xsl:use-attribute-sets="reference-defaults">
                <xsl:apply-templates />
            </fo:inline>
        </xsl:template>

    <!-- INLINE: strong -->
            <xsl:template match="strong">
                    <fo:inline xsl:use-attribute-sets="strong-defaults">
                            <xsl:apply-templates />
                    </fo:inline>
            </xsl:template>

    <!-- INLINE: subscript -->
    <!-- INLINE: substitution_reference -->
    <!-- INLINE: superscript -->
    <!-- INLINE: target -->
    <!-- INLINE: title_reference -->
        <xsl:template match="title_reference">
            <fo:inline xsl:use-attribute-sets="title_reference-defaults">
                <xsl:apply-templates />
            </fo:inline>
        </xsl:template>

    <!-- INLINE: raw -->

    <!-- INLINE: ui_command ui_shortcut ui_guilabel -->
        <xsl:template match="ui_command">
            <fo:inline xsl:use-attribute-sets="ui_command-defaults">
                <xsl:apply-templates />
            </fo:inline>
        </xsl:template>

        <xsl:template match="ui_shortcut">
            <fo:inline xsl:use-attribute-sets="ui_shortcut-defaults">
                <xsl:apply-templates />
            </fo:inline>
        </xsl:template>

        <xsl:template match="ui_guilabel">
            <fo:inline xsl:use-attribute-sets="ui_guilabel-defaults">
                <xsl:apply-templates />
            </fo:inline>
        </xsl:template>

<!-- UTILITY FUNCTIONS -->

    <!-- turn slashes into line-breakable slashes -->
    <xsl:template name="u:make-breakable">
        <xsl:param name="string" />
        <xsl:variable name="breakables">
            &#x0021;&#x0022;&#x0023;&#x0024;&#x0025;&#x0026;&#x0027;
            &#x0028;&#x0029;&#x002A;&#x002B;&#x002C;&#x002D;&#x002E;
            &#x002F;&#x003A;&#x003B;&#x003C;&#x003D;&#x003E;&#x003F;
            &#x0040;&#x005B;&#x005C;&#x005D;&#x005E;&#x005F;&#x0060;
        </xsl:variable>

        <xsl:if test="string-length($string) &gt; 0">
            <xsl:variable name="c1" select="substring($string, 1, 1)" />
            <xsl:choose>
                <xsl:when test="contains($breakables, $c1)">
                    <fo:character treat-as-word-space="true">
                        <xsl:attribute name="character">
                            <xsl:value-of select="$c1" />
                        </xsl:attribute>
                    </fo:character>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$c1" />
                </xsl:otherwise>
            </xsl:choose>

            <xsl:call-template name="u:make-breakable">
                <xsl:with-param name="string" select="substring($string, 2)"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <!-- return docinfo date -->
    <xsl:template name="u:docinfo-date">
        <xsl:param name="whichNodes">self</xsl:param>
        <xsl:variable name="date">
            <xsl:choose>
                <xsl:when test="$whichNodes=self">
                    <!-- just the current node's docinfo -->
                    <xsl:value-of select="./field_list[@class='docinfo']/field/field_body/paragraph[contains(text(),'$Date:')]/text()" />
                </xsl:when>
                <xsl:otherwise>
                    <!-- find the most-recent from all the children -->
                    <!-- dammit, this DOES NOT WORK -->
                    <xsl:for-each select="descendant-or-self::*/field_list[@class='docinfo']/field/field_body/paragraph[contains(text(),'$Date:')]/text()" >
                        <xsl:sort select="." data-type="number" order="descending" />
                        <xsl:if test="position()=1">
                            <xsl:copy-of select="." />
                        </xsl:if>
                    </xsl:for-each>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="normalize-space(substring-before(substring-after($date,'Date: '), ' $'))" />
    </xsl:template>

    <!-- return docinfo revision -->
    <xsl:template name="u:docinfo-revision">
        <xsl:param name="whichNodes">self</xsl:param>
        <xsl:variable name="rev">
            <xsl:choose>
                <!-- just the current node's docinfo -->
                <xsl:when test="$whichNodes = 'self'">
                    <xsl:value-of select="./field_list[@class='docinfo']/field/field_body/paragraph[contains(text(),'$Revision:')]/text()" />
                </xsl:when>
                <xsl:otherwise>
                    <!-- find the most-recent from all the children -->
                    <!-- dammit, this DOES NOT WORK -->
                    <xsl:for-each select="descendant-or-self::*/field_list[@class='docinfo']/field/field_body/paragraph[contains(text(),'$Revision:')]/text()" >
                        <xsl:sort select="." data-type="number" order="descending" />
                        <xsl:if test="position()=1">
                            <xsl:value-of select="." />
                        </xsl:if>
                    </xsl:for-each>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="normalize-space(substring-before(substring-after($rev,'Revision: '), ' $'))" />
    </xsl:template>

    <!-- return filename for document containing the current node -->
    <xsl:template name="u:docinfo-filename">
        <xsl:param name="thisNode" select="self::*"/>
        <xsl:variable name="fname">
            <xsl:value-of select="($thisNode/ancestor-or-self::*/field_list[@class='docinfo']/field/field_body/paragraph[contains(text(),'$RCSfile:')]/text())[last()]" />
        </xsl:variable>
        <xsl:value-of select="normalize-space(substring-before(substring-after($fname,'RCSfile: '), ',v $'))" />
    </xsl:template>


    <!-- DEBUGGING UTILITIES -->
    <!-- show ancestors of current node -->
    <xsl:template name="u:an">
        <xsl:variable name="full-path">
          <xsl:for-each select="ancestor-or-self::*">
            <xsl:value-of select="concat('/',name())" />
          </xsl:for-each>
        </xsl:variable>
        <xsl:message><xsl:value-of select="$full-path" /></xsl:message>
    </xsl:template>

    <!-- show all children of current node -->
    <xsl:template name="u:ch">
        <xsl:for-each select="descendant::*">
            <xsl:call-template name="u:ch" />
        </xsl:for-each>
        <xsl:if test="count(child::*) = 0">
            <xsl:call-template name="u:an" />
        </xsl:if>
    </xsl:template>

    <!-- show immediate children of current node -->
    <xsl:template name="u:ch1">
        <xsl:for-each select="*">
            <xsl:call-template name="u:an" />
        </xsl:for-each>
    </xsl:template>


<!-- REPORT UNKNOWN TAGS -->
    <xsl:template match="*">
        <xsl:message>
            <xsl:value-of select="name(.)" />
            <xsl:text> encountered</xsl:text>
            <xsl:if test="parent::*">
                <xsl:text> in </xsl:text>
                <xsl:value-of select="name(parent::*)" />
            </xsl:if>
            <xsl:text>, but no template matches.</xsl:text>
        </xsl:message>
        <!-- highlight in red in the output -->
        <fo:block >
            <xsl:text>&lt;</xsl:text>
            <xsl:value-of select="name(.)" />
            <xsl:text>&gt;</xsl:text>
            <xsl:apply-templates />
            <xsl:text>&lt;/</xsl:text>
            <xsl:value-of select="name(.)" />
            <xsl:text>&gt;</xsl:text>
        </fo:block>
    </xsl:template>

</xsl:stylesheet>
