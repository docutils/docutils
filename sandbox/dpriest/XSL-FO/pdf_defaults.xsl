<?xml version="1.0" encoding="utf-8"?>
<!--
    Author: David Priest
    Date: 2003-Jun-13
    Purpose: DocUtils XML -> DocUtils FO

    A collection of attributes that define most
    layout aspects.  Most customization (other
    than page layout and ordering) will be
    performed in this file.

    There are page-specific layout attributes
    set in the pagemasters, special_frontmatter,
    and front_matter files.
 -->

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:fox="http://xml.apache.org/fop/extensions"
    xmlns:saxon="http://icl.com/saxon"
    xmlns:u="U"> <!-- for local utility functions -->

<!-- BODY TEXT DEFAULTS -->
    <xsl:attribute-set name="text-defaults">
        <xsl:attribute name="font-family">OfficinaSansBook,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">normal</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="font-size">10pt</xsl:attribute>
        <xsl:attribute name="line-height">12pt</xsl:attribute>
        <xsl:attribute name="space-before">6pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">0.5</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">0.5</xsl:attribute>
        <xsl:attribute name="text-align">left</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="paragraph-defaults" use-attribute-sets="text-defaults">
        <xsl:attribute name="line-height">12pt</xsl:attribute>
        <xsl:attribute name="space-before">6pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">0.75</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">0.75</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="list_item_1stparagraph-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">1.0</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="definition_defn_1stparagraph-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">1.0</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table_header_paragraph-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="font-family">OfficinaSansBold,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">1.0</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table_entry_1stparagraph-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">1.0</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="list_block-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="margin-left">1em</xsl:attribute>
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">1.0</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">1.0</xsl:attribute>
        <xsl:attribute name="provisional-label-separation">.25em</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">1em</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="list_item-defaults">
        <xsl:attribute name="space-before">6pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">0.25</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">0.25</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="definition_item_block-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">1.0</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">1.0</xsl:attribute>
        <xsl:attribute name="provisional-label-separation">.5em</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">2em</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="definition_term-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="font-family">OfficinaSansBold,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="space-before">6pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">1.0</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">1.0</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="definition_classifier-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">1.0</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">1.0</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="definition_defn-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">1.0</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">1.0</xsl:attribute>
        <xsl:attribute name="start-indent">2em</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="line_block-defaults" use-attribute-sets="list_block-defaults">
        <xsl:attribute name="white-space-collapse">false</xsl:attribute>
        <xsl:attribute name="linefeed-treatment">preserve</xsl:attribute>
        <xsl:attribute name="white-space-treatment">preserve</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="literal_block-defaults" use-attribute-sets="list_block-defaults">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
        <xsl:attribute name="white-space-collapse">false</xsl:attribute>
        <xsl:attribute name="linefeed-treatment">preserve</xsl:attribute>
        <xsl:attribute name="white-space-treatment">preserve</xsl:attribute>
    </xsl:attribute-set>

<!-- INLINES -->
    <xsl:attribute-set name="footnote_ref-defaults">
        <xsl:attribute name="font-family">OfficinaSansBoldItalic,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
        <xsl:attribute name="font-size">xx-small</xsl:attribute>
        <xsl:attribute name="baseline-shift">super</xsl:attribute>
        <xsl:attribute name="vertical-align">super</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="strong-defaults">
        <xsl:attribute name="font-family">OfficinaSansBold,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="reference-defaults">
        <xsl:attribute name="font-family">OfficinaSansBookItalic,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">normal</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="emphasis-defaults">
        <xsl:attribute name="font-family">OfficinaSansBookItalic,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">normal</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="ui_command-defaults">
        <xsl:attribute name="font-family">OfficinaSansBold,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="ui_guilabel-defaults">
        <xsl:attribute name="font-family">OfficinaSansBold,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="ui_shortcut-defaults">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="title_reference-defaults">
        <xsl:attribute name="font-family">OfficinaSansBookItalic,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">normal</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="literal-defaults" use-attribute-sets="literal_block-defaults" />

    <xsl:attribute-set name="internal_link-defaults">
        <xsl:attribute name="color">darkgreen</xsl:attribute>
        <xsl:attribute name="font-family">OfficinaSansBoldItalic,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="external_link-defaults">
        <xsl:attribute name="color">midnightblue</xsl:attribute>
        <xsl:attribute name="font-family">OfficinaSansBoldItalic,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="chapter_toc_entry">
        <xsl:attribute name="text-align">right</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="chapter_toc_pagenum">
        <xsl:attribute name="text-align">right</xsl:attribute>
    </xsl:attribute-set>

<!-- TABLE DEFAULTS -->
    <xsl:attribute-set name="table_block-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="space-after">6pt</xsl:attribute>
        <xsl:attribute name="border">1pt solid grey</xsl:attribute>
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="width">100%</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table_head-defaults">
        <xsl:attribute name="font-family">OfficinaSansBold,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="background-color">Gainsboro</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table_body-defaults">
        <xsl:attribute name="font-family">OfficinaSansBook,sans-serif</xsl:attribute>
        <xsl:attribute name="font-weight">normal</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>

    </xsl:attribute-set>

    <xsl:variable name="even_row-color">honeydew</xsl:variable>
    <xsl:variable name="odd_row-color">white</xsl:variable>

    <xsl:attribute-set name="table_row-defaults" />
        <!-- no FOP support for margins, padding, etc? -->

    <xsl:attribute-set name="table_entry-defaults">
        <xsl:attribute name="padding-left">3pt</xsl:attribute>
        <xsl:attribute name="padding-top">3pt</xsl:attribute>
        <xsl:attribute name="padding-right">3pt</xsl:attribute>
        <xsl:attribute name="padding-bottom">1.5pt</xsl:attribute>
    </xsl:attribute-set>

<!-- ADMONITION DEFAULTS -->
    <xsl:attribute-set name="admonition_block-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="space-before">6pt</xsl:attribute>
        <xsl:attribute name="space-after">6pt</xsl:attribute>
        <xsl:attribute name="border">1pt solid grey</xsl:attribute>
        <xsl:attribute name="padding">0.5em</xsl:attribute>
        <xsl:attribute name="background-color">honeydew</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="attention_block-defaults" use-attribute-sets="admonition_block-defaults" />
    <xsl:attribute-set name="caution_block-defaults" use-attribute-sets="admonition_block-defaults" />
    <xsl:attribute-set name="danger_block-defaults" use-attribute-sets="admonition_block-defaults" />
    <xsl:attribute-set name="error_block-defaults" use-attribute-sets="admonition_block-defaults" />
    <xsl:attribute-set name="hint_block-defaults" use-attribute-sets="admonition_block-defaults" />
    <xsl:attribute-set name="important_block-defaults" use-attribute-sets="admonition_block-defaults" />
    <xsl:attribute-set name="note_block-defaults" use-attribute-sets="admonition_block-defaults" />
    <xsl:attribute-set name="tip_block-defaults" use-attribute-sets="admonition_block-defaults" />
    <xsl:attribute-set name="warning_block-defaults" use-attribute-sets="admonition_block-defaults" />

    <xsl:attribute-set name="topic_block-defaults" use-attribute-sets="admonition_block-defaults" >
        <xsl:attribute name="border">1pt double grey</xsl:attribute>
        <xsl:attribute name="background-color">mintcream</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="marker_text-defaults">
        <!-- FOP bug requires overriding most of parent's attributes to
             prevent that style being applied when retrieved -->
        <xsl:attribute name="font-family">OfficinaSerifBook,serif</xsl:attribute>
        <xsl:attribute name="font-weight">normal</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="font-size">10pt</xsl:attribute>
        <xsl:attribute name="line-height">0pt</xsl:attribute>
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">3.0</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">3.0</xsl:attribute>
        <xsl:attribute name="margin">0pt</xsl:attribute>
        <xsl:attribute name="border">0pt</xsl:attribute>
        <xsl:attribute name="padding">0pt</xsl:attribute>
        <xsl:attribute name="color">black</xsl:attribute>
    </xsl:attribute-set>

<!-- CHAPTER TITLE PAGES -->
    <!-- sets overall title block -->
    <xsl:attribute-set name="title-chapter" use-attribute-sets="title-defaults">
        <xsl:attribute name="font-size">24pt</xsl:attribute>
        <xsl:attribute name="line-height">24pt</xsl:attribute>
        <xsl:attribute name="color">white</xsl:attribute>
        <xsl:attribute name="background-color">DarkGreen</xsl:attribute>
        <xsl:attribute name="padding-top">4pt</xsl:attribute>
        <xsl:attribute name="padding-left">1em</xsl:attribute>
        <xsl:attribute name="padding-right">1em</xsl:attribute>
        <xsl:attribute name="space-after">3pt</xsl:attribute>
    </xsl:attribute-set>

    <!-- sets title text within block -->
    <xsl:attribute-set name="chapter-title" use-attribute-sets="title-chapter">
        <xsl:attribute name="background-color">transparent</xsl:attribute>
        <xsl:attribute name="margin-right">1em</xsl:attribute>
        <xsl:attribute name="padding-left">0pt</xsl:attribute>
        <xsl:attribute name="padding-right">0pt</xsl:attribute>
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
    </xsl:attribute-set>

    <!-- sets chapter title chapter number within block -->
    <xsl:attribute-set name="chapter-number" use-attribute-sets="title-chapter">
        <xsl:attribute name="background-color">transparent</xsl:attribute>
        <xsl:attribute name="text-align">right</xsl:attribute>
        <xsl:attribute name="padding-left">0pt</xsl:attribute>
        <xsl:attribute name="padding-right">0pt</xsl:attribute>
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="chapter-toc" use-attribute-sets="title-defaults">
        <xsl:attribute name="space-before">24pt</xsl:attribute>
        <xsl:attribute name="text-align">right</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="chapter-rule">
        <xsl:attribute name="font-size">24pt</xsl:attribute>
        <xsl:attribute name="padding-left">1em</xsl:attribute>
        <xsl:attribute name="padding-right">1em</xsl:attribute>
        <xsl:attribute name="space-before">18pt</xsl:attribute>
        <xsl:attribute name="border-top">1.5pt solid DarkGreen</xsl:attribute>
    </xsl:attribute-set>

<!-- DOC REV DEFAULTS -->
    <xsl:attribute-set name="docrev_block-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="font-size">8pt</xsl:attribute>
        <xsl:attribute name="space-before">0pt</xsl:attribute>
        <xsl:attribute name="text-align-last">justify</xsl:attribute>
        <xsl:attribute name="wrap-option">no-wrap</xsl:attribute>
    </xsl:attribute-set>

<!-- TITLE DEFAULTS -->
    <xsl:attribute-set name="title-defaults">
        <xsl:attribute name="font-family">OfficinaSerifBold,serif</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
        <xsl:attribute name="font-size">10pt</xsl:attribute>
        <xsl:attribute name="line-height">12pt</xsl:attribute>
        <xsl:attribute name="space-before">6pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">0.5</xsl:attribute>
        <xsl:attribute name="space-after">6pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">0.5</xsl:attribute>
        <xsl:attribute name="text-align">left</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level1" use-attribute-sets="title-defaults">
        <xsl:attribute name="font-size">18pt</xsl:attribute>
        <xsl:attribute name="line-height">24pt</xsl:attribute>
        <xsl:attribute name="space-before">18pt</xsl:attribute>
        <xsl:attribute name="space-before.precedence">0.75</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
        <xsl:attribute name="space-after.precedence">0.75</xsl:attribute>
    </xsl:attribute-set>

        <xsl:attribute-set name="title-level2" use-attribute-sets="title-defaults">
            <xsl:attribute name="font-size">14pt</xsl:attribute>
            <xsl:attribute name="line-height">14pt</xsl:attribute>
            <xsl:attribute name="space-before">12pt</xsl:attribute>
            <xsl:attribute name="space-before.precedence">0.75</xsl:attribute>
            <xsl:attribute name="space-after">0pt</xsl:attribute>
            <xsl:attribute name="space-after.precedence">0.75</xsl:attribute>
        </xsl:attribute-set>

            <xsl:attribute-set name="title-level3" use-attribute-sets="title-defaults">
                <xsl:attribute name="font-size">12pt</xsl:attribute>
                <xsl:attribute name="line-height">12pt</xsl:attribute>
                <xsl:attribute name="space-before">12pt</xsl:attribute>
                <xsl:attribute name="space-before.precedence">0.75</xsl:attribute>
                <xsl:attribute name="space-after">0pt</xsl:attribute>
                <xsl:attribute name="space-after.precedence">0.75</xsl:attribute>
            </xsl:attribute-set>

                <xsl:attribute-set name="title-level4up" use-attribute-sets="title-defaults">
                </xsl:attribute-set>


<!-- MISC INLINES & SMALL BITS -->
    <xsl:attribute-set name="docutil_message-defaults" use-attribute-sets="paragraph-defaults">
        <xsl:attribute name="color">darkblue</xsl:attribute>
        <xsl:attribute name="margin-left">-6pt</xsl:attribute>
        <xsl:attribute name="border">0pt</xsl:attribute>
        <xsl:attribute name="border-left">3pt solid darkblue</xsl:attribute>
        <xsl:attribute name="padding-left">3pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="comment-defaults" use-attribute-sets="docutil_message-defaults">
        <xsl:attribute name="color">darkred</xsl:attribute>
        <xsl:attribute name="border-left">3pt solid darkred</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="problem-defaults" use-attribute-sets="docutil_message-defaults" />

    <xsl:attribute-set name="system_message-defaults" use-attribute-sets="docutil_message-defaults">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="unknown_tag-defaults" use-attribute-sets="docutil_message-defaults">
        <xsl:attribute name="color">red</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="border-left">3pt solid red</xsl:attribute>
    </xsl:attribute-set>
</xsl:stylesheet>
