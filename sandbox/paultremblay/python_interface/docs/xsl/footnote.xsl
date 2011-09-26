<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='default-footnote-label-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">None</xsl:with-param> 
        </xsl:call-template>
        <block>
            Sets of the defaults for the label (1, \*, etc), of each label.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footnote']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:footnote</xsl:with-param> 
            <xsl:with-param name="docutils">footnote</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the footnote. By default, it sets properties to neutral, so 
            that it does not inherit any unwanted properties, such as from a 
            definition term.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footnote-separator-flow']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:flow</xsl:with-param> 
            <xsl:with-param name="docutils">footnote</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the flow of the footnote.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footnote-separator-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">footnote</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block (with the leader) that separates the footnotes from the rest
            of the page.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='footnote-label-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">footnote/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">default-footnote-label-inline</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the label for *traditional* footnotes and endnotes at the bottomm of the page
            or with the endnotes. This attribute set does not affect the label for footnotes and 
            endnotes formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footnote-body-label-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">footnote/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">default-footnote-label-inline</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the label for *traditional* footnotes and endnotes in the body
            of the text. This attribute set does not affect the label for footnotes and 
            endnotes formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footnote-list-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-block</xsl:with-param> 
            <xsl:with-param name="docutils">footnote</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the list that contains the footnote. The 'provisional-distance-between-starts'
            controls how far away the footnote label is from the text.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footnote-item-label']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-label</xsl:with-param> 
            <xsl:with-param name="docutils">footnote</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the item-label when the footnote or endnote is formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footnote-label-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">footnote_reference</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block in item-label when the footnote or endnote is formatted as a list.
            By default, the label has no superscript (as opposed to when formatting a 
            "traditional" footnote.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='footnote-item-body']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-body</xsl:with-param> 
            <xsl:with-param name="docutils">footnote</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the item-body when the footnote or endnote is formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footnote-body']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:footnote-body</xsl:with-param> 
            <xsl:with-param name="docutils">footnote</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the body of the footnote. Space-after and space-before seem 
            to have no affect, at least with fop.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footnote-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">footnote/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the paragraphs in the body of a footnote or endnote. Use the 'space-before' 
            to set the space between each paragraphs, for footnotes or endnotes 
            with multiple paragraphs. In addition, for traditional footnotes, use the
            'text-indent="18pt" to create a traditional footnote. (The deault does not
            do this, in order to accommodate the footnote-as-a-list.)
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footnote-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">footnote/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">footnote-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first paragraphs in the body of a footnote or endnote. It inherits
            all the attributes from the 'footnote-paragraphs-block' and sets the space-before
            to 0. It does not make sense to change attributes on this block directly.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='endnotes-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">footnote</xsl:with-param> 
        </xsl:call-template>
        <block>
            The block that wraps all the other blocks of the endnotes. Use to create a 
            page break before, or to create space before and after the endnotes.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='endnote-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">footnote</xsl:with-param> 
        </xsl:call-template>
        <block>
            The block that wraps each individual endnote ('footnote' in docutils). Use 
            to control the spacing between each endnote.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='endnote-first-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">footnote</xsl:with-param> 
            <xsl:with-param name="inherits">endnote-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            The block that wraps each the first endnote ('footnote' in docutils). It 
            does not make sense to change attributes on this set directly.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='endnotes-title-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">rubric[@classes='endnotes']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the title for the endnotes, when one is present.
        </block>
    </xsl:template>

</xsl:stylesheet>
