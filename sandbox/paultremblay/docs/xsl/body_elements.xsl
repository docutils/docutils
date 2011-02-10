<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">document/paragrah|section/paragraph"</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the default paragraph.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">document/paragrah|section/paragraph"</xsl:with-param> 
            <xsl:with-param name="inherits">paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first default paragraph.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='literal-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">document/literal_block|section/literal_block"</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the literal text.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='transition-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">document/transition|section/transition"</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the transition block. The actutal text for this block is set by the 'transition-text' 
            parameter.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='document-title-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">document/title"</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the title for the document.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='document-subtitle-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">document/subtitle"</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the subtitle of the document.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='document-title-page-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">document/title|document/subtitle"</xsl:with-param> 
        </xsl:call-template>
        <block>
            The block that wraps both the title and subtitle. This block only gets written
            if the title and subtitle occur in the front section, or TOC section.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='block-quote-outer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">block_quote</xsl:with-param> 
        </xsl:call-template>
        <block>
            The attribute set that formats the block that wraps the other blocks in a block quote. Use the attribute
            set to format space after or space before, etc.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='block-quote-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">block_quote/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            The attribute set that formats the paragraphs in the block quote. A different set of attributes
            controls the first paragraph (see below). Use this attribute set to set the space 
            between paragraphs with the 'space-before' attribute.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='block-quote-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">block_quote/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">block-quote-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            The attribute set that formats the first paragraph in the block quote.
            It inherits all the attributes from 'block-quote-first-paragraph-block' and 
            then sets the 'space-before' to 0. It does not make sense to modify 
            attributes in this attribute set directly.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='block-quote-attribution-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">block_quote/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">block-quote-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            This attribute set the attribution in a block quote. 
        </block>
    </xsl:template>


</xsl:stylesheet>
