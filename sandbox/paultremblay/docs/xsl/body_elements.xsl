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
</xsl:stylesheet>
