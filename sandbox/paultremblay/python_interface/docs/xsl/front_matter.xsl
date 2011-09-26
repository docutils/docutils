<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->


    <xsl:template match= "xsl:attribute-set[@name='dedication-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "dedication"]</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the dedication text, including the title and subsequent paragraphs, by
            wrapping them in a block.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='abstract-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "abstract"]</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the abstract text, including the title and subsequent paragraphs, by
            wrapping them in a block.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='dedication-title-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "dedication"]/title</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the title for the dedication.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='abstract-title-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "abstract"]/title</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the abstract title.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='dedication-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "dedication"]/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the paragraphs of the dedication.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='dedication-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "dedication"]/paragraph</xsl:with-param> 
            <xsl:with-param name="inherits">dedication-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first paragraph of the dedication.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='abstract-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "abstract"]/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the paragraphs of the abstract.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='abstract-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "abstract"]/paragraph</xsl:with-param> 
            <xsl:with-param name="inherits">abstract-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first paragraph of the abstract.
        </block>
    </xsl:template>


</xsl:stylesheet>
