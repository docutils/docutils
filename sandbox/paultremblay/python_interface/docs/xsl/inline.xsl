<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='emphasis-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">emphasis</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the emphasis element.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='strong-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">strong</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the strong element.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='basic-link-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">basic_link</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the basic_link element.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='literal-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">literal</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the literal element.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='title-reference-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">title-reference</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the title_reference element.
        </block>
    </xsl:template>

</xsl:stylesheet>
