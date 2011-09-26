<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->
    <xsl:template match= "xsl:attribute-set[@name='figure-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">figure</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block that wraps the figure. Use this attribute set to set 
            properties on  the image, caption, and legend, as well as to set the space 
            before and after the figure.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='image-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">image</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block that wraps the image, both for an image by itself, and 
            for an image included in a figure. Use this attribute set to control the space
            before and after the image, as well as to align the image itself.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='figure-caption-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">figure/caption</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block that wraps the caption.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='figure-legend-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">figure/legend</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block that wraps the legend. The paragrahs in the legend have
            their own blocks.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='legend-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">figure/legend/paragaph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block that wraps the paragraphs in the legend.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='legend-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">figure/legend/paragaph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">legend-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first block that wraps the paragraphs in the legend.
        </block>
    </xsl:template>

    
</xsl:stylesheet>

