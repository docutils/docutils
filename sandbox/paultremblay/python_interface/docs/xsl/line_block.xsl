<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='outer-line-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">line_block</xsl:with-param> 
        </xsl:call-template>
        <block>
            The outer block containing the blocks of lines. Use the outer block to
            set space before or after the verse.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='level1-line-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">line_block/line</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attribute sets for the first level of lines.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='level2-line-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">line_block/line_block/line</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attribute sets for the second level of lines.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='level3-line-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">line_block/line_block/line_block/line</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attribute sets for the third level of lines.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='level4-line-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">line_block/line_block/line_block/line_block/line</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attribute sets for the fourth level of lines.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='level5-line-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">line_block/line_block/line_block/line_block/line_block/line</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attribute sets for the fifth level of lines.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='stanza-title-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">line_block/title_reference</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the title of a stanza.
        </block>
    </xsl:template>


</xsl:stylesheet>
