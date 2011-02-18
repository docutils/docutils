<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='toc-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "contents"]</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block that wraps the TOC.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-title-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "contents"]/title</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block for the title for the TOC.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-entry-defaults-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">None</xsl:with-param> 
            <xsl:with-param name="docutils">None</xsl:with-param> 
        </xsl:call-template>
        <block>
            Sets up the defaults for the TOC entries.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='toc-level1-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "contents"]/bullet_list/list_item/paragraph/</xsl:with-param> 
            <xsl:with-param name="inherits">toc-entry-defaults-block</xsl:with-param>
        </xsl:call-template>
        <block>
            Formats the block for the level 1 table of contents entry. If a number exists, it is 
            formatted according to the parameter 'number-section1'.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-level2-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "contents"]/bullet_list/list_item/bullet_list/list_item/paragraph/</xsl:with-param> 
            <xsl:with-param name="inherits">toc-entry-defaults-block</xsl:with-param>
        </xsl:call-template>
        <block>
            Formats the block for the level 2 table of contents entry. If a number exists, it is 
            formatted according to the parameter 'number-section2'.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-level3-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "contents"]/bullet_list/list_item/bullet_list/list_item/paragraph/etc.</xsl:with-param> 
            <xsl:with-param name="inherits">toc-entry-defaults-block</xsl:with-param>
        </xsl:call-template>
        <block>
            Formats the block for the level 3 table of contents entry. If a number exists, it is 
            formatted according to the parameter 'number-section3'.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-level4-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic[@classes = "contents"]/bullet_list/list_item/bullet_list/list_item/paragraph/etc.</xsl:with-param> 
            <xsl:with-param name="inherits">toc-entry-defaults-block</xsl:with-param>
        </xsl:call-template>
        <block>
            Formats the block for the level 4 table of contents entry. If a number exists, it is 
            formatted according to the parameter 'number-section4'.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-level5-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="docutils">topic[@classes = "contents"]/bullet_list/list_item/bullet_list/list_item/paragraph/etc.</xsl:with-param> 
            <xsl:with-param name="inherits">toc-entry-defaults-block</xsl:with-param>
        </xsl:call-template>
        <block>
            Formats the block for the level 5 table of contents entry. If a number exists, it is 
            formatted according to the parameter 'number-section5'.
        </block>
    </xsl:template>

</xsl:stylesheet>
