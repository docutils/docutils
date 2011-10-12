<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!--
    TODO

    Make different templates for numbered and unnumbered verse, to keep the 
    result clean
    -->
    
    <!-- $Id: line_block.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->
    <xsl:attribute-set name="outer-line-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>


    <!--add text-align-last=justfiy only for numbered verse, so change-->
    <xsl:attribute-set name="level1-line-block">
        <xsl:attribute name="start-indent">10mm</xsl:attribute>
    </xsl:attribute-set>
    
    <xsl:attribute-set name="level2-line-block">
        <xsl:attribute name="start-indent">20mm</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="level3-line-block">
        <xsl:attribute name="start-indent">30mm</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="level4-line-block">
        <xsl:attribute name="start-indent">40mm</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="level5-line-block">
        <xsl:attribute name="start-indent">50mm</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="stanza-title-block">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="space-before">12</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

    <xsl:template match="line_block">
        <xsl:variable name="level" select="count(ancestor::line_block) + 1"/>
        <xsl:choose>
            <xsl:when test="not(parent::line_block) ">
                <fo:block xsl:use-attribute-sets="outer-line-block" role="line-block">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template> 


    <xsl:template match="line[title_reference]|line[inline[@classes='title']]" priority="2">
        <fo:block xsl:use-attribute-sets="stanza-title-block" role="stanza-title">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="line/title_reference" priority="2">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="line">
        <xsl:variable name="level" select="count(ancestor::line_block)"/>
        <xsl:choose>
            <xsl:when test="normalize-space(.) = ''">
                <fo:block>
                    <xsl:text>&#x00a0;</xsl:text>
                </fo:block>
            </xsl:when>

            <xsl:when test="$level = 1">
                <fo:block xsl:use-attribute-sets="level1-line-block" role="line">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 2">
                <fo:block xsl:use-attribute-sets="level2-line-block" role="line">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 3">
                <fo:block xsl:use-attribute-sets="level3-line-block" role="line">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 4">
                <fo:block xsl:use-attribute-sets="level4-line-block" role="line">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 5">
                <fo:block xsl:use-attribute-sets="level5-line-block" role="line">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:otherwise>
                <xsl:variable name="msg">
                    <xsl:text>Cannot process line_blocks more than 5 levels deep.</xsl:text>
                </xsl:variable>
                <xsl:call-template name="error-message">
                    <xsl:with-param name="text" select="$msg"/>
                </xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>



    <xsl:template match="inline[@classes='title']"/>

</xsl:stylesheet>
