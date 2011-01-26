<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >



    <xsl:template name="count-lines">
        <xsl:variable name="top-block-id">
            <xsl:for-each select="ancestor::line_block[last()]">
                <xsl:value-of select="generate-id()"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:number from="line_block[generate-id() = $top-block-id]" 
            count="line[normalize-space(.) != ''][not(contains(., 'rst-title'))]" level="any"/>
    </xsl:template>

    <xsl:template match="line[contains(., 'rst-title:']">
        <xsl:message>hit</xsl:message>
        
    </xsl:template>

    <xsl:template match="line[contains(., 'rst-title:']/text()">
        <xsl:message>hit2</xsl:message>
        
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
                    <xsl:call-template name="make-number"/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 2">
                <fo:block xsl:use-attribute-sets="level2-line-block" role="line">
                    <xsl:apply-templates/>
                    <xsl:call-template name="make-number"/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 3">
                <fo:block xsl:use-attribute-sets="level3-line-block" role="line">
                    <xsl:apply-templates/>
                    <xsl:call-template name="make-number"/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 4">
                <fo:block xsl:use-attribute-sets="level4-line-block" role="line">
                    <xsl:apply-templates/>
                    <xsl:call-template name="make-number"/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 5">
                <fo:block xsl:use-attribute-sets="level5-line-block" role="line">
                    <xsl:apply-templates/>
                    <xsl:call-template name="make-number"/>
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

</xsl:stylesheet>
