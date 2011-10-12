<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <xsl:template name="get-line-number">
        <xsl:variable name="top-block-id">
            <xsl:for-each select="ancestor::line_block[last()]">
                <xsl:value-of select="generate-id()"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:number from="line_block[generate-id() = $top-block-id]" 
            count="line[normalize-space(.) != ''][not(title_reference)][not(inline[@classes='title'])]" level="any"/>
    </xsl:template>

    <xsl:template name="number-line">
        <xsl:variable name="num">
            <xsl:call-template name="get-line-number"/>
        </xsl:variable>
            <xsl:choose>
                <xsl:when test="$num != 0 and ($num +1) mod $number-verse = 1">
                    <fo:leader leader-pattern="space" />
                    <xsl:value-of select="$num"/>
                </xsl:when>
                <xsl:otherwise>
                    <fo:leader leader-pattern="space" />
                </xsl:otherwise>
            </xsl:choose>
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
                <fo:block xsl:use-attribute-sets="level1-line-block" text-align-last = "justify" role="line">
                    <xsl:apply-templates/>
                    <xsl:call-template name="number-line"/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 2">
                <fo:block xsl:use-attribute-sets="level2-line-block" text-align-last = "justify" role="line">
                    <xsl:apply-templates/>
                    <xsl:call-template name="number-line"/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 3">
                <fo:block xsl:use-attribute-sets="level3-line-block" text-align-last = "justify" role="line">
                    <xsl:apply-templates/>
                    <xsl:call-template name="number-line"/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 4">
                <fo:block xsl:use-attribute-sets="level4-line-block" text-align-last = "justify" role="line">
                    <xsl:apply-templates/>
                    <xsl:call-template name="number-line"/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 5">
                <fo:block xsl:use-attribute-sets="level5-line-block" text-align-last = "justify" role="line">
                    <xsl:apply-templates/>
                    <xsl:call-template name="number-line"/>
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
