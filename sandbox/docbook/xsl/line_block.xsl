<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template match="line_block/line_block"/>

    <xsl:template match="line_block">
        <xsl:variable name="num-blank-lines">
            <xsl:value-of select="count(descendant::line[normalize-space(.) = ''])"/>
        </xsl:variable>
        <d:poetry>
            <xsl:choose>
                <xsl:when test="$num-blank-lines = 0">
                    <xsl:for-each select="descendant::line">
                        <xsl:call-template name="process-line">
                            <xsl:with-param name="num-working-on" select="0"/>
                        </xsl:call-template>
                    </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:call-template name="group-lines">
                        <xsl:with-param name="total-blank" select="$num-blank-lines"/>
                    </xsl:call-template>
                </xsl:otherwise>
            </xsl:choose>
        </d:poetry>
    </xsl:template>

    <xsl:template name="group-lines">
        <xsl:param name="num-working-on">0</xsl:param>
        <xsl:param name="total-blank"/>
        <xsl:if test="$num-working-on &lt;= $total-blank">
            <d:linegroup role="stanza">
                <xsl:for-each select="descendant::line">
                    <xsl:call-template name="process-line">
                        <xsl:with-param name="num-working-on" select="$num-working-on"/>
                    </xsl:call-template>
                </xsl:for-each>
            </d:linegroup>
            <xsl:call-template name="group-lines">
                <xsl:with-param name="total-blank" select="$total-blank"/>
                <xsl:with-param name="num-working-on" select="$num-working-on + 1"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <xsl:template name="process-line">
        <xsl:param name="num-working-on"/>
        <xsl:variable name="previous-blanks">
            <xsl:call-template name="get-num-blank-lines"/>
        </xsl:variable>
        <xsl:variable name="level" select="count(ancestor::line_block)"/>
        <xsl:variable name="n">
            <xsl:call-template name="get-line-number"/>
        </xsl:variable>
        <xsl:if test="$num-working-on = $previous-blanks">
            <xsl:choose>
                <xsl:when test="title_reference|inline[@classes='title']">
                    <d:title>
                        <xsl:apply-templates/>
                    </d:title>
                </xsl:when>
                <xsl:when test="normalize-space(.) = ''"/>
                <xsl:otherwise>
                    <!--
                    <d:line n="{$n}" level="{$level}">
                    -->
                    <d:line role="level:{$level};number:{$n}">
                        <xsl:apply-templates/>
                    </d:line>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:if>
    </xsl:template>


    <xsl:template name="get-num-blank-lines">
        <xsl:variable name="top-block-id">
            <xsl:for-each select="ancestor::line_block[last()]">
                <xsl:value-of select="generate-id()"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:number from="line_block[generate-id() = $top-block-id]" 
            count="line[normalize-space(.) = ''][not(title_reference)][not(inline[@classes='title'])]" level="any"/>
    </xsl:template>


    <xsl:template name="get-line-number">
        <xsl:variable name="top-block-id">
            <xsl:for-each select="ancestor::line_block[last()]">
                <xsl:value-of select="generate-id()"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:number from="line_block[generate-id() = $top-block-id]" 
            count="line[normalize-space(.) != ''][not(title_reference)][not(inline[@classes='title'])]" level="any"/>
    </xsl:template>

    <xsl:template match="line/title_reference|inline[@classes='title']">
        <xsl:apply-templates/>
    </xsl:template>


    <xsl:template match="comment()">
        <xsl:comment>
            <xsl:value-of select="."/>
        </xsl:comment>
    </xsl:template>
    
</xsl:stylesheet>
