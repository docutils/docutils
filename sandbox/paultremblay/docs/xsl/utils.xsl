<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">



    <xsl:template name="make-border">
        <xsl:param name="length"/>
        <xsl:param name="border-text">-</xsl:param>
        <xsl:if test="$length != 0">
            <xsl:value-of select="$border-text"/>
            <xsl:call-template name="make-border">
                <xsl:with-param name="border-text" select="$border-text"/>
                <xsl:with-param name="length" select="$length - 1"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <xsl:template name="make-title">
        <xsl:param name="level">3</xsl:param>
        <xsl:param name="text"/>
        <xsl:choose>
            <xsl:when test="$level = 3">
                <xsl:value-of select="$text"/>
                <xsl:text>&#xA;</xsl:text>
                <xsl:call-template name="make-border">
                    <xsl:with-param name="length" select="string-length(@name)"/>
                </xsl:call-template>
                <xsl:text>&#xA;&#xA;</xsl:text>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="make-defaults">
        <xsl:text>:defaults:</xsl:text>
        <xsl:text>&#xA;&#xA;</xsl:text>
        <xsl:for-each select="xsl:attribute">
            <xsl:text>     </xsl:text>
            <xsl:value-of select="@name"/>
            <xsl:text>: </xsl:text>
            <xsl:value-of select="."/>
            <xsl:text>&#xA;&#xA;</xsl:text>
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="before-desc">
        <xsl:param name="fo"/>
        <xsl:param name="docutils"/>
        <xsl:call-template name="make-title">
            <xsl:with-param name="text" select="@name"/>
        </xsl:call-template>
        <xsl:text>:fo: </xsl:text>
        <xsl:value-of select="$fo"/>
        <xsl:text>&#xA;&#xA;</xsl:text>
        <xsl:text>:docutils: </xsl:text>
        <xsl:value-of select="$docutils"/>
        <xsl:text>&#xA;&#xA;</xsl:text>
        <xsl:call-template name="make-defaults"/>
    </xsl:template>

</xsl:stylesheet>
