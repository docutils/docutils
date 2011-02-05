<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:include href="comment.xsl"/>
    <xsl:include href="utils.xsl"/>

    <xsl:output method="xml"/>

    <xsl:template match="xsl:stylesheet">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="/">
        <doc>
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">1</xsl:with-param>
                <xsl:with-param name="text">XSL-FO Documentation</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">2</xsl:with-param>
                <xsl:with-param name="text">Parameters</xsl:with-param>
            </xsl:call-template>
            <block>.. contents:: Table of Contents</block>
            <xsl:apply-templates/>
        </doc>
    </xsl:template>

    <xsl:template name="make-name">
        <xsl:call-template name="make-title">
            <xsl:with-param name="level">3</xsl:with-param>
            <xsl:with-param name="text">
                <xsl:value-of select="@name"/>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="possible-values-recurs">
        <xsl:param name="text"/>
        <xsl:param name="final-string">**Possible Values**: </xsl:param>
        <xsl:variable name="value" select="normalize-space(substring-before($text, ','))"/>
        <xsl:choose>
            <xsl:when test="$value != ''">
                <xsl:call-template name="possible-values-recurs">
                    <xsl:with-param name="text" select="substring-after($text, ',')"/>
                    <xsl:with-param name="final-string">
                        <xsl:value-of select="$final-string"/>
                        <xsl:text>``</xsl:text>
                        <xsl:value-of select="$value"/>
                        <xsl:text>``, </xsl:text>
                    </xsl:with-param>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="substring($final-string, 1, string-length($final-string) - 2)"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="possible-values">
        <xsl:param name="text"/>
        <xsl:variable name="parsed-text">
            <xsl:choose>
                <xsl:when test="substring($text, string-length($text),  1) = ','">
                    <xsl:value-of select="$text"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$text"/>
                    <xsl:text>,</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <block>
            <xsl:call-template name="possible-values-recurs">
                <xsl:with-param name="text" select="$parsed-text"/>
            </xsl:call-template>
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='strict']" priority = "3">
        <xsl:call-template name="make-name"/>
        <xsl:call-template name="possible-values">
            <xsl:with-param name="text">
                <xsl:text>True,False,''</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            If set to True, stylesheet processing quits when an error is found.
        </block>
    </xsl:template>


    <xsl:template match="xsl:param" priority="-1">
        <xsl:message>
            <xsl:text>no match for "</xsl:text>
            <xsl:value-of select="@name"/>
            <xsl:text>"</xsl:text>
        </xsl:message>
    </xsl:template>

    <xsl:template match="@*|node()" />

    
</xsl:stylesheet>
