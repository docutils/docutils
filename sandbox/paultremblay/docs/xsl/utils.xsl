<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">


    <!-- $Id$ -->

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
        <xsl:param name="level">5</xsl:param>
        <xsl:param name="text"/>
        <xsl:choose>
            <xsl:when test="$level = 1">
                <block bottom-border="#" bottom-border-length="text" top-border="#" top-border-length="text">
                <xsl:value-of select="$text"/>
                </block>
            </xsl:when>
            <xsl:when test="$level = 2">
                <block bottom-border="^" bottom-border-length="text" top-border="^" top-border-length="text">
                <xsl:value-of select="$text"/>
                </block>
            </xsl:when>
            <xsl:when test="$level = 3">
                <block bottom-border="=" bottom-border-length="text" >
                <xsl:value-of select="$text"/>
                </block>
            </xsl:when>
            <xsl:when test="$level = 5">
                <block bottom-border="-" bottom-border-length="text">
                <xsl:value-of select="$text"/>
                </block>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="make-defaults">
        <block>:defaults:</block>
        <xsl:for-each select="xsl:attribute">
            <block left-indent="5">
                <xsl:value-of select="@name"/>
                <xsl:text>: </xsl:text>
                <xsl:value-of select='.'/>
            </block>
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="make-defaults_old">
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
        <xsl:param name="inherits"/>
        <xsl:call-template name="make-title">
            <xsl:with-param name="text" select="@name"/>
        </xsl:call-template>
        <block>
            <xsl:text>:fo: </xsl:text> 
            <xsl:value-of select="$fo"/>
        </block>
        <block first-line-indent="-9">
            <xsl:text>:docutils: </xsl:text> 
            <xsl:value-of select="$docutils"/>
        </block>
        <!--
        <block literal="true">
            <xsl:text>&#xA;&#xA;</xsl:text>
            <xsl:text>:docutils: </xsl:text> 
            <xsl:value-of select="$docutils"/>
            <xsl:text>&#xA;&#xA;</xsl:text>
        </block>
        -->
        <xsl:if test="$inherits != ''">
            <block>
                <xsl:text>:inherits: </xsl:text> 
                <xsl:value-of select="$inherits"/>
            </block>
        </xsl:if>
        <xsl:call-template name="make-defaults"/>
    </xsl:template>

</xsl:stylesheet>
