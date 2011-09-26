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

    <xsl:template name="make-name">
        <xsl:param name="name"/>
        <xsl:call-template name="make-title">
            <xsl:with-param name="level">3</xsl:with-param>
            <xsl:with-param name="text">
                <xsl:choose>
                    <xsl:when test="$name = ''">
                        <xsl:value-of select="@name"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="$name"/>
                    </xsl:otherwise>
                </xsl:choose>
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
        <xsl:choose>
            <xsl:when test="$text = 'Any Measure' or $text = 'Any Text' or 
                $text = 'Valid Number Formatting String'">
                <block>
                    <xsl:text>**Possible Values**: </xsl:text>
                    <xsl:value-of select="$text"/>
                </block>
            </xsl:when>
            <xsl:otherwise>
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
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="xsl:param[@name='strict']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>True, False, ''</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            If set to True, stylesheet processing quits when an error is found.
        </block>
    </xsl:template>

    <xsl:template name="p-defaults">
        <block>
            <xsl:text>**Default:** </xsl:text>
            <xsl:value-of select="."/>
        </block>
    </xsl:template>

    <xsl:template name="before_p_text">
        <xsl:param name="possible-values"/>
        <xsl:call-template name="make-name"/>
        <xsl:call-template name="possible-values">
            <xsl:with-param name="text" select="$possible-values"/>
        </xsl:call-template>
        <xsl:call-template name="p-defaults"/>
    </xsl:template>


</xsl:stylesheet>
