<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

        <!--
        -->

    <xsl:param name="terminate-on-csv-error"/>
    <xsl:param name="error-no-delim-aft-quote">true</xsl:param>

    <xsl:template name="handle-token">
        <xsl:param name="position"/>
        <xsl:param name="desired-number"/>
        <xsl:param name="string"/>
        <xsl:param name="invalid"/>
        <xsl:param name="action"/>
        <xsl:param name="last"/>
        <xsl:if test="$invalid != '' and $terminate-on-csv-error != ''">
            <xsl:message terminate="yes">
                <xsl:text>Now quiting</xsl:text>
            </xsl:message>
        </xsl:if>
            <token invalid="{$invalid}">
                <xsl:value-of select="$string"/>
            </token>
    </xsl:template>


    <xsl:template name="csv-split">
        <xsl:param name="string"/>
        <xsl:param name="sep-char"/>
        <xsl:param name="action"/>
        <xsl:param name="desired-number"/>
        <xsl:if test="$sep-char = ''">
            <xsl:message terminate="yes">
                <xsl:text>Must have a sep-char&#x00A;</xsl:text>
                <xsl:text>Now quiting</xsl:text>
            </xsl:message>
        </xsl:if>
        <xsl:call-template name="csv-string">
            <xsl:with-param name="string" select="$string"/>
            <xsl:with-param name="sep-char" select="$sep-char"/>
            <xsl:with-param name="action" select="$action"/>
            <xsl:with-param name="desired-number" select="$desired-number"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="csv-string">
        <xsl:param name="string"/>
        <xsl:param name="sep-char"/>
        <xsl:param name="action"/>
        <xsl:param name="previous-string"/>
        <xsl:param name="position">1</xsl:param>
        <xsl:param name="desired-number"/>
        <xsl:param name="in-quote"/>
        <xsl:param name="merge-delimiters"/>
        <xsl:variable name="quote">"</xsl:variable> 
        <xsl:choose>
            <xsl:when test="$string = ''">
                <xsl:call-template name="handle-token">
                    <xsl:with-param name="last" select="'true'"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:when test="$merge-delimiters = 'true' and substring($string, 1, 1) = $sep-char and $in-quote != 'true'">
                <xsl:call-template name="csv-string">
                    <xsl:with-param name="string" select="substring($string,2)"/>
                    <xsl:with-param name="sep-char" select="$sep-char"/>
                    <xsl:with-param name="action" select="$action"/>
                    <xsl:with-param name="position" select="$position"/>
                    <xsl:with-param name="desired-number" select="$desired-number"/>
                    <xsl:with-param name="previous-string" select="$previous-string"/>
                    <xsl:with-param name="in-quote" select="$in-quote"/>
                    <xsl:with-param name="merge-delimiters" select="$merge-delimiters"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:when test="$in-quote = 'true'">
                <xsl:variable name="bef-quote" select="substring-before($string, $quote)"/>
                <xsl:variable name="aft-quote" select="substring-after($string, $quote)"/>
                <xsl:choose>
                    <xsl:when test="$bef-quote = '' and substring($string, 1, 1) != $quote">
                        <xsl:message>
                            <xsl:text>Not valid CSV</xsl:text>
                            <xsl:text>String is missing matching quote:&#xA;"</xsl:text>
                            <xsl:value-of select="$string"/>
                        </xsl:message>
                        <xsl:call-template name="handle-token">
                            <xsl:with-param name="invalid" select="'true'"/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="substring($aft-quote, 1, 1) = $quote">
                        <xsl:call-template name="csv-string">
                            <xsl:with-param name="string" select="substring($aft-quote,2)"/>
                            <xsl:with-param name="sep-char" select="$sep-char"/>
                            <xsl:with-param name="action" select="$action"/>
                            <xsl:with-param name="position" select="$position"/>
                            <xsl:with-param name="desired-number" select="$desired-number"/>
                            <xsl:with-param name="previous-string">
                                <xsl:value-of select="$previous-string"/>
                                <xsl:value-of select="$bef-quote"/>
                                <xsl:text>"</xsl:text>
                            </xsl:with-param>
                            <xsl:with-param name="in-quote" select="'true'"/>
                            <xsl:with-param name="merge-delimiters" select="$merge-delimiters"/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="handle-token">
                            <xsl:with-param name="position">
                                <xsl:value-of select="$position"/>
                            </xsl:with-param>
                            <xsl:with-param name="desired-number" select="$desired-number"/>
                            <xsl:with-param name="string">
                                <xsl:value-of select="$previous-string"/>
                                <xsl:value-of select="$bef-quote"/>
                            </xsl:with-param>
                            <xsl:with-param name="action">
                                <xsl:value-of select="$action"/>
                            </xsl:with-param>
                        </xsl:call-template>
                        <xsl:variable name="next">
                            <xsl:choose>
                                <xsl:when test="$aft-quote != '' and substring($aft-quote, 1, 1) != $sep-char">
                                    <xsl:value-of select="substring($aft-quote, 2)"/>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:value-of select="substring-after($aft-quote, $sep-char)"/>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:variable>
                        <xsl:choose>
                            <xsl:when test="$aft-quote != '' and substring($aft-quote, 1, 1) != $sep-char and 
                                normalize-space($error-no-delim-aft-quote) != ''">
                                <xsl:message>
                                    <xsl:text>Not valid CSV</xsl:text>
                                    <xsl:text> no "</xsl:text>
                                    <xsl:value-of select="$sep-char"/>
                                    <xsl:text>" (CSV separator) after quote character</xsl:text>
                                    <xsl:text>&#xA;the string is "</xsl:text>
                                    <xsl:value-of select="$string"/>
                                    <xsl:text>" and the error is '</xsl:text>
                                    <xsl:value-of select="substring($bef-quote, string-length($bef-quote) - 3)"/>
                                    <xsl:text>"</xsl:text>
                                    <xsl:value-of select="substring($aft-quote, 1, 1)"/>
                                    <xsl:text>'</xsl:text>
                                </xsl:message>
                                <xsl:call-template name="handle-token">
                                    <xsl:with-param name="invalid" select="'true'"/>
                                </xsl:call-template>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:call-template name="csv-string">
                                    <xsl:with-param name="string" select="$next"/>
                                    <xsl:with-param name="sep-char" select="$sep-char"/>
                                    <xsl:with-param name="action" select="$action"/>
                                    <xsl:with-param name="position" select="$position + 1"/>
                                    <xsl:with-param name="desired-number" select="$desired-number"/>
                                    <xsl:with-param name="merge-delimiters" select="$merge-delimiters"/>
                                </xsl:call-template>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:when test="contains($string, $quote) and contains($string, $sep-char)">
                <xsl:variable name="bef-quote" select="substring-before($string, $quote)"/>
                <xsl:variable name="aft-quote" select="substring-after($string, $quote)"/>
                <xsl:variable name="bef-sep-char" select="substring-before($string, $sep-char)"/>
                <xsl:choose>
                    <xsl:when test="string-length($bef-quote) &lt; string-length($bef-sep-char)">
                        <xsl:call-template name="csv-string">
                            <xsl:with-param name="string" select="$aft-quote"/>
                            <xsl:with-param name="position" select="$position"/>
                            <xsl:with-param name="desired-number" select="$desired-number"/>
                            <xsl:with-param name="in-quote" select="'true'"/>
                            <xsl:with-param name="sep-char" select="$sep-char"/>
                            <xsl:with-param name="action" select="$action"/>
                            <xsl:with-param name="merge-delimiters" select="$merge-delimiters"/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="handle-token">
                            <xsl:with-param name="position">
                                <xsl:value-of select="$position"/>
                            </xsl:with-param>
                            <xsl:with-param name="desired-number" select="$desired-number"/>
                            <xsl:with-param name="string">
                                <xsl:value-of select="substring-before($string, $sep-char)"/>
                            </xsl:with-param>
                            <xsl:with-param name="action">
                                <xsl:value-of select="$action"/>
                            </xsl:with-param>
                        </xsl:call-template>
                        <xsl:call-template name="csv-string">
                            <xsl:with-param name="string" select="substring-after($string, $sep-char)"/>
                            <xsl:with-param name="sep-char" select="$sep-char"/>
                            <xsl:with-param name="action" select="$action"/>
                            <xsl:with-param name="position" select="$position + 1"/>
                            <xsl:with-param name="desired-number" select="$desired-number"/>
                            <xsl:with-param name="merge-delimiters" select="$merge-delimiters"/>
                        </xsl:call-template>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:when test="contains($string, $quote)">
                <xsl:variable name="aft-quote" select="substring-after($string, $quote)"/>
                <xsl:call-template name="csv-string">
                    <xsl:with-param name="string" select="$aft-quote"/>
                    <xsl:with-param name="position" select="$position"/>
                    <xsl:with-param name="desired-number" select="$desired-number"/>
                    <xsl:with-param name="in-quote" select="'true'"/>
                    <xsl:with-param name="sep-char" select="$sep-char"/>
                    <xsl:with-param name="action" select="$action"/>
                    <xsl:with-param name="merge-delimiters" select="$merge-delimiters"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:when test="not(contains($string, $quote)) and contains($string, $sep-char)">
                <xsl:call-template name="handle-token">
                    <xsl:with-param name="position">
                        <xsl:value-of select="$position"/>
                    </xsl:with-param>
                    <xsl:with-param name="desired-number" select="$desired-number"/>
                    <xsl:with-param name="string">
                        <xsl:value-of select="substring-before($string, $sep-char)"/>
                    </xsl:with-param>
                    <xsl:with-param name="action">
                        <xsl:value-of select="$action"/>
                    </xsl:with-param>
                </xsl:call-template>
                <xsl:call-template name="csv-string">
                    <xsl:with-param name="string" select="substring-after($string, $sep-char)"/>
                    <xsl:with-param name="sep-char" select="$sep-char"/>
                    <xsl:with-param name="action" select="$action"/>
                    <xsl:with-param name="position" select="$position + 1"/>
                    <xsl:with-param name="desired-number" select="$desired-number"/>
                    <xsl:with-param name="merge-delimiters" select="$merge-delimiters"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:when test="not(contains($string, $quote))">
                <xsl:call-template name="handle-token">
                    <xsl:with-param name="position">
                        <xsl:value-of select="$position"/>
                    </xsl:with-param>
                    <xsl:with-param name="desired-number" select="$desired-number"/>
                    <xsl:with-param name="string">
                        <xsl:value-of select="$string"/>
                    </xsl:with-param>
                    <xsl:with-param name="action">
                        <xsl:value-of select="$action"/>
                    </xsl:with-param>
                    <xsl:with-param name="last">
                        <xsl:value-of select="'true'"/>
                    </xsl:with-param>
                </xsl:call-template>
            </xsl:when>
        </xsl:choose>
    </xsl:template>
                
</xsl:stylesheet>
