<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:date="http://exslt.org/dates-and-times"
    version="1.1">

    <xsl:import href="date_format_date.xsl"/>

    <xsl:template match="@*|node()">
        <xsl:copy> 
            <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
    </xsl:template>


    <xsl:template match="comment()">
        <xsl:comment>
            <xsl:value-of select="."/>
        </xsl:comment>
    </xsl:template>

    <xsl:template match="entry/paragraph">
        <paragraph >
            <xsl:call-template name="format-cell"/>
        </paragraph>
    </xsl:template>

    <xsl:template name="format-date">
        <xsl:param name="string"/>
        <xsl:call-template name="date:format-date">
           <xsl:with-param name="date-time" select="$string" />
           <xsl:with-param name="pattern" select="'b/dd/yyyy'" />
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="format-as-dollar">
        <xsl:param name="string"/>
        <xsl:value-of select="format-number($string, '$###,###.00')"/> 
    </xsl:template>

    <xsl:template name="format-as-number">
        <xsl:param name="string"/>
        <xsl:value-of select="format-number($string, '###,###')"/> 
    </xsl:template>

    <xsl:template name="format-as-integer">
        <xsl:param name="string"/>
        <xsl:variable name="rounded-num" select="round($string)"/>
        <xsl:value-of select="format-number($rounded-num, '###,###')"/> 
    </xsl:template>

    <xsl:template name="format-cell">
        <xsl:variable name="cell-num">
            <xsl:for-each select="..">
                <xsl:number/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="row-num">
            <xsl:for-each select="../..">
                <xsl:number/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="class">
            <xsl:value-of select="ancestor::table/@classes"/>
        </xsl:variable>
        <xsl:choose >
            <xsl:when test="$class = 'date'">
                <xsl:choose >
                    <xsl:when test="$cell-num=1 and ancestor::tbody">
                        <xsl:call-template name="format-date">
                            <xsl:with-param name="string" select="."/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="$cell-num=3 and ancestor::tbody">
                        <xsl:call-template name="format-as-dollar">
                            <xsl:with-param name="string" select="."/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:otherwise >
                        <xsl:value-of select="."/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:when test="$class = 'long-metrics'">
                <xsl:choose >
                    <xsl:when test="$cell-num=3 and ancestor::tbody">
                        <xsl:call-template name="format-as-integer">
                            <xsl:with-param name="string" select="."/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="$cell-num=4 and ancestor::tbody">
                        <xsl:call-template name="format-as-integer">
                            <xsl:with-param name="string" select="."/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="$cell-num=5 and ancestor::tbody">
                        <xsl:variable name="cell3">
                            <xsl:for-each select="..">
                                <xsl:value-of select="preceding-sibling::entry[2]"/>
                            </xsl:for-each>
                        </xsl:variable>
                        <xsl:variable name="cell4">
                            <xsl:for-each select="..">
                                <xsl:value-of select="preceding-sibling::entry[1]"/>
                            </xsl:for-each>
                        </xsl:variable>
                        <xsl:variable name="rate">
                            <xsl:value-of select="$cell3 div $cell4"/>
                        </xsl:variable>
                        <!--
                        <xsl:value-of select="$rate"/>
                        -->
                        <xsl:call-template name="format-as-number">
                            <xsl:with-param name="string" select="$rate"/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="$cell-num=6 and ancestor::tbody">
                        <xsl:call-template name="format-as-integer">
                            <xsl:with-param name="string" select="."/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:when test="$cell-num=7 and ancestor::tbody">
                        <xsl:variable name="cell6">
                            <xsl:for-each select="..">
                                <xsl:value-of select="preceding-sibling::entry[1]"/>
                            </xsl:for-each>
                        </xsl:variable>
                        <xsl:variable name="cell4">
                            <xsl:for-each select="..">
                                <xsl:value-of select="preceding-sibling::entry[3]"/>
                            </xsl:for-each>
                        </xsl:variable>
                        <xsl:variable name="diff">
                            <xsl:value-of select="$cell4 - $cell6"/> 
                        </xsl:variable>
                        <xsl:call-template name="format-as-integer">
                            <xsl:with-param name="string" select="$diff"/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:otherwise >
                        <xsl:value-of select="."/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:when test="$class = 'costs'">
                <xsl:choose >
                    <xsl:when test="$cell-num=2 and ancestor::tbody">
                        <xsl:call-template name="format-as-dollar">
                            <xsl:with-param name="string" select="."/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:otherwise >
                        <xsl:value-of select="."/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:otherwise >
                <xsl:message >
                    <xsl:text >No match for "</xsl:text>
                    <xsl:value-of select="$class"/>
                    <xsl:text >" in stylesheet format_numbers.xsl</xsl:text>
                </xsl:message>
                <xsl:apply-templates/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
</xsl:stylesheet>
