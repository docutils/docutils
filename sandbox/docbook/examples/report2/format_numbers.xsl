<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.1">

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
            <xsl:when test="$class = 'long-metrics'">
                <xsl:choose >
                    <xsl:when test="$cell-num=3 and ancestor::tbody">
                        <xsl:value-of select="format-number(., '###,###')"/> 
                    </xsl:when>
                    <xsl:when test="$cell-num=4 and ancestor::tbody">
                        <xsl:value-of select="format-number(round(.), '###,###')"/> 
                    </xsl:when>
                    <xsl:when test="$cell-num=5 and ancestor::tbody">
                        <xsl:variable name="cell3">
                            <xsl:for-each select="..">
                                <xsl:value-of select="preceding-sibling::entry[2]"/>
                            </xsl:for-each>
                        </xsl:variable>
                        <xsl:variable name="cell4">
                            <xsl:for-each select="..">
                                <xsl:value-of select="round(preceding-sibling::entry[1])"/>
                            </xsl:for-each>
                        </xsl:variable>
                        <xsl:variable name="rate">
                            <xsl:value-of select="$cell3 div $cell4"/>
                        </xsl:variable>
                        <xsl:value-of select="format-number($rate, '###,###.00')"/> 
                    </xsl:when>
                    <xsl:when test="$cell-num=6 and ancestor::tbody">
                        <xsl:value-of select="format-number(round(.), '###,###')"/> 
                    </xsl:when>
                    <xsl:when test="$cell-num=7 and ancestor::tbody">
                        <xsl:variable name="cell6">
                            <xsl:for-each select="..">
                                <xsl:value-of select="round(preceding-sibling::entry[1])"/>
                            </xsl:for-each>
                        </xsl:variable>
                        <xsl:variable name="cell4">
                            <xsl:for-each select="..">
                                <xsl:value-of select="round(preceding-sibling::entry[3])"/>
                            </xsl:for-each>
                        </xsl:variable>
                        <xsl:value-of select="$cell4 - $cell6"/> 
                    </xsl:when>
                    <xsl:otherwise >
                        <xsl:value-of select="."/>
                    </xsl:otherwise>
                </xsl:choose>
                </xsl:when>
                <xsl:when test="$class = 'projected-costs'">
                    <xsl:choose >
                        <xsl:when test="$cell-num=2 and ancestor::tbody">
                            <xsl:value-of select="format-number(., '###,###.00')"/> 
                        </xsl:when>
                        <xsl:when test="$cell-num=3 and ancestor::tbody">
                            <xsl:value-of select="format-number(., '###,###.00')"/> 
                        </xsl:when>
                        <xsl:when test="$cell-num=4 and ancestor::tbody">
                            <xsl:text >$</xsl:text>
                            <xsl:value-of select="format-number(., '###,##0.00')"/> 
                        </xsl:when>
                        <xsl:when test="$cell-num=5 and ancestor::tbody">
                            <xsl:text >$</xsl:text>
                            <xsl:value-of select="format-number(., '###,##0.00')"/> 
                        </xsl:when>
                        <xsl:otherwise >
                            <xsl:value-of select="."/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:when>
                <xsl:when test="$class = 'projected-exceptions'">
                    <xsl:choose >
                        <xsl:when test="$cell-num=2 and ancestor::tbody">
                            <xsl:value-of select="format-number(., '###,###')"/> 
                        </xsl:when>
                        <xsl:when test="$cell-num=3 and ancestor::tbody">
                            <xsl:value-of select="format-number(., '###,###')"/> 
                        </xsl:when>
                        <xsl:otherwise >
                            <xsl:value-of select="."/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:when>
                <xsl:when test="$class = 'projected-summary'">
                    <xsl:choose >
                        <xsl:when test="$cell-num=2 and ancestor::tbody">
                            <xsl:value-of select="format-number(.,
                                '$###,###.00')"/> 
                        </xsl:when>
                        <xsl:when test="$cell-num=3 and ancestor::tbody">
                            <xsl:value-of select="format-number(.,
                                '$###,###.00')"/> 
                        </xsl:when>
                        <xsl:when test="$cell-num=4 and ancestor::tbody">
                            <xsl:value-of select="format-number(.,
                                '$###,###.00')"/> 
                        </xsl:when>
                        <xsl:otherwise >
                            <xsl:value-of select="."/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:when>
                <xsl:when test="$class = 'receive-hours'">
                    <xsl:choose >
                        <xsl:when test="$cell-num != 1 and ancestor::tbody">
                            <xsl:value-of select="format-number(.,
                                '###,###.00')"/> 
                        </xsl:when>
                        <xsl:otherwise >
                            <xsl:value-of select="."/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:when>
                <xsl:when test="$class = 'cutover-costs'">
                    <xsl:choose >
                        <xsl:when test="$cell-num != 1 and ancestor::tbody">
                            <xsl:value-of select="format-number(., '###,###')"/> 
                        </xsl:when>
                        <xsl:otherwise >
                            <xsl:value-of select="."/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:when>
                <xsl:when test="$class = 'overall-costs'">
                    <xsl:choose >
                        <xsl:when test="$cell-num != 1 and ancestor::tbody">
                            <xsl:text >$</xsl:text>
                            <xsl:value-of select="format-number(., '###,###.00')"/> 
                        </xsl:when>
                        <xsl:otherwise >
                            <xsl:value-of select="."/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:when>
                <xsl:when test="$class = 'hardware-costs'">
                    <xsl:choose >
                        <xsl:when test="$cell-num != 1 and ancestor::tbody">
                            <xsl:text >$</xsl:text>
                            <xsl:value-of select="format-number(., '###,###.00')"/> 
                        </xsl:when>
                        <xsl:otherwise >
                            <xsl:value-of select="."/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:when>
                <xsl:when test="$class = 'problem-solving'">
                    <xsl:choose >
                        <xsl:when test="$cell-num &gt; 2 and ancestor::tbody">
                            <xsl:value-of select="format-number(., '###,###.00')"/> 
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
                    <xsl:text >"</xsl:text>
                </xsl:message>
                <xsl:apply-templates/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
</xsl:stylesheet>
