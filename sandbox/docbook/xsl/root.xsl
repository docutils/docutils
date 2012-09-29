<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template match="document">
        <xsl:choose >
            <xsl:when test="$docbook-type = 'book'">
                <d:book>
                    <xsl:call-template name="make-info"/>
                    <xsl:variable name="section-exists">
                        <xsl:for-each select="/">
                            <xsl:if test="descendant::section">true</xsl:if>
                        </xsl:for-each>
                    </xsl:variable>
                    <xsl:choose>
                        <xsl:when test="$section-exists != 'true'">
                            <xsl:call-template name = "make-section"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:apply-templates/>
                        </xsl:otherwise>
                    </xsl:choose>
                    <xsl:call-template name="make-appendix"/>
                </d:book>
            </xsl:when>
            <xsl:otherwise>
                <d:article>
                    <xsl:call-template name="make-info"/>
                    <xsl:apply-templates/>
                    <xsl:call-template name="make-appendix"/>
                </d:article>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
</xsl:stylesheet>
