<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template match="document">
        <xsl:choose >
            <xsl:when test="$docbook-type = 'book'">
                <d:book>
                    <xsl:call-template name="make-info"/>
                    <xsl:apply-templates/>
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
