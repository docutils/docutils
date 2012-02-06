<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template match="section[not(@classes = 'appendix')]">
        <xsl:element name="d:section">
            <xsl:attribute name="xml:id">
                <xsl:choose >
                    <xsl:when test="@names">
                        <xsl:if test="not(contains(@ids, @names))">
                            <xsl:message terminate="yes">This shouldn't happen</xsl:message>
                        </xsl:if>
                        <xsl:value-of select="@names"/>
                    </xsl:when>
                    <xsl:otherwise >
                        <xsl:value-of select="@ids"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:attribute>
            <xsl:if test="@classes">
                <xsl:attribute name="role">
                    <xsl:value-of select="@classes"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <xsl:template match="section/title">
        <d:title>
            <xsl:apply-templates/>
        </d:title>
    </xsl:template>

</xsl:stylesheet>
