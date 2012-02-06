<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    xmlns:ml="http://www.w3.org/1998/Math/MathML"
    version="1.1">

    <xsl:template match="math_block|math">
        <d:equation>
            <xsl:choose >
                <xsl:when test="ml:math">
                    <xsl:apply-templates/>
                </xsl:when>
                <xsl:otherwise >
                    <d:mathphrase>
                        <xsl:apply-templates/>
                    </d:mathphrase>
                </xsl:otherwise>
            </xsl:choose>
        </d:equation>
    </xsl:template>

    <xsl:template match="ml:math">
        <xsl:copy-of select="."/>
    </xsl:template>
    
</xsl:stylesheet>
