<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">
    
    <xsl:template match="citation_reference">
        <d:citation>
            <xsl:apply-templates/>
        </d:citation>
    </xsl:template>

    <xsl:template match="citation">
        <xsl:call-template name="error-message-generic">
            <xsl:with-param name="msg">
                <xsl:text>citation not supported in any meaningful way in restructed text; skipping processing for this element.</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>


</xsl:stylesheet>
