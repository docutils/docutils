<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template name="make-appendix">
        <xsl:if test="section[@classes='appendix']">
            <d:appendix>
                <d:title>
                    <xsl:value-of select="$Appendix.title"/>
                </d:title>
                <xsl:apply-templates select="section[@classes='appendix']"
                    mode="appendix"/>
            </d:appendix>
        </xsl:if>
    </xsl:template>

    <xsl:template match="section[@classes='appendix']" />

    <xsl:template match="section[@classes='appendix']" mode="appendix">
        <xsl:call-template name="make-section"/>
    </xsl:template>
    
</xsl:stylesheet>
