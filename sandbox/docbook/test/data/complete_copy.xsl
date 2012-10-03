<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.1"
>

    <xsl:output method="xml" encoding="UTF-8"/>


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

</xsl:stylesheet>

