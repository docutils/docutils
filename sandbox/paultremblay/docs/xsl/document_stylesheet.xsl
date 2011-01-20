<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <xsl:include href="utils.xsl"/>
    <xsl:include href="option_list.xsl"/>

    <xsl:output method="text"/>

    <xsl:template match="xsl:stylesheet">
            <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="xsl:attribute-set" priority="2">
        <xsl:message>
            <xsl:text>no match for "</xsl:text>
            <xsl:value-of select="@name"/>
            <xsl:text>"</xsl:text>
        </xsl:message>
    </xsl:template>

    <xsl:template match="@*|node()"/>

    
</xsl:stylesheet>
