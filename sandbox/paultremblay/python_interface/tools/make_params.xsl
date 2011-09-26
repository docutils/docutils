<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:output method="text"/>
    <xsl:template match="xsl:stylesheet">
        <xsl:text>param_dict = {&#xA;</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>}</xsl:text>
    </xsl:template>


    <xsl:template match="xsl:param">
        <xsl:text>'</xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:text>' : '</xsl:text>
        <xsl:value-of select="."/>
        <xsl:text>',</xsl:text>
        <xsl:text>&#xA;</xsl:text>
    </xsl:template>

    <xsl:template match="@*|node()"/>

</xsl:stylesheet>
