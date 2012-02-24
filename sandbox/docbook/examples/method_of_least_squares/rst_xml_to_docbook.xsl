<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:import href="http://docutils.svn.sourceforge.net/viewvc/docutils/trunk/sandbox/docbook/xsl/docutils_to_docbook.xsl"/>

    <xsl:template name="address"/>

    <xsl:template match="address" mode="after-author">
        <d:address>
            <xsl:variable name="street" select="substring-before(., '&#xA;')"/>
            <xsl:variable name="rest" select="substring-after(., '&#xA;')"/>
            <xsl:variable name="street2" select="substring-before($rest, '&#xA;')"/>
            <xsl:variable name="city" select="substring-after($rest, '&#xA;')"/>
            <d:street>
                <xsl:value-of select="$street"/>
            </d:street>
            <d:street>
                <xsl:value-of select="$street2"/>
            </d:street>
            <d:city>
                <xsl:value-of select="$city"/>
            </d:city>
        </d:address>
    </xsl:template>



</xsl:stylesheet>
