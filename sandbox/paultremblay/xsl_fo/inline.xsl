<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:attribute-set name="emphasis" >
	<xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="strong" >
	<xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>


    <xsl:template match="strong">
        <fo:inline xsl:use-attribute-sets="strong">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="emphasis">
        <fo:inline xsl:use-attribute-sets="emphasis">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>
    
</xsl:stylesheet>


