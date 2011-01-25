<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:attribute-set name="emphasis-inline" >
	<xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="strong-inline" >
	<xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for reference elements with refuri attriute. Element is fo:basic-link. `docutils`-->
    <xsl:attribute-set name="basic-link-inline" >
	<xsl:attribute name="text-decoration">underline</xsl:attribute>
        <xsl:attribute name="color">blue</xsl:attribute>
    </xsl:attribute-set>


    <xsl:template match="strong">
        <fo:inline xsl:use-attribute-sets="strong-inline">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="emphasis">
        <fo:inline xsl:use-attribute-sets="emphasis-inline">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <!--this template creates clickable links; you may want to give the option to turn this off-->
    <xsl:template match= "reference[@refuri]">
        <fo:basic-link xsl:use-attribute-sets="basic-link-inline" external-destination="url('{@refuri}')"> 
            <xsl:apply-templates/>
        </fo:basic-link>
    </xsl:template> 

    
</xsl:stylesheet>


