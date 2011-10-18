<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id: citation.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->

    <!--Since there is no accepted way to render the limited citation 
    element, I am simply using it like a substition-->

    <xsl:key name="citation" match="citation" use="@ids"/>

    <xsl:template match= "citation_reference">
        <xsl:apply-templates select="key('citation', @refid)" mode="citation"/>
    </xsl:template>

    <xsl:template match="citation" mode="citation">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="citation/label"/>

    <xsl:template match="citation/paragraph">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="citation"/>
    
</xsl:stylesheet>

