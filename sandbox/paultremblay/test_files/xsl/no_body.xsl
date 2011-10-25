<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <xsl:import href="../../xsl_fo/docutils_to_fo.xsl"/>
    
    <xsl:template match = "document">
        <xsl:call-template name='test-params'/>
        <fo:page-sequence master-reference="simple-page" xsl:use-attribute-sets="page-sequence">
            <xsl:apply-templates select="/document/decoration/header" mode="header"/>
            <xsl:apply-templates select="/document/decoration/footer" mode="footer"/>
            <xsl:call-template name="make-footnote-separator"/>
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates/>
                <fo:block/>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>
</xsl:stylesheet> 
