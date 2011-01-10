<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Date: 2011-01-09 02:51:33 -0500 (Sun, 09 Jan 2011) $ -->
    <!--
    The stylesheet for handling root elements, such as document
    -->

    <!--attributes for page numbering of the main body-->
    <xsl:attribute-set name="page-format-body">
        <xsl:attribute name= "format">1</xsl:attribute> 
        <xsl:attribute name= "initial-page-number">auto-odd</xsl:attribute> 
    </xsl:attribute-set>

    <!--attributes for the sequence of pages for the main body. So far, it 
    only uses the page-format-body attriutes -->
    <xsl:attribute-set name="page-sequence-body" use-attribute-sets="page-format-body">
    </xsl:attribute-set>

    <xsl:template match = "document">
        <xsl:choose>
            <!--If the output does not specify chapters, then use the same pages sequence
            for all but the TOC (and possibly other front elements-->
            <xsl:when test="$create-chapters = '' or not(section)">
                <fo:page-sequence master-reference="pages" xsl:use-attribute-sets="page-sequence-body">
                    <!--Set up the footers and headers-->
                    <xsl:apply-templates select="/document/decoration/header" mode="header"/>
                    <xsl:apply-templates select="/document/decoration/footer" mode="footer"/>
                    <fo:flow flow-name="xsl-region-body">
                        <xsl:apply-templates/>
                    </fo:flow>
                </fo:page-sequence>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

</xsl:stylesheet>
