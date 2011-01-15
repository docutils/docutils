<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Date: 2011-01-09 02:51:33 -0500 (Sun, 09 Jan 2011) $ -->

    <!-- The stylesheet for handling root elements, such as document-->
    <!--
    NOTE: This will have to be changed, by the user, if an odd-even layout is used. 
    Namely, the user might have to change the the force-page-count to odd or even, depeding.
    -->
    <!--attributes for page numbering of the toc-->
    <xsl:attribute-set name="page-format-toc">
        <xsl:attribute name= "format">i</xsl:attribute> 
        <xsl:attribute name= "initial-page-number">auto</xsl:attribute> 
        <xsl:attribute name= "force-page-count">no-force</xsl:attribute> 
    </xsl:attribute-set>

    <!--attributes for page numbering of the main body-->
    <xsl:attribute-set name="page-format-body">
        <xsl:attribute name= "format">1</xsl:attribute> 
        <xsl:attribute name= "initial-page-number">1</xsl:attribute> 
    </xsl:attribute-set>


    <!--attributes for the sequence of pages for the main body. So far, it 
    only uses the page-format-body attriutes 
    
    -->
    <xsl:attribute-set name="page-sequence-body" use-attribute-sets="page-format-body">
    </xsl:attribute-set>

    <!--attributes for the sequence of pages for the toc. So far, it 
    only uses the page-format-body attriutes -->
    <xsl:attribute-set name="page-sequence-toc" use-attribute-sets="page-format-toc">
    </xsl:attribute-set>

    <xsl:template match = "document">
        <xsl:if test="topic[@classes='contents']">
            <fo:page-sequence master-reference="toc-pages" xsl:use-attribute-sets="page-sequence-toc">
                <!--Set up the footers and headers-->
                <xsl:apply-templates select="/document/decoration/header" mode="header"/>
                <xsl:apply-templates select="/document/decoration/footer" mode="footer"/>
                <fo:flow flow-name="xsl-region-body">
                    <xsl:apply-templates select="topic[@classes='contents']" mode="toc"/>
                </fo:flow>
            </fo:page-sequence>
        </xsl:if>
        <fo:page-sequence master-reference="pages" xsl:use-attribute-sets="page-sequence-body">
            <!--Set up the footers and headers-->
            <xsl:apply-templates select="/document/decoration/header" mode="header"/>
            <xsl:apply-templates select="/document/decoration/footer" mode="footer"/>
            <fo:flow flow-name="xsl-region-body">
                <xsl:apply-templates/>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>


    <xsl:template match="topic[@classes='contents']"/>

     <xsl:template match="topic[@classes='contents']" mode="toc">
         <xsl:apply-templates/>
     </xsl:template>

</xsl:stylesheet>
