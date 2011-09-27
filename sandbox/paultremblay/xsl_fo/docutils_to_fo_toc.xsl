<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <xsl:import href = "docutils_to_fo.xsl"/>

    <xsl:template match = "document">
        <xsl:call-template name='test-params'/>
        <fo:page-sequence master-reference="pages" format="i" initial-page-number="1">
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates select="topic[@classes='contents']" mode="toc"/>
            </fo:flow>
        </fo:page-sequence>
        <fo:page-sequence master-reference="pages" xsl:use-attribute-sets="body-page-sequence">
            <xsl:apply-templates select="/document/decoration/header" mode="header"/>
            <xsl:apply-templates select="/document/decoration/footer" mode="footer"/>
            <xsl:call-template name="make-footnote-separator"/>
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates/>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>


    <xsl:template name="make-pages">
         <fo:layout-master-set>
          <fo:simple-page-master master-name="simple-page"  xsl:use-attribute-sets="page-size">
            <fo:region-body xsl:use-attribute-sets="region-body"/>
            <!--put footers or headers here, if needed-->
          </fo:simple-page-master>
              <fo:page-sequence-master master-name="pages">
                    <fo:repeatable-page-master-reference master-reference="simple-page"/>
              </fo:page-sequence-master>
         </fo:layout-master-set>
    </xsl:template>

    <xsl:template match="topic[@classes='contents']" mode="toc">
        <fo:block role="toc" xsl:use-attribute-sets="toc-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="topic[@classes='contents']"/>

</xsl:stylesheet>
