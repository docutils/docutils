<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <xsl:import href = "../docutils_to_fo.xsl"/>

    <xsl:template match = "document">
        <xsl:call-template name='test-params'/>
        <fo:page-sequence master-reference="pages" format="i" initial-page-number="1">
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates select="topic[@classes='contents']" mode="toc"/>
            </fo:flow>
        </fo:page-sequence>
        <fo:page-sequence master-reference="pages" xsl:use-attribute-sets="body-page-sequence">
            <xsl:call-template name="make-footnote-separator"/>
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates/>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>


    <xsl:template name="make-pages">
        <fo:layout-master-set>
            <fo:simple-page-master master-name="abstract-page"  xsl:use-attribute-sets="page-size">
                <fo:region-body margin-bottom="1in"/>
                <fo:region-after region-name = "abstract-footer" extent = "1in"/>
            </fo:simple-page-master>
            <fo:simple-page-master master-name="dedication-page" xsl:use-attribute-sets="page-size">
                <fo:region-body/>
            </fo:simple-page-master>
            <fo:simple-page-master master-name="first-page" xsl:use-attribute-sets="page-size">
                <fo:region-body/>
            </fo:simple-page-master>
            <fo:simple-page-master master-name="even-page" xsl:use-attribute-sets="page-size">
                <fo:region-body margin-top="1in" margin-bottom="1in"/>
                <fo:region-before region-name="even-header" extent="1in"/>
                <fo:region-after region-name="even-footer" extent="1in"/>
            </fo:simple-page-master>
            <fo:simple-page-master master-name="odd-page" xsl:use-attribute-sets="page-size">
                <fo:region-body  margin-bottom="1in" margin-top="2in"/>
                <fo:region-before region-name="odd-header" extent="2in"/>
                <fo:region-after region-name="odd-footer" extent="1in"/>
            </fo:simple-page-master>

            <fo:page-sequence-master master-name="abstract-pages">
                <fo:repeatable-page-master-reference master-reference="abstract-page"/>
            </fo:page-sequence-master>
            <fo:page-sequence-master master-name="dedication-pages">
                <fo:repeatable-page-master-reference master-reference="dedication-page"/>
            </fo:page-sequence-master>
            <fo:page-sequence-master master-name="toc-pages">
                <fo:repeatable-page-master-alternatives>
                    <fo:conditional-page-master-reference master-reference = "even-page" odd-or-even = "even"/>
                    <fo:conditional-page-master-reference master-reference = "odd-page" odd-or-even = "odd"/>
                </fo:repeatable-page-master-alternatives>
            </fo:page-sequence-master>
            <fo:page-sequence-master master-name="body-pages">
                <fo:repeatable-page-master-alternatives>
                    <fo:conditional-page-master-reference master-reference = "even-page" odd-or-even = "even"/>
                    <fo:conditional-page-master-reference master-reference = "odd-page" odd-or-even = "odd"/>
                    <fo:conditional-page-master-reference master-reference = "first" page-position = "first"/>
                </fo:repeatable-page-master-alternatives>
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
