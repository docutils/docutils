<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <xsl:import href = "../docutils_to_fo.xsl"/>

    <xsl:template name="make-pages">
        <fo:layout-master-set>
            <fo:simple-page-master master-name="abstract-page"  xsl:use-attribute-sets="page-size">
                <fo:region-body margin-bottom="1in"/>
                <fo:region-after region-name = "odd-footer" extent = "1in"/>
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

    <xsl:template match = "document">
        <xsl:call-template name='test-params'/>
        <fo:page-sequence master-reference="dedication-page" format="i" initial-page-number="1" force-page-count="no-force">
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates select="/document/docinfo" mode="docinfo"/>
            </fo:flow>
        </fo:page-sequence>
        <fo:page-sequence master-reference="dedication-page" format="i" initial-page-number="1" force-page-count="no-force">
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates select="topic[@classes='dedication']" mode="dedication"/>
            </fo:flow>
        </fo:page-sequence>
        <fo:page-sequence master-reference="abstract-page" format="i" initial-page-number="1" force-page-count="no-force">
            <fo:static-content flow-name = "odd-footer">
                <fo:block><fo:page-number/></fo:block>
            </fo:static-content>
            <fo:static-content flow-name = "even-footer">
                <fo:block><fo:page-number/></fo:block>
            </fo:static-content>
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates select="topic[@classes='abstract']" mode="abstract"/>
            </fo:flow>
        </fo:page-sequence>
        <fo:page-sequence master-reference="toc-pages" format="i" initial-page-number="1" force-page-count="no-force">
            <fo:static-content flow-name = "odd-footer">
                <fo:block><fo:page-number/></fo:block>
            </fo:static-content>
            <fo:static-content flow-name = "even-footer">
                <fo:block><fo:page-number/></fo:block>
            </fo:static-content>
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates select="topic[@classes='contents']" mode="toc"/>
            </fo:flow>
        </fo:page-sequence>
        <fo:page-sequence master-reference="body-pages" >
            <xsl:call-template name="make-footnote-separator"/>
            <fo:static-content flow-name="odd-header">
                <fo:block><fo:page-number/></fo:block>
            </fo:static-content>
            <fo:static-content flow-name="even-header">
                <fo:block><fo:page-number/></fo:block>
            </fo:static-content>
            <fo:flow flow-name="xsl-region-body" >
                <xsl:apply-templates/>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>



    <xsl:template match="topic[@classes='contents']" mode="toc">
        <fo:block role="toc" xsl:use-attribute-sets="toc-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="topic[@classes='abstract']" mode="abstract">
        <fo:block role="toc" xsl:use-attribute-sets="toc-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="topic[@classes='dedication']" mode="dedication">
        <fo:block role="toc" xsl:use-attribute-sets="toc-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="/document/docinfo" mode="docinfo">
        <fo:list-block role="field-list" 
            xsl:use-attribute-sets="bibliographic-fields-list-block">
            <xsl:apply-templates mode="list"/>
        </fo:list-block>
    </xsl:template>

    <xsl:template match="topic[@classes='contents']|topic[@classes='dedication']|topic[@classes='abstract']|/document/docinfo"/>

</xsl:stylesheet>
