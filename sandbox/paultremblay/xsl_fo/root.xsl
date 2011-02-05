<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <!-- The stylesheet for handling root elements, such as document-->


    <xsl:attribute-set name="front-page-sequence">
        <xsl:attribute name= "force-page-count">no-force</xsl:attribute> 
    </xsl:attribute-set>

    <!--attributes for the sequence of pages for the toc. 
    NOTE: The page numbering might have to be changed, by the user, if an odd-even layout is used. 
    Namely, the user might have to change the the force-page-count to odd or even, depending.  -->
    <xsl:attribute-set name="toc-page-sequence">
        <xsl:attribute name= "format">i</xsl:attribute> 
        <xsl:attribute name= "initial-page-number">1</xsl:attribute> 
        <xsl:attribute name= "force-page-count">no-force</xsl:attribute> 
    </xsl:attribute-set>


    <!--attributes for the sequence of pages for the main body. -->
    <xsl:attribute-set name="body-page-sequence" >
        <xsl:attribute name= "format">1</xsl:attribute> 
        <xsl:attribute name= "initial-page-number">1</xsl:attribute> 
    </xsl:attribute-set>

    <!--default for fo:flow-->
    <!--NOTE: note of the flows yet implemented, but when implemented, could be 
    used to set font-size, background, line-height, etc.-->
    <xsl:attribute-set name="default-flow">
    </xsl:attribute-set>


    <xsl:attribute-set name="front-flow" use-attribute-sets="default-flow">
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-flow" use-attribute-sets="default-flow">
    </xsl:attribute-set>

    <xsl:attribute-set name="body-flow" use-attribute-sets="default-flow">
    </xsl:attribute-set>

    <xsl:template match="/">
        <xsl:element name="fo:root">
            <xsl:call-template name="make-pages">
                <xsl:with-param name="page-layout" select="$page-layout"/>
            </xsl:call-template>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <xsl:template name="make-front">
        <fo:page-sequence master-reference="front-matter-pages" xsl:use-attribute-sets="front-page-sequence">
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="front-flow">
                <xsl:if test="$title-pagination='with-front'">
                    <xsl:apply-templates select="/document/title" mode="front"/>
                </xsl:if>
                <xsl:if test="$bibliographic-pagination='with-front'">
                    <xsl:apply-templates select="docinfo" mode="front"/>
                </xsl:if>
                <xsl:if test="$dedication-pagination='with-front'">
                    <xsl:apply-templates select="topic[@classes='dedication']" mode="front"/>
                </xsl:if>
                <xsl:if test="$abstract-pagination='with-front'">
                    <xsl:apply-templates select="topic[@classes='abstract']" mode="front"/>
                </xsl:if>
                <xsl:if test="$toc-pagination='with-front'">
                    <xsl:apply-templates select="topic[@classes='contents']" mode="front"/>
                </xsl:if>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>

    <xsl:template name="make-toc">
        <fo:page-sequence master-reference="toc-pages" xsl:use-attribute-sets="toc-page-sequence">
            <xsl:apply-templates select="/document/decoration/header" mode="header"/>
            <xsl:apply-templates select="/document/decoration/footer" mode="footer"/>
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="toc-flow">
                <xsl:if test="$title-pagination='with-toc'">
                    <xsl:apply-templates select="/document/title" mode="front"/>
                </xsl:if>
                <xsl:if test="$bibliographic-pagination='with-toc'">
                    <xsl:apply-templates select="docinfo" mode="front"/>
                </xsl:if>
                <xsl:if test="$dedication-pagination='with-toc'">
                    <xsl:apply-templates select="topic[@classes='dedication']" mode="front"/>
                </xsl:if>
                <xsl:if test="$abstract-pagination='with-toc'">
                    <xsl:apply-templates select="topic[@classes='abstract']" mode="front"/>
                </xsl:if>
                <xsl:if test="$toc-pagination='with-toc'">
                    <xsl:apply-templates select="topic[@classes='contents']" mode="front"/>
                </xsl:if>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>

    <!--title, bibliographic-info dedication, abstract toc-->
    <xsl:template match = "document">
        <xsl:call-template name='test-params'/>
        <xsl:choose>
            <!--won't match anything-->
            <xsl:when test="$page-sequence-type = 'front-toc-body'">
                <xsl:call-template name="make-front"/>
                <xsl:call-template name="make-toc"/>
            </xsl:when>
            <xsl:when test="$page-sequence-type = 'front-body'">
                <xsl:call-template name="make-front"/>
            </xsl:when>
            <xsl:when test="$page-sequence-type = 'toc-body'">
                <xsl:call-template name="make-toc"/>
            </xsl:when>
        </xsl:choose>
        <fo:page-sequence master-reference="pages" xsl:use-attribute-sets="body-page-sequence">
            <xsl:apply-templates select="/document/decoration/header" mode="header"/>
            <xsl:apply-templates select="/document/decoration/footer" mode="footer"/>
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates/>
                <!--write an empty block in case there is no content. A hack which I will have to fix later-->
                <fo:block/>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>



</xsl:stylesheet>
