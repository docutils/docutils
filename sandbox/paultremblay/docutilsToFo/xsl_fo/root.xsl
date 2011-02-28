<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <!-- The stylesheet for handling root elements, such as document-->

    <!--any attributes here will apply to the entire document-->
    <xsl:attribute-set name="default-page-sequence">
    </xsl:attribute-set>

    <xsl:attribute-set name="front-page-sequence" use-attribute-sets="default-page-sequence">
        <xsl:attribute name= "force-page-count">no-force</xsl:attribute> 
    </xsl:attribute-set>

    <!--attributes for the sequence of pages for the toc. 
    NOTE: The page numbering might have to be changed, by the user, if an odd-even layout is used. 
    Namely, the user might have to change the the force-page-count to odd or even, depending.  -->
    <xsl:attribute-set name="toc-page-sequence" use-attribute-sets="default-page-sequence">
        <xsl:attribute name= "format">i</xsl:attribute> 
        <xsl:attribute name= "initial-page-number">1</xsl:attribute> 
        <xsl:attribute name= "force-page-count">no-force</xsl:attribute> 
    </xsl:attribute-set>


    <!--attributes for the sequence of pages for the main body. -->
    <xsl:attribute-set name="body-page-sequence" use-attribute-sets="default-page-sequence">
        <xsl:attribute name= "format">1</xsl:attribute> 
        <xsl:attribute name= "initial-page-number">1</xsl:attribute> 
    </xsl:attribute-set>

    <!--default for fo:flow-->
    <xsl:attribute-set name="default-flow">
    </xsl:attribute-set>


    <xsl:attribute-set name="front-flow" use-attribute-sets="default-flow">
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-flow" use-attribute-sets="default-flow">
    </xsl:attribute-set>

    <xsl:attribute-set name="body-flow" use-attribute-sets="default-flow">
    </xsl:attribute-set>

    <xsl:attribute-set name="footnote-separator-flow">
    </xsl:attribute-set>

    <xsl:attribute-set name="footnote-separator-block">
        <!--putting space before can create way too much space-->
        <!--
        <xsl:attribute name="space-before">5mm</xsl:attribute>
        <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
        -->
    </xsl:attribute-set>

    <xsl:template match="/">
        <xsl:element name="fo:root">
            <xsl:call-template name="make-pages"/>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <!--title, bibliographic-info dedication, abstract toc-->
    <xsl:template match = "document">
        <xsl:call-template name='test-params'/>
        <xsl:if test="$test='True'">
            <xsl:message>
                <xsl:text>value of $front-order = "</xsl:text>
                <xsl:value-of select="$front-order"/>
                <xsl:text>"</xsl:text>
            </xsl:message>
        </xsl:if>
        <xsl:choose>
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
            <xsl:apply-templates select="/document/container[@classes='body-header']" mode="header"/>
            <xsl:apply-templates select="/document/container[@classes='first-header']" mode="header"/>
            <xsl:apply-templates select="/document/container[@classes='odd-header']" mode="header"/>
            <xsl:apply-templates select="/document/container[@classes='even-header']" mode="header"/>
            <xsl:apply-templates select="/document/decoration/footer" mode="footer"/>
            <xsl:apply-templates select="/document/container[@classes='body-footer']" mode="footer"/>
            <xsl:apply-templates select="/document/container[@classes='first-footer']" mode="footer"/>
            <xsl:apply-templates select="/document/container[@classes='odd-footer']" mode="footer"/>
            <xsl:apply-templates select="/document/container[@classes='even-footer']" mode="footer"/>
            <fo:static-content role = "footnote-separator" 
                xsl:use-attribute-sets = "footnote-separator-flow" flow-name="xsl-footnote-separator">
               <fo:block xsl:use-attribute-sets = "footnote-separator-block">
                    <fo:leader leader-pattern="rule" leader-length="100%"/>
               </fo:block>
            </fo:static-content>
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates/>
                <!--write an empty block in case there is no content. A hack which I will have to fix later-->
                <fo:block/>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>

    <xsl:template name="make-front">
        <fo:page-sequence master-reference="front-matter-pages" xsl:use-attribute-sets="front-page-sequence">
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="front-flow">
                <xsl:call-template name="apply-in-order">
                    <xsl:with-param name="order" select="$front-order"/>
                    <xsl:with-param name="region" select="'with-front'"/>
                </xsl:call-template>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>

    <xsl:template name="make-toc">
        <fo:page-sequence master-reference="toc-pages" xsl:use-attribute-sets="toc-page-sequence">
            <xsl:apply-templates select="/document/decoration/header" mode="header"/>
            <xsl:apply-templates select="/document/container[@classes='toc-body-header']" mode="header"/>
            <xsl:apply-templates select="/document/container[@classes='toc-first-header']" mode="header"/>
            <xsl:apply-templates select="/document/container[@classes='toc-odd-header']" mode="header"/>
            <xsl:apply-templates select="/document/container[@classes='toc-even-header']" mode="header"/>
            <xsl:apply-templates select="/document/decoration/footer" mode="footer"/>
            <xsl:apply-templates select="/document/container[@classes='toc-body-footer']" mode="footer"/>
            <xsl:apply-templates select="/document/container[@classes='toc-first-footer']" mode="footer"/>
            <xsl:apply-templates select="/document/container[@classes='toc-odd-footer']" mode="footer"/>
            <xsl:apply-templates select="/document/container[@classes='toc-even-footer']" mode="footer"/>
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="toc-flow">
                <xsl:call-template name="apply-in-order">
                    <xsl:with-param name="order" select="$front-order"/>
                    <xsl:with-param name="region" select="'with-toc'"/>
                </xsl:call-template>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>


    <xsl:template name="apply-in-order">
        <xsl:param name="order"/>
        <xsl:param name="first-run">True</xsl:param>
        <xsl:param name="region"/>
        <xsl:variable name="order-string">
            <xsl:choose>
                <xsl:when test="$first-run='True'">
                    <xsl:value-of select="concat($order, ',')"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$order"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="matter" select="normalize-space(substring-before($order-string, ','))"/>
        <xsl:if test = "$matter != ''">
            <xsl:if test="$title-pagination=$region and $matter='title'">
                <xsl:apply-templates select="/document/title" mode="front"/>
            </xsl:if>
            <xsl:if test="$bibliographic-pagination=$region and $matter = 'bibliographic'">
                <xsl:apply-templates select="docinfo" mode="front"/>
            </xsl:if>
            <xsl:if test="$dedication-pagination=$region and $matter = 'dedication'">
                <xsl:apply-templates select="topic[@classes='dedication']" mode="front"/>
            </xsl:if>
            <xsl:if test="$abstract-pagination=$region and $matter = 'abstract'">
                <xsl:apply-templates select="topic[@classes='abstract']" mode="front"/>
            </xsl:if>
            <xsl:if test="$toc-pagination=$region and $matter = 'toc'">
                <xsl:apply-templates select="topic[@classes='contents']" mode="front"/>
            </xsl:if>
            <xsl:call-template name="apply-in-order">
                <xsl:with-param name="first-run">False</xsl:with-param>
                <xsl:with-param name="order" select="substring-after($order-string, ',')"/>
                <xsl:with-param name="region" select="$region"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>



</xsl:stylesheet>
