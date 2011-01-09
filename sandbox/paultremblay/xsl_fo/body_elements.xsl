<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <xsl:template match = "document">
        <xsl:choose>
            <xsl:when test="$create-chapters = '' or not(section)">
                <!--No chapters and no TOC-->
                <fo:page-sequence master-reference="pages">
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

    <xsl:template match="decoration">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="decoration/header" mode="header">
        <xsl:choose>
            <xsl:when test="$page-layout = '' or $page-layout = 'simple'">
                <fo:static-content flow-name="odd-even-header">
                    <xsl:apply-templates/>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$page-layout = 'odd-even'">
                <fo:static-content flow-name="odd-header">
                    <xsl:apply-templates/>
                </fo:static-content>
                <fo:static-content flow-name="even-header">
                    <xsl:apply-templates/>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$page-layout = 'first'">
                <xsl:if test="$suppress-first-page-header != 'True'">
                    <fo:static-content flow-name = "first-header">
                        <xsl:apply-templates/>
                    </fo:static-content>
                </xsl:if>
                <fo:static-content flow-name = "odd-even-header">
                    <xsl:apply-templates/>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$page-layout = 'first-odd-even'">
                <xsl:if test="$suppress-first-page-header != 'True'">
                    <fo:static-content flow-name = "first-header">
                        <xsl:apply-templates/>
                    </fo:static-content>
                </xsl:if>
                <fo:static-content flow-name = "odd-header">
                    <xsl:apply-templates/>
                </fo:static-content>
                <fo:static-content flow-name = "even-header">
                    <xsl:apply-templates/>
                </fo:static-content>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="decoration/footer" mode="footer">
        <xsl:choose>
            <xsl:when test="$page-layout = '' or $page-layout = 'simple'">
                <fo:static-content flow-name="odd-even-footer">
                    <xsl:apply-templates/>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$page-layout = 'odd-even'">
                <fo:static-content flow-name="odd-footer">
                    <xsl:apply-templates/>
                </fo:static-content>
                <fo:static-content flow-name="even-footer">
                    <xsl:apply-templates/>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$page-layout = 'first'">
                <xsl:if test="$suppress-first-page-footer != 'True'">
                    <fo:static-content flow-name = "first-footer">
                        <xsl:apply-templates/>
                    </fo:static-content>
                </xsl:if>
                <fo:static-content flow-name = "odd-even-footer">
                    <xsl:apply-templates/>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$page-layout = 'first-odd-even'">
                <xsl:if test="$suppress-first-page-footer != 'True'">
                    <fo:static-content flow-name = "first-footer">
                        <xsl:apply-templates/>
                    </fo:static-content>
                </xsl:if>
                <fo:static-content flow-name = "odd-footer">
                    <xsl:apply-templates/>
                </fo:static-content>
                <fo:static-content flow-name = "even-footer">
                    <xsl:apply-templates/>
                </fo:static-content>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="inline[@classes='page-num']">
        <fo:page-number/>
    </xsl:template>

    <xsl:template match="paragraph">
        <xsl:call-template name="default-paragraph"/>
    </xsl:template>

    <xsl:template name="default-paragraph">
        <xsl:element name="fo:block">
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <xsl:template match="comment|decoration/header|decoration/footer"/>
    
</xsl:stylesheet>
