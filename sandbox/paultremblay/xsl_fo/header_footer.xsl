<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Date: 2011-01-09 02:51:33 -0500 (Sun, 09 Jan 2011) $ -->
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


    
</xsl:stylesheet>
