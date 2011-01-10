<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Date: 2011-01-09 02:51:33 -0500 (Sun, 09 Jan 2011) $ -->
    <!--
    This stylesheet handles headers and footers. It creates the fo:static-content 
    elements, and the child fo:block elements. Each paragraph up to three has its 
    own attriute set.
    -->

    <xsl:attribute-set name="header-first-paragraph">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="header-second-paragraph">
        <xsl:attribute name="space-before">5pt</xsl:attribute>
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="header-third-paragraph">
        <xsl:attribute name="space-before">5pt</xsl:attribute>
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="footer-first-paragraph">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="footer-second-paragraph">
        <xsl:attribute name="space-before">5pt</xsl:attribute>
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="footer-third-paragraph">
        <xsl:attribute name="space-before">5pt</xsl:attribute>
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

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

    <xsl:template match="decoration/header/paragraph[1]">
        <fo:block xsl:use-attribute-sets="header-first-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/header/paragraph[2]">
        <fo:block xsl:use-attribute-sets="header-second-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/header/paragraph[3]">
        <fo:block xsl:use-attribute-sets="header-third-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/footer/paragraph[1]">
        <fo:block xsl:use-attribute-sets="footer-first-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/footer/paragraph[2]">
        <fo:block xsl:use-attribute-sets="footer-second-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/footer/paragraph[3]">
        <fo:block xsl:use-attribute-sets="footer-third-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

</xsl:stylesheet>
