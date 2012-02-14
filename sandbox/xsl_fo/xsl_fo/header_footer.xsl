<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Date: 2011-01-09 02:51:33 -0500 (Sun, 09 Jan 2011) $ -->
    <!--
    This stylesheet handles headers and footers. It creates the fo:static-content 
    elements, and the child fo:block elements. 
    -->


    <xsl:attribute-set name="paragraph-header-block">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="paragraph-footer-block">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>


    <xsl:template match="decoration">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="decoration/header" mode="header">
        <fo:static-content flow-name="xsl-region-before">
            <xsl:apply-templates/>
        </fo:static-content>
    </xsl:template>

    <xsl:template match="decoration/footer" mode="footer">
        <fo:static-content flow-name="xsl-region-after">
            <xsl:apply-templates/>
        </fo:static-content>
    </xsl:template>

    <xsl:template match="inline[@classes='page-num']">
        <fo:page-number/>
    </xsl:template>

    <xsl:template match="decoration/header/paragraph">
        <fo:block role="header" xsl:use-attribute-sets="paragraph-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/footer/paragraph">
        <fo:block role="footer" xsl:use-attribute-sets="paragraph-footer-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/header|decoration/footer"/>


</xsl:stylesheet>
