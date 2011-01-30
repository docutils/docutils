<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <xsl:attribute-set name="figure-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="image-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="figure-caption-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">smaller</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="figure-legend-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="legend-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="legend-first-paragraph-block" use-attribute-sets="legend-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:template name="get-alignment">
        <xsl:choose>
            <xsl:when test="@align">
                <xsl:value-of select="@align"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>left</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="create-external-image-element">
        <xsl:element name="fo:external-graphic">
            <xsl:attribute name="src">
                <xsl:text>url('</xsl:text>
                <xsl:value-of select="@uri"/>
                <xsl:text>')</xsl:text>
            </xsl:attribute>
            <xsl:if test="@scale">
                <xsl:attribute name="content-height">
                    <xsl:value-of select="@scale"/>
                    <xsl:text>%</xsl:text>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test="@height">
                <xsl:attribute name="content-height">
                    <xsl:value-of select="@height"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test="@width">
                <xsl:attribute name="content-width">
                    <xsl:value-of select="@width"/>
                </xsl:attribute>
            </xsl:if>
            <!--doesn't work-->
            <xsl:if test="@align">
                <xsl:attribute name="text-align">
                    <xsl:value-of select="@align"/>
                </xsl:attribute>
            </xsl:if>
        </xsl:element>
    </xsl:template>

    <xsl:template match="image">
        <xsl:call-template name="test-attributes"/>
        <xsl:variable name="text-align">
            <xsl:call-template name="get-alignment"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="parent::figure">
                <fo:block role="image" xsl:use-attribute-sets="image-block" >
                    <xsl:call-template name="create-external-image-element"/>
                </fo:block>
            </xsl:when>
            <xsl:otherwise>
                <fo:block role="image" xsl:use-attribute-sets="image-block" text-align="{$text-align}" >
                    <xsl:call-template name="create-external-image-element"/>
                </fo:block>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="figure">
        <xsl:variable name="text-align">
            <xsl:call-template name="get-alignment"/>
        </xsl:variable>
        <fo:block role="figure" xsl:use-attribute-sets="figure-block" text-align="{$text-align}">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="figure/caption">
        <fo:block role="caption" xsl:use-attribute-sets="figure-caption-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="figure/legend">
        <fo:block role="legend" xsl:use-attribute-sets="figure-legend-block" id="generate-id()">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="figure/legend/paragraph[1]" priority="2">
        <fo:block role="legend-paragraph-block" xsl:use-attribute-sets="legend-first-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="figure/legend/paragraph">
        <fo:block role="legend-paragraph-block" xsl:use-attribute-sets="legend-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <!--just test that top and bottom aren't used, since they make no sense-->
    <xsl:template name="test-attributes">
    </xsl:template>

    
</xsl:stylesheet>
