<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id: footnote.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->

    <xsl:key name="footnote" match="footnote" use="@ids"/>

    <!--note that a value of 'baseline' for baseline-shift means the number
    has no shift, and has the same effect as not setting the value at all-->



    <!--controls the space between each endnote-->
    <xsl:attribute-set name="endnote-block">
        <xsl:attribute name="space-before">5pt</xsl:attribute>
    </xsl:attribute-set>

    <!--the spacing for the first endnote-->
    <xsl:attribute-set name="endnote-first-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>


    <!--for the title of the endnotes-->
    <xsl:attribute-set name="endnotes-title-block">
        <xsl:attribute name="space-after">18pt</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">18pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

    <xsl:template match="footnote_reference">
        <xsl:choose>
            <xsl:when test = "string(. + 1) = 'NaN'">
                <xsl:apply-templates select="key('footnote', @refid)" mode="footnote"/>
            </xsl:when>
            <xsl:otherwise>
                <fo:inline xsl:use-attribute-sets="footnote-body-label-inline">
                    <xsl:value-of select="."/>
                </fo:inline> 
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="footnote[label = '1']" priority="2">
        <xsl:apply-templates select="../rubric[@classes='endnotes']" mode="endnotes"/>
        <xsl:for-each select="self::footnote|following-sibling::footnote">
            <xsl:variable name="label" select="label"/>
            <xsl:if test="string($label + 1) != 'NaN'">
                <xsl:call-template name="endnote"/>
            </xsl:if>
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="endnote">
        <xsl:call-template name="footnote-list-body"/>
    </xsl:template>

    <xsl:template match="rubric[@classes='endnotes']" mode="endnotes">
        <fo:block role="endnotes-title" xsl:use-attribute-sets="endnotes-title-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


    <xsl:template match="footnote|footnote/label|footnote/paragraph|rubric[@classes='endnotes']"/>

</xsl:stylesheet>
