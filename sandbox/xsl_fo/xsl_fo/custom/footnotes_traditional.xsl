<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!--overwrites templates to create footnotes in the traditional way, rather than as lists-->

    <!--for the paragraphs in the footnote, the first one-->
    <xsl:attribute-set name="footnote-first-paragraph-block" use-attribute-sets="footnote-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <!--the attributes for the label at the bottom of the page-->
    <xsl:attribute-set name="footnote-label-inline" use-attribute-sets="default-footnote-label-inline">
    </xsl:attribute-set>


    <xsl:template match="footnote" mode="footnote">
        <fo:footnote xsl:use-attribute-sets = "footnote">
            <xsl:apply-templates select="label" mode="footnote"/>
            <fo:footnote-body xsl:use-attribute-sets="footnote-body">
                <xsl:apply-templates select="paragraph" mode="footnote"/>
                <fo:block role="spacer" font-size="{$space-between-footnotes}" >&#x00a0;</fo:block>
            </fo:footnote-body>
        </fo:footnote>
    </xsl:template>

    <xsl:template match="footnote/paragraph[1]" mode="footnote" priority="2">
        <fo:block role="footnote-first-para" xsl:use-attribute-sets="footnote-first-paragraph-block">
            <fo:inline xsl:use-attribute-sets="footnote-label-inline">
                <xsl:value-of select="../label"/>
            </fo:inline>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="footnote/paragraph" mode="footnote">
        <fo:block role="footnote-para" xsl:use-attribute-sets="footnote-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <!--have to overwrite the endnotes template, which uses the list format-->
    <xsl:template name="endnote">
        <xsl:apply-templates select="paragraph" mode="footnote"/>
    </xsl:template>


    
</xsl:stylesheet>

