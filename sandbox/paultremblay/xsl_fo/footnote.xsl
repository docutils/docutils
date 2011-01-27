<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:key name="footnote" match="footnote" use="@ids"/>

    <xsl:attribute-set name="footnote-label-inline">
        <xsl:attribute name="baseline-shift">super</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="footnote-paragraph-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="footnote-list-block">
        <xsl:attribute name="provisional-label-separation">0pt</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">18pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name= "footnote-item-label">
        <xsl:attribute name="end-indent">label-end()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name= "footnote-label-block">
    </xsl:attribute-set>

    <xsl:attribute-set name= "footnote-item-body">
        <xsl:attribute name="start-indent">body-start()</xsl:attribute>
    </xsl:attribute-set>

    <!--three way to place footnotes:

    1. as list as a footnote

    2. more traditional, as footnote

    3. as endnotes, or where the footnotes really are.
    -->


    <xsl:template match="footnote_reference">
        <xsl:apply-templates select="key('footnote', @refid)" mode="footnote"/>
    </xsl:template>

    <xsl:template match="footnote" mode="footnote">
        <fo:footnote>
            <xsl:apply-templates select="label" mode="footnote"/>
            <fo:footnote-body xsl:use-attribute-sets="footnote-block">
                <fo:list-block xsl:use-attribute-sets="footnote-list-block">
                    <fo:list-item>
                        <fo:list-item-label xsl:use-attribute-sets="footnote-item-label">
                            <fo:block xsl:use-attribute-sets="footnote-label-block">
                                <xsl:value-of select="label"/>
                            </fo:block>
                        </fo:list-item-label>
                        <fo:list-item-body xsl:use-attribute-sets="footnote-item-body">
                            <xsl:apply-templates select="paragraph" mode="footnote"/>
                        </fo:list-item-body>
                    </fo:list-item>
                </fo:list-block>
            </fo:footnote-body>
        </fo:footnote>
    </xsl:template>
    

    <xsl:template match="footnote/label" mode="footnote">
        <fo:inline>
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="footnote/paragraph" mode="footnote">
        <fo:block xsl:use-attribute-sets="footnote-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="footnote|footnote/label|footnote/paragraph"/>

</xsl:stylesheet>
