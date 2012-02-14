<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id: footnote.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->

    <xsl:key name="footnote" match="footnote" use="@ids"/>

    <!--note that a value of 'baseline' for baseline-shift means the number
    has no shift, and has the same effect as not setting the value at all-->

    <xsl:attribute-set name="footnote">
        <xsl:attribute name="font-weight">normal</xsl:attribute>
        <xsl:attribute name="font-style">normal</xsl:attribute>
    </xsl:attribute-set>

    <!--set up the defaults for the footnote label-->
    <xsl:attribute-set name="default-footnote-label-inline">
        <xsl:attribute name="baseline-shift">super</xsl:attribute>
        <xsl:attribute name="font-size">8pt</xsl:attribute>
    </xsl:attribute-set>

    <!--the attributes for the label in the body of the text-->
    <xsl:attribute-set name="footnote-body-label-inline" use-attribute-sets="default-footnote-label-inline">
    </xsl:attribute-set>


    <!-- the attributes for the list that formats each footnote-->
    <xsl:attribute-set name="footnote-list-block">
        <xsl:attribute name="provisional-label-separation">0pt</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">18pt</xsl:attribute>
    </xsl:attribute-set>

    <!--the item-lable in the list when footnotes formatted as a list-->
    <xsl:attribute-set name= "footnote-item-label">
        <xsl:attribute name="end-indent">label-end()</xsl:attribute>
    </xsl:attribute-set>

    <!--the block that surrounds the label. By default, this is not a superscript-->
    <xsl:attribute-set name= "footnote-label-block">
    </xsl:attribute-set>

    <!--the attributes for the item-body of the list when a footnote is formatted
    as a list-->
    <xsl:attribute-set name= "footnote-item-body">
        <xsl:attribute name="start-indent">body-start()</xsl:attribute>
    </xsl:attribute-set>

    <!--the attributes for the footnote-body-->
    <xsl:attribute-set name= "footnote-body">
    </xsl:attribute-set>

    <!--for the paragraphs in the footnote-->
    <xsl:attribute-set name="footnote-paragraph-block">
        <xsl:attribute name="space-before">5pt</xsl:attribute>
    </xsl:attribute-set>


    <xsl:template match="footnote_reference">
        <!--need to check that there are no ancestors in a footnote reference, or 
        you could get infinite recursion-->
        <xsl:if test="not(ancestor::footnote)">
            <xsl:apply-templates select="key('footnote', @refid)" mode="footnote"/>
        </xsl:if>
    </xsl:template>

    <xsl:template match="footnote" mode="footnote">
        <fo:footnote xsl:use-attribute-sets = "footnote">
            <xsl:apply-templates select="label" mode="footnote"/>
            <fo:footnote-body xsl:use-attribute-sets="footnote-body">
                <xsl:call-template name="footnote-list-body"/>
            </fo:footnote-body>
        </fo:footnote>
    </xsl:template>

    <xsl:template name="footnote-list-body">
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
    </xsl:template>


    <xsl:template match="footnote/label" mode="footnote">
        <fo:inline xsl:use-attribute-sets="footnote-body-label-inline">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="footnote/paragraph" mode="footnote">
        <fo:block role="footnote-para" xsl:use-attribute-sets="footnote-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


    <xsl:template match="footnote|footnote/label|footnote/paragraph"/>

</xsl:stylesheet>
