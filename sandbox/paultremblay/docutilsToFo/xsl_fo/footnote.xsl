<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

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

    <!--the attributes for the label at the bottom of the page, or with the endnotes-->
    <xsl:attribute-set name="footnote-label-inline" use-attribute-sets="default-footnote-label-inline">
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

    <!--for the paragraphs in the footnote, the first one-->
    <xsl:attribute-set name="footnote-first-paragraph-block" use-attribute-sets="footnote-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <!--ENDNOTES-->

    <!--the block that wraps all the endnotes-->
    <xsl:attribute-set name="endnotes-block">
        <xsl:attribute name="break-before">page</xsl:attribute>
    </xsl:attribute-set>

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
            <xsl:when test = "$footnote-placement = 'footnote'">
                <xsl:apply-templates select="key('footnote', @refid)" mode="footnote"/>
            </xsl:when>
            <xsl:when test = "$footnote-placement = 'endnote' and string(. + 1) = 'NaN'">
                <xsl:apply-templates select="key('footnote', @refid)" mode="footnote"/>
            </xsl:when>
            <xsl:when test = "$footnote-placement = 'endnote' and string(. + 1) != 'NaN'">
                <fo:inline xsl:use-attribute-sets="footnote-body-label-inline">
                    <xsl:value-of select="."/>
                </fo:inline> 
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="footnote" mode="footnote">
        <xsl:choose>
            <xsl:when test="$footnote-style = 'list'">
                <xsl:call-template name="footnote-as-list"/>
            </xsl:when>
            <xsl:when test="$footnote-style = 'traditional'">
                <xsl:call-template name="footnote-traditional"/>
            </xsl:when>
        </xsl:choose>
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
        <fo:block role="spacer" font-size="{$space-between-footnotes}" >&#x00a0;</fo:block>
    </xsl:template>

    <xsl:template name="footnote-as-list">
        <fo:footnote xsl:use-attribute-sets = "footnote">
            <xsl:apply-templates select="label" mode="footnote"/>
            <fo:footnote-body xsl:use-attribute-sets="footnote-body">
                <xsl:call-template name="footnote-list-body"/>
            </fo:footnote-body>
        </fo:footnote>
    </xsl:template>

    <xsl:template name="footnote-traditional">
        <fo:footnote xsl:use-attribute-sets = "footnote">
            <xsl:apply-templates select="label" mode="footnote"/>
            <fo:footnote-body xsl:use-attribute-sets="footnote-body">
                <xsl:apply-templates select="paragraph" mode="traditional-footnote"/>
                <fo:block role="spacer" font-size="{$space-between-footnotes}" >&#x00a0;</fo:block>
            </fo:footnote-body>
        </fo:footnote>
    </xsl:template>
    

    <xsl:template match="footnote/label" mode="footnote">
        <fo:inline xsl:use-attribute-sets="footnote-body-label-inline">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="footnote/paragraph" mode="footnote">
        <fo:block xsl:use-attribute-sets="footnote-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="footnote/paragraph[1]" mode="traditional-footnote" priority="2">
        <fo:block xsl:use-attribute-sets="footnote-first-paragraph-block">
            <fo:inline xsl:use-attribute-sets="footnote-label-inline">
                <xsl:value-of select="../label"/>
            </fo:inline>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="footnote/paragraph" mode="traditional-footnote">
        <fo:block xsl:use-attribute-sets="footnote-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="footnote[label = '1']" priority="2">
        <xsl:if test="$footnote-placement='endnote'">
            <fo:block role="endnotes" xsl:use-attribute-sets="endnotes-block">
                <xsl:apply-templates select="../rubric[@classes='endnotes']" mode="endnotes"/>
                <xsl:for-each select="self::footnote|following-sibling::footnote">
                    <xsl:variable name="label" select="label"/>
                    <xsl:if test="string($label + 1) != 'NaN'">
                        <xsl:call-template name="endnote"/>
                    </xsl:if>
                </xsl:for-each>
            </fo:block>
        </xsl:if>
    </xsl:template>

    <xsl:template name="endnote">
        <xsl:choose>
            <xsl:when test="$footnote-style = 'list'">
                <xsl:call-template name="footnote-list-body"/>
            </xsl:when>
            <xsl:when test="$footnote-style = 'traditional'">
                <xsl:choose>
                    <xsl:when test="self::footnote[label = '1']">
                        <fo:block role="endnote" xsl:use-attribute-sets="endnote-first-block">
                            <xsl:apply-templates select="paragraph" mode="traditional-footnote"/>
                        </fo:block>
                    </xsl:when>
                    <xsl:otherwise>
                        <fo:block role="endnote" xsl:use-attribute-sets="endnote-block">
                            <xsl:apply-templates select="paragraph" mode="traditional-footnote"/>
                        </fo:block>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="rubric[@classes='endnotes']" mode="endnotes">
        <fo:block role="endnotes-title" xsl:use-attribute-sets="endnotes-title-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


    <xsl:template match="footnote|footnote/label|footnote/paragraph|rubric[@classes='endnotes']"/>

</xsl:stylesheet>
