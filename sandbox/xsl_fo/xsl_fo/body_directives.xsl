<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:ml = "http://www.w3.org/1998/Math/MathML"
    version="1.1"
    >
    <!-- $Id: body_directives.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->

    <xsl:attribute-set name="topic-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="topic-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="topic-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">0pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="topic-first-paragraph-block" use-attribute-sets="topic-paragraph-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="sidebar-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="background-color">#FFFFF0</xsl:attribute>
        <xsl:attribute name="padding">6pt</xsl:attribute>
        <xsl:attribute name="start-indent">10mm</xsl:attribute>
        <xsl:attribute name="end-indent">40mm</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="sidebar-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="sidebar-subtitle-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="sidebar-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="sidebar-first-paragraph-block" use-attribute-sets="sidebar-paragraph-block">
        <xsl:attribute name="space-after">0pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="rubric-block">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="font-size">larger</xsl:attribute>
        <xsl:attribute name="color">red</xsl:attribute>
    </xsl:attribute-set>

    <!--epigraph-->
    <xsl:attribute-set name="epigraph-outer-block">
        <xsl:attribute name="start-indent">20mm</xsl:attribute>
        <xsl:attribute name="end-indent">20mm</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="text-align">right</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="epigraph-paragraph-block">
        <xsl:attribute name="start-indent">inherit</xsl:attribute>
        <xsl:attribute name="end-indent">inherit</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="epigraph-first-paragraph-block" use-attribute-sets="block-quote-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="epigraph-attribution-block">
        <xsl:attribute name="text-align">right</xsl:attribute>
    </xsl:attribute-set>

    <!--highlights-->
    <xsl:attribute-set name="highlights-outer-block">
        <xsl:attribute name="start-indent">20mm</xsl:attribute>
        <xsl:attribute name="end-indent">20mm</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="highlights-paragraph-block">
        <xsl:attribute name="start-indent">inherit</xsl:attribute>
        <xsl:attribute name="end-indent">inherit</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="highlights-first-paragraph-block" use-attribute-sets="block-quote-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <!--pull-quote-->
    <xsl:attribute-set name="pull-quote-outer-block">
        <xsl:attribute name="start-indent">20mm</xsl:attribute>
        <xsl:attribute name="end-indent">20mm</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="pull-quote-paragraph-block">
        <xsl:attribute name="start-indent">inherit</xsl:attribute>
        <xsl:attribute name="end-indent">inherit</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="pull-quote-first-paragraph-block" use-attribute-sets="block-quote-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="pull-quote-attribution-block">
        <xsl:attribute name="text-align">right</xsl:attribute>
    </xsl:attribute-set>

    <!--container-->
    <xsl:attribute-set name="container-outer-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="container-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="container-first-paragraph-block" use-attribute-sets="block-quote-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="mathml-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>


    <!--END OF ATTRIBUTE SETS-->

    <!--TOPIC-->
    <xsl:template match="topic[not(@classes)]">
        <fo:block xsl:use-attribute-sets="topic-block" role="topic">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="topic[not(@classes)]/title">
        <fo:block role="title" xsl:use-attribute-sets = "topic-title-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="topic[not(@classes)]/paragraph[1]" priority="2">
        <fo:block role="topic-paragraph" xsl:use-attribute-sets="topic-first-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="topic[not(@classes)]/paragraph">
        <fo:block role="topic-paragraph" xsl:use-attribute-sets="topic-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <!--SIDEBAR-->
    <!--SHOULD I generate a warning that FOP can't do sidebars instead?-->
    <xsl:template match="sidebar">
        <fo:block xsl:use-attribute-sets="sidebar-block" role="sidebar">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="sidebar/title">
        <fo:block role="title" xsl:use-attribute-sets = "sidebar-title-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="sidebar/subtitle">
        <fo:block role="subtitle" xsl:use-attribute-sets = "sidebar-subtitle-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="sidebar/paragraph[1]" priority="2">
        <fo:block role="sidebar-paragraph" xsl:use-attribute-sets="sidebar-first-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="sidebar/paragraph">
        <fo:block role="sidebar-paragraph" xsl:use-attribute-sets="sidebar-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <!--RUBRIC-->
    <xsl:template match="rubric[not(@classes)]">
        <fo:block role="rubric" xsl:use-attribute-sets="rubric-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <!--EPIGRAPH-->
    <xsl:template match="block_quote[@classes='epigraph']">
        <fo:block role="epigraph" xsl:use-attribute-sets = "epigraph-outer-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[@classes='epigraph']/paragraph">
        <fo:block role="epigraph-paragraph" xsl:use-attribute-sets = "epigraph-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[@classes='epigraph']/paragraph[1]" priority="2">
        <fo:block role="epigraph-paragraph" xsl:use-attribute-sets = "epigraph-first-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[@classes='epigraph']/attribution">
        <fo:block role="epigraph-attribution" xsl:use-attribute-sets = "epigraph-attribution-block">
            <xsl:value-of select="$text-before-epigraph-attribution"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <!--HIGHLIGHTS-->
    <xsl:template match="block_quote[@classes='highlights']">
        <fo:block role="highlights" xsl:use-attribute-sets = "highlights-outer-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[@classes='highlights']/paragraph">
        <fo:block role="highlights-paragraph" xsl:use-attribute-sets = "highlights-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[@classes='highlights']/paragraph[1]" priority="2">
        <fo:block role="highlights-paragraph" xsl:use-attribute-sets = "highlights-first-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <!--PULL-QUOTE-->
    <xsl:template match="block_quote[@classes='pull-quote']">
        <fo:block role="pull-quote" xsl:use-attribute-sets = "pull-quote-outer-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[@classes='pull-quote']/paragraph">
        <fo:block role="pull-quote-paragraph" xsl:use-attribute-sets = "pull-quote-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[@classes='pull-quote']/paragraph[1]" priority="2">
        <fo:block role="pull-quote-paragraph" xsl:use-attribute-sets = "pull-quote-first-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[@classes='pull-quote']/attribution">
        <fo:block role="pull-quote-attribution" xsl:use-attribute-sets = "pull-quote-attribution-block">
            <xsl:value-of select="$text-before-pull-quote-attribution"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <!--CONTAINER-->
    <xsl:template match="container">
        <fo:block role="container" xsl:use-attribute-sets = "container-outer-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="container/paragraph">
        <fo:block role="container-paragraph" xsl:use-attribute-sets = "container-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="container/paragraph[1]" priority="2">
        <fo:block role="container-paragraph" xsl:use-attribute-sets = "container-first-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="compound">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="math_block">
        <xsl:choose>
            <xsl:when test="descendant::ml:math">
                <fo:block xsl:use-attribute-sets="mathml-block">
                    <fo:instream-foreign-object>
                        <xsl:copy-of select="ml:math"/>
                    </fo:instream-foreign-object>
                </fo:block>
            </xsl:when>
            <xsl:otherwise>
                <fo:block xsl:use-attribute-sets="literal-block">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:otherwise>
        </xsl:choose>
        
    </xsl:template>
    


</xsl:stylesheet>
