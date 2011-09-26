<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    
    <xsl:attribute-set name="borderless-table-block-container" >
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="borderless-table">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="borderless-table-header">
    </xsl:attribute-set>

    <xsl:attribute-set name="borderless-table-header-row">
    </xsl:attribute-set>

    <xsl:attribute-set name="borderless-table-header-cell">
        <xsl:attribute name="padding">1em</xsl:attribute>
        <xsl:attribute name="border-collapse">collapse</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="borderless-table-header-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="borderless-table-body">
    </xsl:attribute-set>

    <xsl:attribute-set name="borderless-table-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="borderless-table-cell" >
    </xsl:attribute-set>

    <xsl:attribute-set name="borderless-cell-block">
    </xsl:attribute-set>



    <xsl:template match="table[@classes='borderless']">
        <fo:block-container xsl:use-attribute-sets = "borderless-table-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" mode="caption"/>
            </xsl:if>
            <fo:table role="borderless" xsl:use-attribute-sets="borderless-table">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates/>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" mode="caption"/>
            </xsl:if>
        </fo:block-container>
    </xsl:template>

    <xsl:template match="table[@classes='borderless']/tgroup">
        <xsl:apply-templates/>
    </xsl:template>


    <xsl:template match="table[@classes = 'borderless']/tgroup/thead">
        <fo:table-header xsl:use-attribute-sets = "borderless-table-header">
            <xsl:apply-templates/>
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='borderless']/tgroup//thead/row">
        <fo:table-row xsl:use-attribute-sets="borderless-table-header-row">
            <xsl:apply-templates/>
        </fo:table-row>
    </xsl:template>


    <xsl:template match="table[@classes='borderless']/tgroup/thead/row/entry">
        <fo:table-cell xsl:use-attribute-sets="borderless-table-header-cell">
            <xsl:apply-templates/>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='borderless']/tgroup/thead/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets="borderless-table-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='borderless']/tgroup/tbody">
        <fo:table-body xsl:use-attribute-sets="borderless-table-body">
            <xsl:apply-templates/>
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='borderless']/tgroup/tbody/row">
        <fo:table-row xsl:use-attribute-sets="borderless-table-row">
            <xsl:apply-templates/>
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes = 'borderless']/tgroup/tbody/row/entry">
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="borderless-table-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="borderless-table-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="borderless-table-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="borderless-table-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes = 'borderless']/tgroup/tbody/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets="borderless-cell-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


</xsl:stylesheet> 
