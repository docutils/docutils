<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id$ -->

    <!--Just a temp fix for now-->
    <xsl:variable name="table-width">6in</xsl:variable>

    <xsl:attribute-set name="table">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension"><xsl:value-of select="$table-width"/></xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="thead-header">
    </xsl:attribute-set>

    <xsl:attribute-set name="default-cell">
        <xsl:attribute name="border">solid black 1px</xsl:attribute>
        <xsl:attribute name="padding">1em</xsl:attribute>
        <xsl:attribute name="border-collapse">collapse</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="thead-cell" use-attribute-sets="default-cell">
    </xsl:attribute-set>

    <xsl:attribute-set name="thead-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="table-body">
    </xsl:attribute-set>

    <xsl:attribute-set name="table-row">
    </xsl:attribute-set>

    <xsl:attribute-set name="table-cell" use-attribute-sets="default-cell">
    </xsl:attribute-set>

    <xsl:attribute-set name="cell-block">
    </xsl:attribute-set>
    

    <!--END OF ATTRIBUTE SETS-->

    <xsl:template match="table">
        <fo:table xsl:use-attribute-sets="table">
            <xsl:apply-templates/>
        </fo:table>
    </xsl:template>

    <xsl:template match="tgroup">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="tgroup/colspec">
        <xsl:variable name="col-num">
            <xsl:number/>
        </xsl:variable>
        <fo:table-column column-number="{$col-num}" 
            column-width="proportional-column-width({@colwidth})"/>
    </xsl:template>

    <xsl:template match="tgroup/thead">
        <fo:table-header xsl:use-attribute-sets = "thead-header">
            <xsl:apply-templates/>
        </fo:table-header>
    </xsl:template>

    <xsl:template match="thead/row">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="thead/row/entry">
        <fo:table-cell xsl:use-attribute-sets="thead-cell">
            <xsl:apply-templates/>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="thead/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets="thead-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="tbody">
        <fo:table-body xsl:use-attribute-sets="table-body">
            <xsl:apply-templates/>
        </fo:table-body>
    </xsl:template>

    <xsl:template match="tbody/row">
        <fo:table-row xsl:use-attribute-sets="table-row">
            <xsl:apply-templates/>
        </fo:table-row>
    </xsl:template>

    <xsl:template match="tbody/row/entry">
        <xsl:variable name="cols-spanned">
            <xsl:choose>
                <xsl:when test="@morecols">
                    <xsl:value-of select="@morecols + 1"/>
                </xsl:when>
                <xsl:otherwise>1</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:choose>
                <xsl:when test="@morerows">
                    <xsl:value-of select="@morerows + 1"/>
                </xsl:when>
                <xsl:otherwise>1</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <fo:table-cell xsl:use-attribute-sets="table-cell" 
            number-columns-spanned="{$cols-spanned}"
            number-rows-spanned="{$rows-spanned}"
            >
            <xsl:apply-templates/>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="tbody/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets="cell-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    
</xsl:stylesheet>
