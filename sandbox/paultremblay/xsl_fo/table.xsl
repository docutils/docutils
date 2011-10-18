<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id: table.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->


    <!--wraps the rest of the table.-->
    <xsl:attribute-set name="table-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table-header">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="default-cell">
        <xsl:attribute name="border">solid black 1px</xsl:attribute>
        <xsl:attribute name="padding">1em</xsl:attribute>
        <xsl:attribute name="border-collapse">collapse</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table-header-cell" use-attribute-sets="default-cell">
        <xsl:attribute name="border-bottom">solid black 2px</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table-header-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="table-body">
    </xsl:attribute-set>

    <xsl:attribute-set name="table-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table-cell" use-attribute-sets="default-cell">
    </xsl:attribute-set>

    <xsl:attribute-set name="cell-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="caption-block">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="space-before">6pt</xsl:attribute>
        <xsl:attribute name="space-after">6pt</xsl:attribute>
    </xsl:attribute-set>
    

    <!--END OF ATTRIBUTE SETS-->

    <xsl:template match="table[not(@classes)]">
        <fo:block-container xsl:use-attribute-sets = "table-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" mode="caption"/>
            </xsl:if>
            <fo:table xsl:use-attribute-sets="table">
                <xsl:apply-templates select="tgroup/colspec" mode="use"/>
                <xsl:apply-templates/>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" mode="caption"/>
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[not(@classes)]/tgroup">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="table/tgroup/colspec"/>

    <xsl:template match="table[not(@classes)]/tgroup/thead">
        <fo:table-header xsl:use-attribute-sets = "table-header">
            <xsl:apply-templates/>
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[not(@classes)]/tgroup/thead/row">
        <fo:table-row>
            <xsl:apply-templates/>
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[not(@classes)]/tgroup/thead/row/entry">
        <fo:table-cell xsl:use-attribute-sets="table-header-cell">
            <xsl:apply-templates/>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[not(@classes)]/tgroup/thead/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets="table-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[not(@classes)]/tgroup/tbody">
        <fo:table-body xsl:use-attribute-sets="table-body">
            <xsl:apply-templates/>
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[not(@classes)]/tgroup/tbody/row">
        <fo:table-row xsl:use-attribute-sets="table-row">
            <xsl:apply-templates/>
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[not(@classes)]/tgroup/tbody/row/entry">
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
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table-cell"
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


    <xsl:template match="table[not(@classes)]/tgroup/tbody/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets="cell-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table/title"/>

    <xsl:template match="table/title" mode="caption">
        <xsl:choose>
            <xsl:when test="$table-title-placement = 'top'">
                <fo:block xsl:use-attribute-sets="caption-block" keep-with-next="always">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$table-title-placement = 'bottom'">
                <fo:block xsl:use-attribute-sets="caption-block" keep-with-previous="always">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

     <xsl:template match="tgroup/colspec" mode="use">
        <xsl:variable name="col-num">
            <xsl:number/>
        </xsl:variable>
        <fo:table-column column-number="{$col-num}" 
            column-width="proportional-column-width({@colwidth})"/>
     </xsl:template>
    
</xsl:stylesheet>
