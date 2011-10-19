<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <xsl:attribute-set name="borderless-table-header-cell">
        <xsl:attribute name="padding">1em</xsl:attribute>
        <xsl:attribute name="border-collapse">collapse</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="borderless-table-cell" >
    </xsl:attribute-set>

    <xsl:template match="table[@classes='borderless']">
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

    <xsl:template match="table[@classes = 'borderless']/tgroup">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="table[@classes = 'borderless']/tgroup/thead">
        <fo:table-header xsl:use-attribute-sets = "table-header">
            <xsl:apply-templates/>
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='borderless']/tgroup/thead/row">
        <fo:table-row>
            <xsl:apply-templates/>
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='borderless']/tgroup/thead/row/entry">
        <fo:table-cell xsl:use-attribute-sets="borderless-table-cell">
            <xsl:apply-templates/>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes= 'borderless']/tgroup/thead/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets="table-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


    <xsl:template match="table[@classes = 'borderless']/tgroup/tbody">
        <fo:table-body xsl:use-attribute-sets="table-body">
            <xsl:apply-templates/>
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='borderless']/tgroup/tbody/row">
        <fo:table-row xsl:use-attribute-sets="table-row">
            <xsl:apply-templates/>
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='borderless']/tgroup/tbody/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets="cell-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template name="cols-spanned">
        <xsl:choose>
            <xsl:when test="@morecols">
                <xsl:value-of select="@morecols + 1"/>
            </xsl:when>
            <xsl:otherwise>1</xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="rows-spanned">
        <xsl:choose>
            <xsl:when test="@morerows">
                <xsl:value-of select="@morerows + 1"/>
            </xsl:when>
            <xsl:otherwise>1</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
</xsl:stylesheet>
