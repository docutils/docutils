<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id$ -->

     <xsl:attribute-set name="table1-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table1">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">6in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="thead1-header">
    </xsl:attribute-set>

    <xsl:attribute-set name="thead1-cell" use-attribute-sets="default-cell">
    </xsl:attribute-set>

    <xsl:attribute-set name="thead1-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="table1-body">
    </xsl:attribute-set>

    <xsl:attribute-set name="table1-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table1-cell" use-attribute-sets="default-cell">
    </xsl:attribute-set>

    <xsl:attribute-set name="table1-cell-borderless" >
        <xsl:attribute name="padding">1em</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="cell1-block">
    </xsl:attribute-set>

    

     <!--END OF ATTRIBUTE SETS-->

     <xsl:template name="make-fo-table-cols">
         <xsl:param name="cols-string"/>
         <xsl:param name="col-num">1</xsl:param>
         <xsl:variable name="before-comma" 
            select="normalize-space(substring-before($cols-string, ','))" />
         <xsl:variable name="col-width">
             <xsl:choose>
                 <xsl:when test="$before-comma">
                     <xsl:value-of select="$before-comma"/>
                 </xsl:when>
                 <xsl:otherwise>
                     <xsl:value-of select="$cols-string"/>
                 </xsl:otherwise>
             </xsl:choose>
         </xsl:variable>
         <xsl:choose>
             <xsl:when test="$col-width != ''">
                <fo:table-column column-number="{$col-num}" 
                    column-width="proportional-column-width({$col-width})"/>
                <xsl:call-template name="make-fo-table-cols">
                    <xsl:with-param name="cols-string" 
                        select= "normalize-space(substring-after($cols-string, ','))"/>
                    <xsl:with-param name="col-num" select="$col-num + 1"/>
                </xsl:call-template>
             </xsl:when>
         </xsl:choose>
     </xsl:template>

    <xsl:template name="get-column-widths">
        <xsl:param name="classes"/>
        <xsl:variable name="columns">
            <xsl:choose>
                <xsl:when test="$classes = 'table1'">
                    <xsl:value-of select="$table1-cols"/>
                </xsl:when>
                <xsl:when test="$classes = 'table2'">
                    <xsl:value-of select="$table2-cols"/>
                </xsl:when>
                <xsl:when test="$classes = 'table3'">
                    <xsl:value-of select="$table3-cols"/>
                </xsl:when>
            </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="$columns"/>
    </xsl:template>

     <xsl:template match="tgroup" mode="classes">
         <xsl:param name="classes"/>
         <xsl:apply-templates mode="classes">
             <xsl:with-param name="classes" select="$classes"/>
         </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="tgroup/colspec" mode="classes"/>

    <xsl:template match="tgroup/colspec" mode="use">
        <xsl:variable name="col-num">
            <xsl:number/>
        </xsl:variable>
        <fo:table-column column-number="{$col-num}" 
            column-width="proportional-column-width({@colwidth})"/>
    </xsl:template>

    <xsl:template name="make-col-specs">
        <xsl:param name="classes"/>
        <xsl:variable name="columns">
            <xsl:call-template name = "get-column-widths">
                <xsl:with-param name="classes" select = "$classes"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$columns != ''">
                <xsl:call-template name="make-fo-table-cols">
                    <xsl:with-param name="cols-string" select="$columns"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates select="tgroup/colspec" mode="use">
                    <xsl:with-param name="classe" select="$classes"/>
                </xsl:apply-templates>
            </xsl:otherwise>
        </xsl:choose>
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


     <xsl:template match="table[@classes='table1']">
        <fo:block-container xsl:use-attribute-sets = "table1-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" mode="caption"/>
            </xsl:if>
            <fo:table role="table1" xsl:use-attribute-sets="table1">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates mode="classes">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" mode="caption"/>
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table1']/tgroup/thead" mode="classes">
        <fo:table-header xsl:use-attribute-sets = "thead1-header">
            <xsl:apply-templates mode="classes"/>
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/thead/row" mode="classes">
        <xsl:apply-templates mode="classes"/>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/thead/row/entry" mode="classes">
        <fo:table-cell xsl:use-attribute-sets="thead1-cell">
            <xsl:apply-templates mode="classes"/>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/thead/row/entry/paragraph" mode="classes">
        <fo:block xsl:use-attribute-sets="thead1-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/tbody" mode="classes">
        <fo:table-body xsl:use-attribute-sets="table1-body">
            <xsl:apply-templates mode="classes"/>
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/tbody/row" mode="classes">
        <fo:table-row xsl:use-attribute-sets="table-row">
            <xsl:apply-templates mode="classes"/>
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/tbody/row/entry" mode="classes">
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table1-cell">
                    <xsl:apply-templates mode="classes"/>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table1-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates mode="classes"/>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table1-cell"
                    number-columns-spanned="{$columns-spanned}">
                    <xsl:apply-templates mode="classes"/>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table1-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$columns-spanned}">
                    <xsl:apply-templates mode="classes"/>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/tbody/row/entry/paragraph" mode="classes">
        <fo:block xsl:use-attribute-sets="cell1-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


     <xsl:template match="title" mode="classes"/>
</xsl:stylesheet>
