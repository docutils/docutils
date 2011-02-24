<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id$ -->

    <xsl:attribute-set name="long-table">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="long-thead-header">
    </xsl:attribute-set>

    <xsl:attribute-set name="long-thead-cell" use-attribute-sets="default-cell">
    </xsl:attribute-set>

    <xsl:attribute-set name="long-thead-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="long-table-body">
    </xsl:attribute-set>

    <xsl:attribute-set name="long-table-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="long-table-cell" use-attribute-sets="default-cell">
    </xsl:attribute-set>

    <xsl:attribute-set name="long-cell-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="long-caption-block">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="space-before">6pt</xsl:attribute>
        <xsl:attribute name="space-after">26pt</xsl:attribute>
        <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
        <xsl:attribute name="space-after.conditionality">retain</xsl:attribute>
    </xsl:attribute-set>
    
    <!--END OF ATTRIBUTE SETS-->

    <xsl:template name="make-first-long-header">
        <xsl:variable name="cols-spanned"  select="count(tgroup/colspec)"/>
        <fo:table-row role="caption">
            <fo:table-cell number-columns-spanned="{$cols-spanned}">
                <fo:block xsl:use-attribute-sets="long-caption-block">
                    <xsl:apply-templates select="title" mode="use"/>
                </fo:block>
            </fo:table-cell>
        </fo:table-row>
    </xsl:template>

    <xsl:template name="make-second-long-header">
        <xsl:variable name="cols-spanned"  select="count(tgroup/colspec)"/>
        <xsl:choose>
            <xsl:when test="preceding-sibling::rubric[@classes='caption-non-first-page']">
                <fo:table-row role="caption">
                    <fo:table-cell number-columns-spanned="{$cols-spanned}">
                        <fo:block xsl:use-attribute-sets="long-caption-block">
                            <xsl:apply-templates 
                                select="preceding-sibling::rubric[@classes='caption-non-first-page']"
                                mode="use"/>
                        </fo:block>
                    </fo:table-cell>
                </fo:table-row>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="make-first-long-header"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="caption-if-no-header">
        <xsl:param name="type"/>
        <xsl:if test="not(tgroup/thead) and title">
            <fo:table-header xsl:use-attribute-sets = "long-thead-header">
                <xsl:choose>
                    <xsl:when test = "$type = 'first'">
                        <xsl:call-template name="make-first-long-header"/>
                    </xsl:when>
                    <xsl:when test = "$type = 'second'">
                        <xsl:call-template name="make-second-long-header"/>
                    </xsl:when>
                </xsl:choose>
            </fo:table-header>
        </xsl:if>
    </xsl:template>

    <xsl:template match="table[@classes= 'long']">
        <fo:table xsl:use-attribute-sets="long-table" break-after="page">
            <xsl:call-template name="make-col-specs">
                <xsl:with-param name="classes" select="@classes"/>
            </xsl:call-template>
            <xsl:call-template name="caption-if-no-header">
                <xsl:with-param name="type" select="'first'"/>
            </xsl:call-template>
            <xsl:apply-templates mode="first-long"/>
        </fo:table>
        <fo:table xsl:use-attribute-sets="long-table">
            <xsl:call-template name="make-col-specs">
                <xsl:with-param name="classes" select="@classes"/>
            </xsl:call-template>
            <xsl:call-template name="caption-if-no-header">
                <xsl:with-param name="type" select="'second'"/>
            </xsl:call-template>
            <xsl:apply-templates mode="second-long"/>
        </fo:table>
    </xsl:template>

    <xsl:template match="table/title" mode="use" priority="2">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="table/title" mode="first-long"/>
    <xsl:template match="table/title" mode="second-long"/>

    <xsl:template match="tgroup" mode="first-long">
        <xsl:apply-templates mode="first-long"/>
    </xsl:template>

    <xsl:template match="tgroup" mode="second-long">
        <xsl:apply-templates mode="second-long"/>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/thead" mode="first-long">
        <fo:table-header xsl:use-attribute-sets = "long-thead-header">
            <xsl:if test="../../title and $table-title-placement = 'top'">
                <xsl:for-each select="../..">
                    <xsl:call-template name="make-first-long-header"/>
                </xsl:for-each>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes = 'long']/tgroup/thead" mode="second-long">
        <fo:table-header xsl:use-attribute-sets = "long-thead-header">
            <xsl:if test="../../title and $table-title-placement = 'top'">
                <xsl:for-each select="../..">
                    <xsl:call-template name="make-second-long-header"/>
                </xsl:for-each>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/thead/row">
        <fo:table-row xsl:use-attribute-sets = "long-header-table-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/thead/row/entry">
        <fo:table-cell xsl:use-attribute-sets = "long-thead-cell">
            <xsl:apply-templates/>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/thead/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets = "long-thead-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes = 'long']/tgroup/tbody" mode="first-long">
        <fo:table-body xsl:use-attribute-sets="long-table-body">
            <xsl:apply-templates mode="first-long"/>
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/tbody" mode="second-long">
        <fo:table-body xsl:use-attribute-sets="long-table-body">
            <xsl:apply-templates mode="second-long"/>
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/tbody/row" mode="first-long">
        <xsl:variable name="num">
            <xsl:number/>
        </xsl:variable>
        <xsl:if test="$num  &lt; $long-rows-first-page + 1">
            <fo:table-row xsl:use-attribute-sets = "long-table-row">
                <xsl:apply-templates />
            </fo:table-row>
        </xsl:if>
    </xsl:template>

    <xsl:template match="table[@classes = 'long']/tgroup/tbody/row" mode="second-long">
        <xsl:variable name="num">
            <xsl:number/>
        </xsl:variable>
        <xsl:if test="$num &gt; $long-rows-first-page">
            <fo:table-row xsl:use-attribute-sets = "long-table-row">
                <xsl:apply-templates />
            </fo:table-row>
        </xsl:if>
    </xsl:template>

    <xsl:template match="table[@classes = 'long']/tgroup/tbody/row/entry">
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="long-table-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="long-table-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="long-table-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="long-table-cell"
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


    <xsl:template match="tbody/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets="cell-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="rubric[@classes = 'caption-non-first-page']"/>


</xsl:stylesheet> 
