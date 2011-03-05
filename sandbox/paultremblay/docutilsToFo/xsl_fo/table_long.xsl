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

    <xsl:attribute-set name="long-table-header">
    </xsl:attribute-set>

    <xsl:attribute-set name="long-table-header-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="long-table-header-cell" use-attribute-sets="default-cell">
    </xsl:attribute-set>

    <xsl:attribute-set name="long-table-header-block">
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
        <xsl:if test="not(tgroup/thead) and title and $table-title-placement = 'top'">
            <fo:table-header xsl:use-attribute-sets = "long-table-header">
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

    <xsl:template name="caption-if-no-footer">
        <xsl:param name="type"/>
        <xsl:if test="not(tgroup/tfoot) and title and $table-title-placement = 'bottom'">
            <fo:table-footer xsl:use-attribute-sets = "long-table-header">
                <xsl:choose>
                    <xsl:when test = "$type = 'first'">
                        <xsl:call-template name="make-first-long-header"/>
                    </xsl:when>
                    <xsl:when test = "$type = 'second'">
                        <xsl:call-template name="make-second-long-header"/>
                    </xsl:when>
                </xsl:choose>
            </fo:table-footer>
        </xsl:if>
    </xsl:template>

    <xsl:template name="num-first-rows">
        <xsl:param name="preceding-tables"/>
        <xsl:param name="row-string"/>
        <xsl:param name="first-run">True</xsl:param>
        <xsl:param name="recurs-num">1</xsl:param>
        <xsl:variable name="table-num">
            <xsl:choose>
                <xsl:when test="$preceding-tables != ''">
                    <xsl:value-of select="$preceding-tables"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="count(preceding::table[@classes='long']) + 1"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="row-str">
            <xsl:choose>
                <xsl:when test="$first-run = 'True'">
                    <xsl:value-of select="normalize-space($long-rows-first-page)"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$row-string"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="num">
            <xsl:choose>
                <xsl:when test="substring-before($row-str, ',') != ''">
                    <xsl:value-of select="substring-before($row-str, ',')"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="normalize-space($row-str)"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$num = ''"/>
            <xsl:when test="$recurs-num = $table-num">
                <xsl:variable name="num-t-rows" select =  "count(tgroup/tbody/row)"/>
                <xsl:if test = "$num != 0 and $num &gt; $num-t-rows - 1">
                    <xsl:variable name="msg">
                        <xsl:value-of select="$num"/>
                        <xsl:text> too large a number for long table </xsl:text>
                        <xsl:value-of select="$table-num"/>
                        <xsl:text>&#xA;</xsl:text>
                        <xsl:text>Table has </xsl:text>
                        <xsl:value-of select="$num-t-rows"/>
                        <xsl:text> rows. Can have no more than </xsl:text>
                        <xsl:value-of select="$num-t-rows - 1"/>
                        <xsl:text> rows on first page.&#xA;</xsl:text>
                    </xsl:variable>
                    <xsl:call-template name="quit-message">
                        <xsl:with-param name="msg" select="$msg"/>
                    </xsl:call-template>
                </xsl:if>
                <xsl:value-of select="$num"/> 
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="num-first-rows">
                    <xsl:with-param name="preceding-tables" select="$table-num"/>
                    <xsl:with-param name="recurs-num" select="$recurs-num + 1"/>
                    <xsl:with-param name="row-string" select="substring-after($row-str, ',')"/>
                    <xsl:with-param name="first-run" select="'False'"/>
                </xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <xsl:template match="table[@classes= 'long']">
        <xsl:variable name="num-first-rows">
            <xsl:call-template name="num-first-rows"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$num-first-rows = '' or $num-first-rows = 0">
                <fo:table xsl:use-attribute-sets="long-table">
                    <xsl:call-template name="make-col-specs">
                        <xsl:with-param name="classes" select="@classes"/>
                    </xsl:call-template>
                    <xsl:call-template name="caption-if-no-header">
                        <xsl:with-param name="type" select="'first'"/>
                    </xsl:call-template>
                    <xsl:if test = "not(tgroup/thead)">
                        <xsl:call-template name="caption-if-no-footer">
                            <xsl:with-param name="type" select="'first'"/>
                        </xsl:call-template>
                    </xsl:if>
                    <xsl:apply-templates/>
                </fo:table>
            </xsl:when>
            <xsl:otherwise>
                <fo:table xsl:use-attribute-sets="long-table" break-after="page">
                    <xsl:call-template name="make-col-specs">
                        <xsl:with-param name="classes" select="@classes"/>
                    </xsl:call-template>
                    <xsl:call-template name="caption-if-no-header">
                        <xsl:with-param name="type" select="'first'"/>
                    </xsl:call-template>
                    <xsl:if test = "not(tgroup/thead)">
                        <xsl:call-template name="caption-if-no-footer">
                            <xsl:with-param name="type" select="'first'"/>
                        </xsl:call-template>
                    </xsl:if>
                    <xsl:apply-templates mode="first-long">
                        <xsl:with-param name="num-first-rows" select="$num-first-rows"/>
                    </xsl:apply-templates>
                </fo:table>
                <fo:table xsl:use-attribute-sets="long-table">
                    <xsl:call-template name="make-col-specs">
                        <xsl:with-param name="classes" select="@classes"/>
                    </xsl:call-template>
                    <xsl:call-template name="caption-if-no-header">
                        <xsl:with-param name="type" select="'second'"/>
                    </xsl:call-template>
                    <xsl:if test = "not(tgroup/thead)">
                        <xsl:call-template name="caption-if-no-footer">
                            <xsl:with-param name="type" select="'second'"/>
                        </xsl:call-template>
                    </xsl:if>
                    <xsl:apply-templates mode="second-long">
                        <xsl:with-param name="num-first-rows" select="$num-first-rows"/>
                    </xsl:apply-templates>
                </fo:table>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table/title" mode="use" priority="2">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="table/title" mode="first-long"/>
    <xsl:template match="table/title" mode="second-long"/>

    <xsl:template match="tgroup" mode="first-long">
        <xsl:param name="num-first-rows"/>
        <xsl:apply-templates mode="first-long">
            <xsl:with-param name="num-first-rows" select="$num-first-rows"/>
        </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="tgroup" mode="second-long">
        <xsl:param name="num-first-rows"/>
        <xsl:apply-templates mode="second-long">
            <xsl:with-param name="num-first-rows" select="$num-first-rows"/>
        </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/thead" mode="first-long">
        <fo:table-header xsl:use-attribute-sets = "long-table-header">
            <xsl:if test="../../title and $table-title-placement = 'top'">
                <xsl:for-each select="../..">
                    <xsl:call-template name="make-first-long-header"/>
                </xsl:for-each>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:table-header>
        <xsl:for-each select="../..">
            <xsl:call-template name="caption-if-no-footer">
                <xsl:with-param name="type" select="'first'"/>
            </xsl:call-template>
        </xsl:for-each>
    </xsl:template>

    <xsl:template match="table[@classes = 'long']/tgroup/thead" mode="second-long">
        <fo:table-header xsl:use-attribute-sets = "long-table-header">
            <xsl:if test="../../title and $table-title-placement = 'top'">
                <xsl:for-each select="../..">
                    <xsl:call-template name="make-second-long-header"/>
                </xsl:for-each>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:table-header>
        <xsl:for-each select="../..">
            <xsl:call-template name="caption-if-no-footer">
                <xsl:with-param name="type" select="'second'"/>
            </xsl:call-template>
        </xsl:for-each>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/thead">
        <fo:table-header xsl:use-attribute-sets = "long-table-header">
            <xsl:if test="../../title and $table-title-placement = 'top'">
                <xsl:for-each select="../..">
                    <xsl:call-template name="make-first-long-header"/>
                </xsl:for-each>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:table-header>
        <xsl:for-each select="../..">
            <xsl:call-template name="caption-if-no-footer">
                <xsl:with-param name="type" select="'first'"/>
            </xsl:call-template>
        </xsl:for-each>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/thead/row">
        <fo:table-row xsl:use-attribute-sets = "long-table-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/thead/row/entry">
        <fo:table-cell xsl:use-attribute-sets = "long-table-header-cell">
            <xsl:apply-templates/>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/thead/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets = "long-table-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes = 'long']/tgroup/tbody" mode="first-long">
        <xsl:param name="num-first-rows"/>
        <fo:table-body xsl:use-attribute-sets="long-table-body">
            <xsl:apply-templates mode="first-long">
                <xsl:with-param name="num-first-rows" select="$num-first-rows"/>
            </xsl:apply-templates>
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/tbody" mode="second-long">
        <xsl:param name="num-first-rows"/>
        <fo:table-body xsl:use-attribute-sets="long-table-body">
            <xsl:apply-templates mode="second-long">
                <xsl:with-param name="num-first-rows" select="$num-first-rows"/>
            </xsl:apply-templates>
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes = 'long']/tgroup/tbody">
        <xsl:param name="num-first-rows"/>
        <fo:table-body xsl:use-attribute-sets="long-table-body">
            <xsl:apply-templates/>
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/tbody/row" mode="first-long">
        <xsl:param name="num-first-rows"/>
        <xsl:variable name="num">
            <xsl:number/>
        </xsl:variable>
        <xsl:if test="$num  &lt; $num-first-rows + 1">
            <fo:table-row xsl:use-attribute-sets = "long-table-row">
                <xsl:apply-templates />
            </fo:table-row>
        </xsl:if>
    </xsl:template>

    <xsl:template match="table[@classes = 'long']/tgroup/tbody/row" mode="second-long">
        <xsl:param name="num-first-rows"/>
        <xsl:variable name="num">
            <xsl:number/>
        </xsl:variable>
        <xsl:if test="$num &gt; $num-first-rows">
            <fo:table-row xsl:use-attribute-sets = "long-table-row">
                <xsl:apply-templates />
            </fo:table-row>
        </xsl:if>
    </xsl:template>

    <xsl:template match="table[@classes ='long']/tgroup/tbody/row">
        <fo:table-row xsl:use-attribute-sets = "long-table-row">
            <xsl:apply-templates />
        </fo:table-row>
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


    <xsl:template match="table[@classes = 'long']/tgroup/tbody/row/entry/paragraph">
        <fo:block xsl:use-attribute-sets="long-cell-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="rubric[@classes = 'caption-non-first-page']"/>


</xsl:stylesheet> 
