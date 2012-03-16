<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:import href="http://docutils.svn.sourceforge.net/viewvc/docutils/trunk/sandbox/docbook/xsl/docutils_to_docbook.xsl"/>

    <xsl:template match="inline[@classes='my-note']"/>

    <xsl:template name="colspec-align">
        <xsl:param name="classes"/>
        <xsl:param name="position"/>
        <xsl:choose >
            <xsl:when test="$classes='receive-hours' and $position > 1">
                <xsl:text >right</xsl:text>
            </xsl:when>
            <xsl:when test="$classes='cutover-costs' and $position > 1">
                <xsl:text >right</xsl:text>
            </xsl:when>
            <xsl:when test="$classes='projected-summary' and $position > 1">
                <xsl:text >right</xsl:text>
            </xsl:when>
            <xsl:when test="$classes='projected-costs' and $position > 1">
                <xsl:text >right</xsl:text>
            </xsl:when>
            <xsl:when test="$classes='projected-exceptions' and $position > 1">
                <xsl:text >right</xsl:text>
            </xsl:when>
            <xsl:when test="$classes='projected-exceptions' and $position > 1">
                <xsl:text >right</xsl:text>
            </xsl:when>
            <xsl:when test="$classes='long-metrics' and $position > 2">
                <xsl:text >right</xsl:text>
            </xsl:when>
            <xsl:otherwise >
                <xsl:text >left</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="table.rowsep">
        <xsl:text >0</xsl:text>
    </xsl:template>

    <xsl:template name="table.colsep" >
        <xsl:text >0</xsl:text>
    </xsl:template>


    <xsl:template name="tabstyle">
        <xsl:param name="classes"/>
        <xsl:choose >
            <xsl:when test="$classes = 'problem-solving'">
                <xsl:text >long</xsl:text>
            </xsl:when>
            <xsl:otherwise >
                <xsl:text >default</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes = 'overall-costs']/tgroup/tbody/row[last()]" priority="2"/>
    <xsl:template match="table[@classes = 'hardware-costs']/tgroup/tbody/row[last()]" priority="2"/>

    <xsl:template name="make-table-foot">
        <xsl:param name="classes"/>
        <xsl:choose >
            <xsl:when test="$classes = 'overall-costs' or $classes = 'hardware-costs'">
                <xsl:apply-templates select="../tbody/row[last()]"
                    mode="foot"/>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes = 'overall-costs']/tgroup/tbody/row[last()]|
        table[@classes = 'hardware-costs']/tgroup/tbody/row[last()]" 
        mode="foot">
        <d:tfoot >
            <d:row rowsep="1">
                <xsl:apply-templates/>
            </d:row>
        </d:tfoot>
    </xsl:template>

    <xsl:template name="make-table-width">
        <xsl:param name="classes"/>
        <xsl:choose >
            <xsl:when test="$classes = 'overall-costs'">
                <xsl:processing-instruction name="dbfo">
                    <xsl:text >table-width="50%"</xsl:text>
                </xsl:processing-instruction> 
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="table-keep-together">
        <xsl:param name="classes"/>
        <xsl:choose >
            <xsl:when test="$classes='long-metrics'">
                <xsl:processing-instruction
                    name="dbfo">
                    <xsl:text >keep-together="auto" </xsl:text>
                </xsl:processing-instruction>
            </xsl:when>
        </xsl:choose>
    </xsl:template>


    <xsl:template name="page-break-before-section">
        <xsl:param name="classes"/>
        <xsl:param name="id"/>
        <xsl:if test="@ids = 'tables-of-rates-of-productivity-pre-and-post-cutover'">
            <xsl:processing-instruction name="hard-pagebreak"/>
        </xsl:if>
    </xsl:template>
    
</xsl:stylesheet>
