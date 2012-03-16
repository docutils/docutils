<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:import
        href="http://docutils.svn.sourceforge.net/viewvc/docutils/trunk/sandbox/docbook/xsl/long_table.xsl"/>

    <xsl:template match="table[@classes='long-metrics']">
        <xsl:variable name="position">
            <xsl:value-of select="count(preceding::table[@classes='long-metrics']) + 1"/>
        </xsl:variable>
        <xsl:variable name="first-page-rows">
            <xsl:choose>
                <xsl:when test="$position  &lt; 1000">
                    <xsl:text>34</xsl:text>
                </xsl:when>
            </xsl:choose>
        </xsl:variable>
        <xsl:if test="$position != 1">
            <xsl:processing-instruction name="hard-pagebreak"/>
        </xsl:if>
        <xsl:choose>
            <xsl:when test="$position = 3">
                <xsl:copy-of select="."/>
                <xsl:apply-templates select="following-sibling::container[1][@classes='caption']" mode="keep"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="split-table">
                    <xsl:with-param name="first-page-rows" select="$first-page-rows"/>
                </xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
</xsl:stylesheet>
