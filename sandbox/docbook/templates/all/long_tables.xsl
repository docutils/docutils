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
            <xsl:text>34</xsl:text>
        </xsl:variable>
        <xsl:processing-instruction name="hard-pagebreak"/>
        <xsl:call-template name="split-table">
            <xsl:with-param name="first-page-rows" select="$first-page-rows"/>
        </xsl:call-template>
    </xsl:template>
</xsl:stylesheet>
