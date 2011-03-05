<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='long-table']" priority="3">
        <xsl:call-template name="make-title">
            <xsl:with-param name="level">5</xsl:with-param>
            <xsl:with-param name="text">Long Tables</xsl:with-param>
        </xsl:call-template>
        <block>
            <xsl:text>:fo: See below</xsl:text> 
        </block>
        <block first-line-indent="-9">
            <xsl:text>:docutils: See below</xsl:text> 
        </block>
        <block>

            The table_long.xsl contains templates for a long table. Each attribute set has 
            has the same function as its corresponding attribute set in the regular
            table, as documented above. There is no block-container attriute set, because
            there is no block-container element; the caption is rendered as part of the 
            header or footer.

        </block>

        <block>* long-table => table</block>
        <block>* long-table-header => table-header</block>
        <block>* long-table-header-row => table-header-row</block>
        <block>* long-thead-cell => thead-cell</block>
        <block>* long-table-header-block => table-header-block</block>
        <block>* long-table-body => table-body</block>
        <block>* long-table-header-row => table-header-row</block>
        <block>* long-table-row => table-row</block>
        <block>* long-table-cell => table-cell</block>
        <block>* long-cell-block => cell-block</block>
        <block>* long-caption-block => caption-block</block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='long-table-header']|
        xsl:attribute-set[@name='long-table-header-row']|
        xsl:attribute-set[@name='long-table-header-cell']|
        xsl:attribute-set[@name='long-table-header-block']|
        xsl:attribute-set[@name='long-table-body']|
        xsl:attribute-set[@name='long-table-row']|
        xsl:attribute-set[@name='long-table-cell']|
        xsl:attribute-set[@name='long-cell-block']|
        xsl:attribute-set[@name='long-caption-block']" priority="3"/>



</xsl:stylesheet>
