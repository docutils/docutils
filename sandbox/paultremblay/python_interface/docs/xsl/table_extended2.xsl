<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='borderless-table-block-container']" priority="3">
        <xsl:call-template name="make-title">
            <xsl:with-param name="level">5</xsl:with-param>
            <xsl:with-param name="text">Custom Tables</xsl:with-param>
        </xsl:call-template>
        <block>
            <xsl:text>:fo: See below</xsl:text> 
        </block>
        <block first-line-indent="-9">
            <xsl:text>:docutils: See below</xsl:text> 
        </block>
        <block>

            The table_extend2.xsl contains templates for the borderless table. Each attribute set has 
            has the same function as its corresponding attribute set in the regular
            table, as documented above.

        </block>

        <block>* borderless-table-block-container => table-block-contanter</block>
        <block>* borderless-table => table</block>
        <block>* borderless-table-header => table-header</block>
        <block>* borderless-table-header-cell => table-header-cell</block>
        <block>* borderless-table-header-block => table-header-block</block>
        <block>* borderless-table-body => table-body</block>
        <block>* borderless-table-header-row => table-header-row</block>
        <block>* borderless-table-row => table-row</block>
        <block>* borderless-table-cell => table-cell</block>
        <block>* borderless-cell-block => cell-block</block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='borderless-table']|
        xsl:attribute-set[@name='borderless-table-header']|
        xsl:attribute-set[@name='borderless-table-header-row']|
        xsl:attribute-set[@name='borderless-table-header-cell']|
        xsl:attribute-set[@name='borderless-table-header-block']|
        xsl:attribute-set[@name='borderless-table-body']|
        xsl:attribute-set[@name='borderless-table-row']|
        xsl:attribute-set[@name='borderless-table-cell']|
        xsl:attribute-set[@name='borderless-cell-block']" priority="3"/>



</xsl:stylesheet>
