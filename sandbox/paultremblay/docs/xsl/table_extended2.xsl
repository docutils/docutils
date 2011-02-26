<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='table-borderless-block-container']" priority="3">
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

        <block>* table-borderless-block-container => table-block-contanter</block>
        <block>* table-borderless => table</block>
        <block>* borderless-thead-header => table-header</block>
        <block>* borderless-thed-cell => thead-cell</block>
        <block>* borderless-thead-block => thead-block</block>
        <block>* table-borderless-body => table-body</block>
        <block>* table-borderless-header-row => table-header-row</block>
        <block>* table-borderless-row => table-row</block>
        <block>* table-borderless-cell => table-cell</block>
        <block>* borderless-cell-block => cell-block</block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='table-borderless']|
        xsl:attribute-set[@name='borderless-thead-header']|
        xsl:attribute-set[@name='table-borderless-header-row']|
        xsl:attribute-set[@name='borderless-thead-cell']|
        xsl:attribute-set[@name='borderless-thead-block']|
        xsl:attribute-set[@name='table-borderless-body']|
        xsl:attribute-set[@name='table-borderless-row']|
        xsl:attribute-set[@name='table-borderless-cell']|
        xsl:attribute-set[@name='borderless-cell-block']" priority="3"/>



</xsl:stylesheet>
