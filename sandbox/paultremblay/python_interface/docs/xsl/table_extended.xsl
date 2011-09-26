<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='table1-block-container']" priority="3">
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

            The table_extend.xsl contains templates to match 30 custom tables, 
            and and attribute sets for each of these tables. Each attribute set has 
            has the same function as its corresponding attribute set in the regular
            table, as documented above.

        </block>

        <block>* table1-block-container => table-block-contanter</block>
        <block>* table1 => table</block>
        <block>* table1-header => table-header</block>
        <block>* table1-header-cell => table-header-cell</block>
        <block>* table1-header-block => table-header-block</block>
        <block>* table1-body => table-body</block>
        <block>* table1-header-row => table-header-row</block>
        <block>* table1-row => table-row</block>
        <block>* table1-cell => table-cell</block>
        <block>* cell1-block => cell-block</block>
        <block>* table2-block-container => table-block-contanter</block>
        <block>* table2 => table</block>
        <block>* ... </block>
        <block>* table2-row => table-row</block>
        <block>* table2-cell => table-cell</block>
        <block>* cell2-block => cell-block</block>
        <block>* ... </block>
        <block>* table30-block-container => table-block-contanter</block>
        <block>* table30 => table</block>
        <block>* table30-header => table-header</block>
        <block>* table30-header-cell => table-header-cell</block>
        <block>* table30-header-block => table-header-block</block>
        <block>* table30-body => table-body</block>
        <block>* table30-header-row => table-header-row</block>
        <block>* table30-row => table-row</block>
        <block>* table30-cell => table-cell</block>
        <block>* cell30-block => cell-block</block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='table2-block-container']|
     xsl:attribute-set[@name='table3-block-container']|
     xsl:attribute-set[@name='table4-block-container']|
     xsl:attribute-set[@name='table5-block-container']|
     xsl:attribute-set[@name='table6-block-container']|
     xsl:attribute-set[@name='table7-block-container']|
     xsl:attribute-set[@name='table8-block-container']|
     xsl:attribute-set[@name='table9-block-container']|
     xsl:attribute-set[@name='table10-block-container']|
     xsl:attribute-set[@name='table11-block-container']|
     xsl:attribute-set[@name='table12-block-container']|
     xsl:attribute-set[@name='table13-block-container']|
     xsl:attribute-set[@name='table14-block-container']|
     xsl:attribute-set[@name='table15-block-container']|
     xsl:attribute-set[@name='table16-block-container']|
     xsl:attribute-set[@name='table17-block-container']|
     xsl:attribute-set[@name='table18-block-container']|
     xsl:attribute-set[@name='table19-block-container']|
     xsl:attribute-set[@name='table20-block-container']|
     xsl:attribute-set[@name='table21-block-container']|
     xsl:attribute-set[@name='table22-block-container']|
     xsl:attribute-set[@name='table23-block-container']|
     xsl:attribute-set[@name='table24-block-container']|
     xsl:attribute-set[@name='table25-block-container']|
     xsl:attribute-set[@name='table26-block-container']|
     xsl:attribute-set[@name='table27-block-container']|
     xsl:attribute-set[@name='table28-block-container']|
     xsl:attribute-set[@name='table29-block-container']|
     xsl:attribute-set[@name='table30-block-container']" priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='table1']|
     xsl:attribute-set[@name='table2']|
     xsl:attribute-set[@name='table3']|
     xsl:attribute-set[@name='table4']|
     xsl:attribute-set[@name='table5']|
     xsl:attribute-set[@name='table6']|
     xsl:attribute-set[@name='table7']|
     xsl:attribute-set[@name='table8']|
     xsl:attribute-set[@name='table9']|
     xsl:attribute-set[@name='table10']|
     xsl:attribute-set[@name='table11']|
     xsl:attribute-set[@name='table12']|
     xsl:attribute-set[@name='table13']|
     xsl:attribute-set[@name='table14']|
     xsl:attribute-set[@name='table15']|
     xsl:attribute-set[@name='table16']|
     xsl:attribute-set[@name='table17']|
     xsl:attribute-set[@name='table18']|
     xsl:attribute-set[@name='table19']|
     xsl:attribute-set[@name='table20']|
     xsl:attribute-set[@name='table21']|
     xsl:attribute-set[@name='table22']|
     xsl:attribute-set[@name='table23']|
     xsl:attribute-set[@name='table24']|
     xsl:attribute-set[@name='table25']|
     xsl:attribute-set[@name='table26']|
     xsl:attribute-set[@name='table27']|
     xsl:attribute-set[@name='table28']|
     xsl:attribute-set[@name='table29']|
     xsl:attribute-set[@name='table30']" priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='table1-header']|
     xsl:attribute-set[@name='table2-header']|
     xsl:attribute-set[@name='table3-header']|
     xsl:attribute-set[@name='table4-header']|
     xsl:attribute-set[@name='table5-header']|
     xsl:attribute-set[@name='table6-header']|
     xsl:attribute-set[@name='table7-header']|
     xsl:attribute-set[@name='table8-header']|
     xsl:attribute-set[@name='table9-header']|
     xsl:attribute-set[@name='table10-header']|
     xsl:attribute-set[@name='table11-header']|
     xsl:attribute-set[@name='table12-header']|
     xsl:attribute-set[@name='table13-header']|
     xsl:attribute-set[@name='table14-header']|
     xsl:attribute-set[@name='table15-header']|
     xsl:attribute-set[@name='table16-header']|
     xsl:attribute-set[@name='table17-header']|
     xsl:attribute-set[@name='table18-header']|
     xsl:attribute-set[@name='table19-header']|
     xsl:attribute-set[@name='table20-header']|
     xsl:attribute-set[@name='table21-header']|
     xsl:attribute-set[@name='table22-header']|
     xsl:attribute-set[@name='table23-header']|
     xsl:attribute-set[@name='table24-header']|
     xsl:attribute-set[@name='table25-header']|
     xsl:attribute-set[@name='table26-header']|
     xsl:attribute-set[@name='table27-header']|
     xsl:attribute-set[@name='table28-header']|
     xsl:attribute-set[@name='table29-header']|
     xsl:attribute-set[@name='table30-header']" priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='table1-header-cell']|
     xsl:attribute-set[@name='table2-header-cell']|
     xsl:attribute-set[@name='table3-header-cell']|
     xsl:attribute-set[@name='table4-header-cell']|
     xsl:attribute-set[@name='table5-header-cell']|
     xsl:attribute-set[@name='table6-header-cell']|
     xsl:attribute-set[@name='table7-header-cell']|
     xsl:attribute-set[@name='table8-header-cell']|
     xsl:attribute-set[@name='table9-header-cell']|
     xsl:attribute-set[@name='table10-header-cell']|
     xsl:attribute-set[@name='table11-header-cell']|
     xsl:attribute-set[@name='table12-header-cell']|
     xsl:attribute-set[@name='table13-header-cell']|
     xsl:attribute-set[@name='table14-header-cell']|
     xsl:attribute-set[@name='table15-header-cell']|
     xsl:attribute-set[@name='table16-header-cell']|
     xsl:attribute-set[@name='table17-header-cell']|
     xsl:attribute-set[@name='table18-header-cell']|
     xsl:attribute-set[@name='table19-header-cell']|
     xsl:attribute-set[@name='table20-header-cell']|
     xsl:attribute-set[@name='table21-header-cell']|
     xsl:attribute-set[@name='table22-header-cell']|
     xsl:attribute-set[@name='table23-header-cell']|
     xsl:attribute-set[@name='table24-header-cell']|
     xsl:attribute-set[@name='table25-header-cell']|
     xsl:attribute-set[@name='table26-header-cell']|
     xsl:attribute-set[@name='table27-header-cell']|
     xsl:attribute-set[@name='table28-header-cell']|
     xsl:attribute-set[@name='table29-header-cell']|
     xsl:attribute-set[@name='table30-header-cell']" priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='table1-header-block']|
     xsl:attribute-set[@name='table2-header-block']|
     xsl:attribute-set[@name='table3-header-block']|
     xsl:attribute-set[@name='table4-header-block']|
     xsl:attribute-set[@name='table5-header-block']|
     xsl:attribute-set[@name='table6-header-block']|
     xsl:attribute-set[@name='table7-header-block']|
     xsl:attribute-set[@name='table8-header-block']|
     xsl:attribute-set[@name='table9-header-block']|
     xsl:attribute-set[@name='table10-header-block']|
     xsl:attribute-set[@name='table11-header-block']|
     xsl:attribute-set[@name='table12-header-block']|
     xsl:attribute-set[@name='table13-header-block']|
     xsl:attribute-set[@name='table14-header-block']|
     xsl:attribute-set[@name='table15-header-block']|
     xsl:attribute-set[@name='table16-header-block']|
     xsl:attribute-set[@name='table17-header-block']|
     xsl:attribute-set[@name='table18-header-block']|
     xsl:attribute-set[@name='table19-header-block']|
     xsl:attribute-set[@name='table20-header-block']|
     xsl:attribute-set[@name='table21-header-block']|
     xsl:attribute-set[@name='table22-header-block']|
     xsl:attribute-set[@name='table23-header-block']|
     xsl:attribute-set[@name='table24-header-block']|
     xsl:attribute-set[@name='table25-header-block']|
     xsl:attribute-set[@name='table26-header-block']|
     xsl:attribute-set[@name='table27-header-block']|
     xsl:attribute-set[@name='table28-header-block']|
     xsl:attribute-set[@name='table29-header-block']|
     xsl:attribute-set[@name='table30-header-block']" priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='table1-body']|
     xsl:attribute-set[@name='table2-body']|
     xsl:attribute-set[@name='table3-body']|
     xsl:attribute-set[@name='table4-body']|
     xsl:attribute-set[@name='table5-body']|
     xsl:attribute-set[@name='table6-body']|
     xsl:attribute-set[@name='table7-body']|
     xsl:attribute-set[@name='table8-body']|
     xsl:attribute-set[@name='table9-body']|
     xsl:attribute-set[@name='table10-body']|
     xsl:attribute-set[@name='table11-body']|
     xsl:attribute-set[@name='table12-body']|
     xsl:attribute-set[@name='table13-body']|
     xsl:attribute-set[@name='table14-body']|
     xsl:attribute-set[@name='table15-body']|
     xsl:attribute-set[@name='table16-body']|
     xsl:attribute-set[@name='table17-body']|
     xsl:attribute-set[@name='table18-body']|
     xsl:attribute-set[@name='table19-body']|
     xsl:attribute-set[@name='table20-body']|
     xsl:attribute-set[@name='table21-body']|
     xsl:attribute-set[@name='table22-body']|
     xsl:attribute-set[@name='table23-body']|
     xsl:attribute-set[@name='table24-body']|
     xsl:attribute-set[@name='table25-body']|
     xsl:attribute-set[@name='table26-body']|
     xsl:attribute-set[@name='table27-body']|
     xsl:attribute-set[@name='table28-body']|
     xsl:attribute-set[@name='table29-body']|
     xsl:attribute-set[@name='table30-body']" priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='table1-header-row']|
     xsl:attribute-set[@name='table2-header-row']|
     xsl:attribute-set[@name='table3-header-row']|
     xsl:attribute-set[@name='table4-header-row']|
     xsl:attribute-set[@name='table5-header-row']|
     xsl:attribute-set[@name='table6-header-row']|
     xsl:attribute-set[@name='table7-header-row']|
     xsl:attribute-set[@name='table8-header-row']|
     xsl:attribute-set[@name='table9-header-row']|
     xsl:attribute-set[@name='table10-header-row']|
     xsl:attribute-set[@name='table11-header-row']|
     xsl:attribute-set[@name='table12-header-row']|
     xsl:attribute-set[@name='table13-header-row']|
     xsl:attribute-set[@name='table14-header-row']|
     xsl:attribute-set[@name='table15-header-row']|
     xsl:attribute-set[@name='table16-header-row']|
     xsl:attribute-set[@name='table17-header-row']|
     xsl:attribute-set[@name='table18-header-row']|
     xsl:attribute-set[@name='table19-header-row']|
     xsl:attribute-set[@name='table20-header-row']|
     xsl:attribute-set[@name='table21-header-row']|
     xsl:attribute-set[@name='table22-header-row']|
     xsl:attribute-set[@name='table23-header-row']|
     xsl:attribute-set[@name='table24-header-row']|
     xsl:attribute-set[@name='table25-header-row']|
     xsl:attribute-set[@name='table26-header-row']|
     xsl:attribute-set[@name='table27-header-row']|
     xsl:attribute-set[@name='table28-header-row']|
     xsl:attribute-set[@name='table29-header-row']|
     xsl:attribute-set[@name='table30-header-row']" priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='table1-row']|
     xsl:attribute-set[@name='table2-row']|
     xsl:attribute-set[@name='table3-row']|
     xsl:attribute-set[@name='table4-row']|
     xsl:attribute-set[@name='table5-row']|
     xsl:attribute-set[@name='table6-row']|
     xsl:attribute-set[@name='table7-row']|
     xsl:attribute-set[@name='table8-row']|
     xsl:attribute-set[@name='table9-row']|
     xsl:attribute-set[@name='table10-row']|
     xsl:attribute-set[@name='table11-row']|
     xsl:attribute-set[@name='table12-row']|
     xsl:attribute-set[@name='table13-row']|
     xsl:attribute-set[@name='table14-row']|
     xsl:attribute-set[@name='table15-row']|
     xsl:attribute-set[@name='table16-row']|
     xsl:attribute-set[@name='table17-row']|
     xsl:attribute-set[@name='table18-row']|
     xsl:attribute-set[@name='table19-row']|
     xsl:attribute-set[@name='table20-row']|
     xsl:attribute-set[@name='table21-row']|
     xsl:attribute-set[@name='table22-row']|
     xsl:attribute-set[@name='table23-row']|
     xsl:attribute-set[@name='table24-row']|
     xsl:attribute-set[@name='table25-row']|
     xsl:attribute-set[@name='table26-row']|
     xsl:attribute-set[@name='table27-row']|
     xsl:attribute-set[@name='table28-row']|
     xsl:attribute-set[@name='table29-row']|
     xsl:attribute-set[@name='table30-row']" priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='table1-cell']|
     xsl:attribute-set[@name='table2-cell']|
     xsl:attribute-set[@name='table3-cell']|
     xsl:attribute-set[@name='table4-cell']|
     xsl:attribute-set[@name='table5-cell']|
     xsl:attribute-set[@name='table6-cell']|
     xsl:attribute-set[@name='table7-cell']|
     xsl:attribute-set[@name='table8-cell']|
     xsl:attribute-set[@name='table9-cell']|
     xsl:attribute-set[@name='table10-cell']|
     xsl:attribute-set[@name='table11-cell']|
     xsl:attribute-set[@name='table12-cell']|
     xsl:attribute-set[@name='table13-cell']|
     xsl:attribute-set[@name='table14-cell']|
     xsl:attribute-set[@name='table15-cell']|
     xsl:attribute-set[@name='table16-cell']|
     xsl:attribute-set[@name='table17-cell']|
     xsl:attribute-set[@name='table18-cell']|
     xsl:attribute-set[@name='table19-cell']|
     xsl:attribute-set[@name='table20-cell']|
     xsl:attribute-set[@name='table21-cell']|
     xsl:attribute-set[@name='table22-cell']|
     xsl:attribute-set[@name='table23-cell']|
     xsl:attribute-set[@name='table24-cell']|
     xsl:attribute-set[@name='table25-cell']|
     xsl:attribute-set[@name='table26-cell']|
     xsl:attribute-set[@name='table27-cell']|
     xsl:attribute-set[@name='table28-cell']|
     xsl:attribute-set[@name='table29-cell']|
     xsl:attribute-set[@name='table30-cell']" priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='cell1-block']|
     xsl:attribute-set[@name='cell2-block']|
     xsl:attribute-set[@name='cell3-block']|
     xsl:attribute-set[@name='cell4-block']|
     xsl:attribute-set[@name='cell5-block']|
     xsl:attribute-set[@name='cell6-block']|
     xsl:attribute-set[@name='cell7-block']|
     xsl:attribute-set[@name='cell8-block']|
     xsl:attribute-set[@name='cell9-block']|
     xsl:attribute-set[@name='cell10-block']|
     xsl:attribute-set[@name='cell11-block']|
     xsl:attribute-set[@name='cell12-block']|
     xsl:attribute-set[@name='cell13-block']|
     xsl:attribute-set[@name='cell14-block']|
     xsl:attribute-set[@name='cell15-block']|
     xsl:attribute-set[@name='cell16-block']|
     xsl:attribute-set[@name='cell17-block']|
     xsl:attribute-set[@name='cell18-block']|
     xsl:attribute-set[@name='cell19-block']|
     xsl:attribute-set[@name='cell20-block']|
     xsl:attribute-set[@name='cell21-block']|
     xsl:attribute-set[@name='cell22-block']|
     xsl:attribute-set[@name='cell23-block']|
     xsl:attribute-set[@name='cell24-block']|
     xsl:attribute-set[@name='cell25-block']|
     xsl:attribute-set[@name='cell26-block']|
     xsl:attribute-set[@name='cell27-block']|
     xsl:attribute-set[@name='cell28-block']|
     xsl:attribute-set[@name='cell29-block']|
     xsl:attribute-set[@name='cell30-block']" priority="3"/>


</xsl:stylesheet>
