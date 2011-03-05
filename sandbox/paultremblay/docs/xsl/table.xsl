<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='table-block-container']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block-container</xsl:with-param> 
            <xsl:with-param name="docutils">table</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block container that wraps bothe the table and a the table title (captin)
            if one exists. Use to control space before and after the table.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='table']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:table</xsl:with-param> 
            <xsl:with-param name="docutils">table</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the table.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='table-header']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:table-header</xsl:with-param> 
            <xsl:with-param name="docutils">tgroup/thead</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the header of the table.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='default-cell']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:cell</xsl:with-param> 
            <xsl:with-param name="docutils">None</xsl:with-param> 
        </xsl:call-template>
        <block>
            Sets the defaults for all cells.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='table-header-cell']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:cell</xsl:with-param> 
            <xsl:with-param name="docutils">thead/row/entry</xsl:with-param> 
            <xsl:with-param name="inherits">default-cell</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the cells in the table header.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='thead-borderless-cell_old']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:cell</xsl:with-param> 
            <xsl:with-param name="docutils">thead/row/entry</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the cells in the table header for a borderless table.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='table-body']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:table-body</xsl:with-param> 
            <xsl:with-param name="docutils">tbody</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attributes for the table body.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='table-row']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:table-row</xsl:with-param> 
            <xsl:with-param name="docutils">tbody/row</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attributes for the table row.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='table-cell']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:table-cell</xsl:with-param> 
            <xsl:with-param name="docutils">tbody/row/entry</xsl:with-param> 
            <xsl:with-param name="inherits">default-cell</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attributes for the table cell.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='table-cell-borderless_old']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:table-cell</xsl:with-param> 
            <xsl:with-param name="docutils">tbody/row/entry</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attributes for the table cell for borderless table.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='cell-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">tbody/row/entry/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attributes for the paragraphs in the cell.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='table-header-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">thead/row/entry/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attributes for the paragraphs in the header cell.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='caption-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">table/title</xsl:with-param> 
        </xsl:call-template>
        <block>
            Attributes for the table title, or caption. The parameter 'table-title-placement', 
            controls whether this block is placed before or after the table. If it is placed 
            on top of the table, it has a 'keep-with-next="always"' value that cannot be changed.
            If this block is placed on the bottom it has a 'keep-with-previous="always"' value
            that cannot be changed.
        </block>
    </xsl:template>

</xsl:stylesheet>
