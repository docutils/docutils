<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <!--for the list-block that wraps the whole list-->
    <xsl:attribute-set name="field-list-block" >
        <xsl:attribute name="start-indent">0mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">30mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--Can control space between items-->
    <xsl:attribute-set name="field-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="field-first-list-item" use-attribute-sets="field-list-item">
        <xsl:attribute name="space-before">0pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="field-list-item-label">
        <xsl:attribute name="end-indent">label-end()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="field-list-item-body">
        <xsl:attribute name="start-indent">body-start()</xsl:attribute>
    </xsl:attribute-set>

    <!--element is fo:block. Can control space between
    paragraphs when items has multiple paragraphs-->
    <xsl:attribute-set name="field-body-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="field-list-item-label-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute> 
    </xsl:attribute-set>


    <!--END attribute sets-->




    <xsl:template match="field_list">
        <fo:list-block role="field-list" xsl:use-attribute-sets="field-list-block">
            <xsl:apply-templates/>
        </fo:list-block>
    </xsl:template>

    <xsl:template match="field_list/field">
        <fo:list-item xsl:use-attribute-sets="field-list-item">
            <xsl:apply-templates/>
        </fo:list-item>
    </xsl:template>


    <!--last item, may be different for space-->
    <xsl:template match="field_list/field[1]" priority="2">
        <fo:list-item xsl:use-attribute-sets="field-first-list-item">
            <xsl:apply-templates/>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="field_list/field/field_body/paragraph">
        <fo:block xsl:use-attribute-sets="field-body-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="field_list/field/field_body">
        <fo:list-item-body xsl:use-attribute-sets="field-list-item-body">
            <xsl:apply-templates/>
        </fo:list-item-body>
    </xsl:template>

    <xsl:template match="field_list/field/field_name">
        <fo:list-item-label xsl:use-attribute-sets="field-list-item-label">
            <fo:block xsl:use-attribute-sets="field-list-item-label-block">
                <xsl:apply-templates/>
            </fo:block>
        </fo:list-item-label>
    </xsl:template>


</xsl:stylesheet> 
