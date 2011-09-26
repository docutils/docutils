<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='field-list-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">list-block</xsl:with-param> 
            <xsl:with-param name="docutils">field_list</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the field list. Since this element contains all the other list
            elements, it can be used to set values such as the font, background color,
            line-height, etc, for the entire list, as well as the space after and
            before.
        </block>
        <block>
            "The provisional-distance-between-starts property of the list-block
            specifies the distance bewteen the start of the label (the bullet, for
            example) and the actual start of the list content" (Pawson, 100).
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='field-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">field_list/field</xsl:with-param> 
        </xsl:call-template>
        <block>
        For the items, or 'fields' in the field list. The attributes can control the
        spacing between each item. A different set of attributes controls the spacing
        of the first item (see below).
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='field-first-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">field_list/field[1]</xsl:with-param> 
            <xsl:with-param name="inherits">field-list-item</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the first item in the field list. This attribute set inherits
            all the properties form 'field-list-item', and then re-defines the
            space-before to 0pt. In order to get space between the first item and the
            text before it, use the space-after attribute in the field-list-block attribute
            set.
        </block>
        <block>
            It does not make sense to change this attriubte set directly.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='field-list-item-label']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-label</xsl:with-param> 
            <xsl:with-param name="docutils">field_list/field/field_name</xsl:with-param> 
        </xsl:call-template>
        <block>
            The default attribute end-indent = "label-end()" ensures
            that the label aligns properly.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='field-list-item-label-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">field_list/field/field_name</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block that wraps the field name.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='field-list-item-body']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-body</xsl:with-param> 
            <xsl:with-param name="docutils">field_list/field/field_body</xsl:with-param> 
        </xsl:call-template>
        <block>
            The default of start-indent = "body-start()" ensures the correct 
            alignment of the labels.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='field-body-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">field_list/field/field_body/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the blocks (docutilis paragraphs) of the field.
        </block>
    </xsl:template>


</xsl:stylesheet>
