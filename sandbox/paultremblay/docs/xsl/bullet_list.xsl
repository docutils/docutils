<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->



    <xsl:template match= "xsl:attribute-set[@name='bullet-list-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">list-block</xsl:with-param> 
            <xsl:with-param name="docutils">bullet_list</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the bullet list. Since this element contains all the other list
            elements, it can be used to set values such as the font, background color,
            line-height, etc, for the entire list, as well as the space after and
            before.
        </block>
        <block>
            "The provisional-distance-between-starts property of the list-block
            specifies the distance bewteen the start of the label (the bullet, for
            example) and the actual start of the list content" (Pawson, 100)
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='bullet-level2-list-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">list-block</xsl:with-param> 
            <xsl:with-param name="docutils">bullet_list/bullet_list</xsl:with-param> 
        </xsl:call-template>
        <block>
            Same as for the bullet-list-block attribute. The default sets the 
            start-indent property to a greater value to indent this nested
            list.
        </block>

    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='bullet-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">bullet_list/list_item</xsl:with-param> 
        </xsl:call-template>
        <block>
        For the item in the bullet list. The attributes can control the
        spacing between each item. A different set of attributes controls the spacing
        of the first item (see below).
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='bullet-level2-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">bullet_list/bullet_list/list_item</xsl:with-param> 
        </xsl:call-template>
        <block>
        Same as above, except for a nested bullet list.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='bullet-first-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">bullet_list/list_item[1]</xsl:with-param> 
            <xsl:with-param name="inherits">bullet-list-item</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the first item in the bullet list. This attribute set inherits
            all the properties form 'bullet-list-item', and then re-defines the
            space-before to 0pt. In order to get space between the first item and the
            text before it, use the space-after attribute in the bullet-list attribute
            set.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='bullet-level2-first-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">bullet_list/list_item[1]</xsl:with-param> 
            <xsl:with-param name="inherits">bullet-level2-list-item</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the first item in a nested bullet list. This attribute set inherits
            all the properties form 'bullet-list-item', and then re-defines the
            space-before to 0pt. In order to get space between the first item and the
            text before it, use the space-after attribute in the bullet-list attribute
            set.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='bullet-list-item-label']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-label</xsl:with-param> 
            <xsl:with-param name="docutils">bullet_list/list_item</xsl:with-param> 
        </xsl:call-template>
        <block>
            The default attribute end-indent = "label-end()" ensures
            that the label aligns properly.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='bullet-list-item-label-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">bullet_list/list_item</xsl:with-param> 
        </xsl:call-template>
        <block>
            These attributes format the block that wraps the bullet. (FO requires such
            a block, even for a small label like this.)
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='bullet-list-item-body']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-body</xsl:with-param> 
            <xsl:with-param name="docutils">bullet_list/list_item</xsl:with-param> 
        </xsl:call-template>
        <block>
            The default of start-indent = "body-start()" ensures the correct 
            alignment of the labels.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='bullet-list-item-body-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">bullet_list/list_item/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the blocks (docutilis paragraphs) of the body of each item.
        </block>
    </xsl:template>

</xsl:stylesheet>
