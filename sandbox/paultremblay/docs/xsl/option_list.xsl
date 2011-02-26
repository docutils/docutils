<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->



    <xsl:template match= "xsl:attribute-set[@name='option-list-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">list-block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the option list. Since this element contains all the other list
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


    <xsl:template match= "xsl:attribute-set[@name='option-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item</xsl:with-param> 
        </xsl:call-template>
        <block>
        For the items in the option list. The attributes can control the
        spacing between each item. A different set of attributes controls the spacing
        of the first item (see below).
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-first-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item[1]</xsl:with-param> 
            <xsl:with-param name="inherits">option-list-item</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the first item in the option list. This attribute set inherits
            all the properties form 'option-list-item', and then re-defines the
            space-before to 0pt. In order to get space between the first item and the
            text before it, use the space-after attribute in the option-list attribute
            set.
        </block>
        <block>
            It does not make sense to change this attriubte set directly.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-item-label']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-label</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/option_group</xsl:with-param> 
        </xsl:call-template>
        <block>
            The default attribute end-indent = "label-end()" ensures
            that the label aligns properly.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-item-label-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/option_group/option_string|option_argument</xsl:with-param> 
        </xsl:call-template>
        <block>
            These attributes format the block that wraps the option_string and 
            option_argument.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/option_group/option/option_string</xsl:with-param> 
        </xsl:call-template>
        <block>
            Used to format any inline properties of the option_string.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-argument-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/option_group/option/option_argument</xsl:with-param> 
        </xsl:call-template>
        <block>
        Used to format any inline properties of the option_string.
        </block>
    </xsl:template>



    <xsl:template match= "xsl:attribute-set[@name='option-list-item-body']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-body</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/description</xsl:with-param> 
        </xsl:call-template>
        <block>
            The default of start-indent = "body-start()" ensures the correct 
            alignment of the labels.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-item-body-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/description/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the blocks (docutilis paragraphs) that describe the 
            options. If there was more than one paragraph, you could use attributes
            such as space after.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-definition-block']" priority="3">
        <xsl:call-template name="make-title">
            <xsl:with-param name="level">3</xsl:with-param>
            <xsl:with-param name="text">option list as definition list</xsl:with-param>
        </xsl:call-template>
        <block>
            These attribute sets are used for the options list when it is rendered as a
            definition list. (See the docutils reference guide for an example of a 
            definition list, or see the defintion list in the test files.)
        </block>
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block that wraps the other blocks. Use to control 
            space after and before, or to set any block items on the entire list. 
        </block>
        <block>
        This block wraps around another block, which in turn wraps around a third
        block.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-item-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block that wraps the options and descriptions, which are also blocks. 
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-first-item-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item</xsl:with-param> 
        </xsl:call-template>
        <block>
            Same as for option-list-item-block, but sets the space-before to 0pt 
        </block>
        <block>
            Does not make sense to change the attributes here directly.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-item-first-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item</xsl:with-param> 
            <xsl:with-param name="inherits">option-list-definition-item-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            The same as the option-list-definition-item-block, except these attributes
            format the first item. By default, this attribute set inherits the attributes from
            the option-list-definition-item-block, and then re-defines the space-before to 0pt.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='option-group-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list_item/option_list_item/option_group</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block that contains the inline elements of the options and arguments.
            For a defintion list, this block serves as the term, and sits on top, and to the left 
            of the description.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-description-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/description</xsl:with-param> 
        </xsl:call-template>
        <block>

            Formats the blocks wrappring the paragraphs describing the options
            or arguments. This groups of blocks sits below the blocks
            formatting the options and arguments, and in a defintion list are
            usually indented right.

        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-description-first-block']"
        priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/description[1]</xsl:with-param> 
            <xsl:with-param name="inherits">option-list-description-block</xsl:with-param> 
        </xsl:call-template>
        <block>

            Same as above, except formats the first such element.

        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/description/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
        Formats the paragraphs in the description for an options list formatted as a definition list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/description/paragraph</xsl:with-param> 
            <xsl:with-param name="inherits">option-list-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
        Formats the first paragraph in the description for an options list formatted as a definition list.
        </block>
    </xsl:template>

</xsl:stylesheet>
