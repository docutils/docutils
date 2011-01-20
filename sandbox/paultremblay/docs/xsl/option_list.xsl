<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <xsl:template match="comment()">
        <xsl:if test="contains(., '$Id: option_list.xsl')">

<xsl:text>
option list as list
======================

Since an option list can be rendered as either a traditonal list, or a
definition list, there are two sets of attribute sets. These attribute sets
are used for the options list when it is rendered as a list.

</xsl:text>

        </xsl:if>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='option-list-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">list-block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list</xsl:with-param> 
        </xsl:call-template>
<xsl:text>For the option list. Since this element contains all the other list
elements, it can be used to set values such as the font, background color,
line-height, etc, for the entire list, as well as the space after and
before.

"The provisional-distance-between-starts property of the list-block
specifies the distance bewteen the start of the label (the bullet, for
example) and the actual start of the list content" (Pawson, 100)

</xsl:text>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='option-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item</xsl:with-param> 
        </xsl:call-template>
<xsl:text>For the items in the option list. The attributes can control the
spacing between each item. A different set of attributes controls the spacing
of the first item (see below).

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-first-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item[1]</xsl:with-param> 
        </xsl:call-template>
<xsl:text>For the first item in the option list. This attribute set inherits
all the properties form 'option-list-item', and then re-defines the
space-before to 0pt. In order to get space between the first item and the
text before it, use the space-after attribute in the option-list attribute
set.

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-item-label']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-label</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/option_group</xsl:with-param> 
        </xsl:call-template>
<xsl:text>The deault attribute end-indent = "label-end()" ensures
that the label aligns properly.

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-item-label-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/option_group/option_string|option_argument</xsl:with-param> 
        </xsl:call-template>
<xsl:text>These attributes format the block that wraps the option_string and 
option_argument.

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/option_group/option/option_string</xsl:with-param> 
        </xsl:call-template>
<xsl:text>Used to format any inline properties of the option_string.

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-argument-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/option_group/option/option_argument</xsl:with-param> 
        </xsl:call-template>
<xsl:text>Used to format any inline properties of the option_string.

</xsl:text>
    </xsl:template>



    <xsl:template match= "xsl:attribute-set[@name='option-list-item-body']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-body</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/description</xsl:with-param> 
        </xsl:call-template>

<xsl:text>The default of start-indent = "body-start()" ensures the correct 
alignment of the labels.

</xsl:text>

    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-item-body-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/description/paragraph</xsl:with-param> 
        </xsl:call-template>
<xsl:text>Formats the blocks (docutilis paragraphs) that describe the 
options. If there was more than one paragraph, you could use attributes
such as space after.

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-definition-block']" priority="3">
<xsl:text>

option list as definition list
================================

These attribute sets are used for the options list when it is rendered as a
definition list. (See the docutils reference guide for an example of a 
definition list, or see the defintion list in the test files.)

</xsl:text>
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list</xsl:with-param> 
        </xsl:call-template>
<xsl:text>Formats the block that wraps the other blocks. Use to control 
space after and before, or to set any block items on the entire list. 

This block wraps around another block, which in turn wraps around a third
block.

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-item-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item</xsl:with-param> 
        </xsl:call-template>
<xsl:text>Formats the block that wraps the options and descriptions, which are also blocks. 

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-item-first-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item</xsl:with-param> 
        </xsl:call-template>
<xsl:text>The same as the option-list-definition-item-block, except these attributes
format the first item. By default, this attribute set inherits the attributes from
the option-list-definition-item-block, and then re-defines the space-before to 0pt.

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='template']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">document</xsl:with-param> 
        </xsl:call-template>
<xsl:text>

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-group-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list_item/option_list_item/option_group</xsl:with-param> 
        </xsl:call-template>
<xsl:text>Formats the block that contains the inline elements of the options and arguments.
For a defintion list, this block serves as the term, and sits on top, and to the left 
of the description.

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-description-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/description/paragraph</xsl:with-param> 
        </xsl:call-template>
<xsl:text>Formats the paragraphs describing the options or arguments. This groups of blocks sits below the 
blocks formatting the options and arguments, and in a defintion list are usually indented right.

</xsl:text>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='option-list-description-first-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">option_list/option_list_item/description/paragraph</xsl:with-param> 
        </xsl:call-template>
        <xsl:text>Same as the option-list-description-block, from which it inherits all 
of its attibutes. By default, it redfines the space-before as 0pt.

</xsl:text>
    </xsl:template>

</xsl:stylesheet>
