<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->



    <xsl:template match= "xsl:attribute-set[@name='definition-list-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">block</xsl:with-param> 
            <xsl:with-param name="docutils">definition_list</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the definition list. Since this element contains all the other
            blocks in the list, it can be used to set values such as
            the font, background color, line-height, etc, for the entire list,
            as well as the space after and before.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='definition-list-item-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">definition_list/definition_list_item</xsl:with-param> 
        </xsl:call-template>
        <block>
        For the items in the definition list. The attributes can control the
        spacing between each item. A different set of attributes controls the spacing
        of the first item (see below).
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='definition-list-item-first-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">definition_list/definition_list_item</xsl:with-param> 
            <xsl:with-param name="inherits">definition-list-item-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the first item in the definition list. This attribute set inherits
            all the properties form 'definition-list-item', and then re-defines the
            space-before to 0pt. In order to get space between the first item and the
            text before it, use the space-after attribute in the option-list attribute
            set.
        </block>
        <block>
            It does not makes sense to change this set direclty.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='definition-term-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">definition_list/definition_list_item/term</xsl:with-param> 
        </xsl:call-template>
        <block>
        Formats the bock of the the term. Can be used to control spacing
        between term and definition, but don't use with space before, or you
        won't be able to control spacing before list
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='definition-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">definition_list/definition_list_item/definition</xsl:with-param> 
        </xsl:call-template>
        <block>
        Formats the bock of the of the defintion, that wraps the paragraph blocks.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='definition-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">definition_list/definition_list_item/definition/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
        Formats the blocks (paragraphs in the defintion. Can be lsed to control
    the space between paragraphs by setting the space-bfore attribute. Don't
    use the space-after attribute, or you won't be able to contorl the spacing
    between items 
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='definition-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">definition_list/definition_list_item/definition/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">definition-first-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the first paragraph in the definition list. This attribute set inherits
            all the properties frorm 'definition-first-paragraph-block', and then re-defines the
            space-before to 0pt. 
        </block>
        <block>
            It does not makes sense to change this set direclty.
        </block>
    </xsl:template>

    <!--
    <xsl:template match= "xsl:attribute-set[@name='definition-term-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">definition_list/definition_list_item/term</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the inine properties of the term item. 
        </block>
    </xsl:template>
    -->

    <xsl:template match= "xsl:attribute-set[@name='classifier-inline']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:inline</xsl:with-param> 
            <xsl:with-param name="docutils">definition_list/definition_list_item/classifier</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the inine properties of the classifier item. 
        </block>
    </xsl:template>



</xsl:stylesheet>
