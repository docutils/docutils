<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!--for the options-list . Element is fo:block.-->
    <xsl:attribute-set name="option-list-definition-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block for each item (option with description) in an option list. Element is fo:block.-->
    <xsl:attribute-set name="option-list-item-block">
        <xsl:attribute name="space-before">8pt</xsl:attribute>
    </xsl:attribute-set>

    <!--for the first block for each item (option with description) in an option list. 
    May need special space for first item.
    Element is fo:block.-->
    <xsl:attribute-set name="option-list-first-item-block" use-attribute-sets="option-list-item-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block for each option. Element is fo:block.-->
    <xsl:attribute-set name="option-group-block">
        <xsl:attribute name="keep-with-next">always</xsl:attribute>
    </xsl:attribute-set>

    <!--
    <xsl:attribute-set name="option-list-description-first-block" use-attribute-sets = "option-list-description-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>
    -->

    <xsl:attribute-set name="option-list-description-block">
        <xsl:attribute name="start-indent">16pt</xsl:attribute>
        <xsl:attribute name="space-before">8pt</xsl:attribute>
    </xsl:attribute-set>


    <!--for the block for paragraphs describing options. 
    This attribute set for the first of such paragraphs
    Element is fo:block.-->

    <xsl:attribute-set name="option-list-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="option-list-first-paragraph-block" use-attribute-sets="option-list-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>


    <xsl:template match="option_list">
        <fo:block role = "option-list" xsl:use-attribute-sets="option-list-definition-block">
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>


    <xsl:template match="option_list/option_list_item[1]"  priority="2">
        <fo:block role="item" xsl:use-attribute-sets = "option-list-first-item-block">
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>

    <xsl:template match="option_list/option_list_item"  priority="1">
        <fo:block role="item" xsl:use-attribute-sets = "option-list-item-block">
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>

    <xsl:template match="option_group" >
        <fo:block role="option-group" xsl:use-attribute-sets = "option-group-block">
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>

    <xsl:template match="option_group/option" >
        <xsl:if test="preceding-sibling::option">
            <xsl:value-of select="$options-separator"/>
        </xsl:if>
        <xsl:apply-templates />
    </xsl:template>

    <xsl:template match="option_list_item/description" >
        <fo:block role="option-list-description" xsl:use-attribute-sets="option-list-description-block">
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>

    <xsl:template match="option_list_item/description/paragraph[1]"  priority="2">
        <fo:block role="option-list-description" xsl:use-attribute-sets="option-list-first-paragraph-block">
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>

    <xsl:template match="option_list_item/description/paragraph"  priority="1">
        <fo:block role="option-list-description" xsl:use-attribute-sets="option-list-paragraph-block">
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>

    <xsl:template match="option_argument" >
        <xsl:value-of select="@delimiter"/>
        <fo:inline xsl:use-attribute-sets="option-argument-inline" role="option-arg">
            <xsl:apply-templates />
        </fo:inline>
    </xsl:template>


</xsl:stylesheet>
