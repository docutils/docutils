<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id: option_list.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->

    <xsl:attribute-set name="option-list-block">
        <xsl:attribute name="start-indent">0mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">50mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--for the option list item. FO element is list-item. docutils element is list_item.-->
    <xsl:attribute-set name="option-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <!--for the option list item last item. Space may be different.  Element is fo:list-item.-->
    <xsl:attribute-set name="option-first-list-item" use-attribute-sets="option-list-item">
        <xsl:attribute name="space-before">0pt</xsl:attribute> 
    </xsl:attribute-set>

    <!--Element is fo:list-item-label.-->
    <xsl:attribute-set name="option-list-item-label">
        <xsl:attribute name="end-indent">label-end()</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block in the options label fo:list-item-lable/block. Element is fo:block.-->
    <xsl:attribute-set name="option-list-item-label-block">
    </xsl:attribute-set>

    <!--for the option list item body. Element is fo:list-item-body.-->
    <xsl:attribute-set name="option-list-item-body">
        <xsl:attribute name="start-indent">body-start()</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block that formats each description in an option list. Element is fo:block.-->
    <xsl:attribute-set name="option-list-item-body-block">
    </xsl:attribute-set>


    <!--for the text of each option . Element is fo:inline.-->
    <xsl:attribute-set name="option-inline">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
    </xsl:attribute-set>

    <!--for the text of each option argument in an option list. Element is fo:inline.-->
    <xsl:attribute-set name="option-argument-inline">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
	<xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>


    <xsl:template match="option_list">
        <fo:list-block xsl:use-attribute-sets="option-list-block" role="option-list">
            <xsl:apply-templates />
        </fo:list-block>
    </xsl:template>


    <xsl:template match="option_list/option_list_item" >
        <fo:list-item xsl:use-attribute-sets="option-list-item">
            <xsl:apply-templates />
        </fo:list-item>
    </xsl:template>

    <!--first item, may want different spacing-->
    <xsl:template match="option_list/option_list_item[1]" priority="2">
        <fo:list-item xsl:use-attribute-sets="option-first-list-item">
            <xsl:apply-templates />
        </fo:list-item>
    </xsl:template>

    <xsl:template match="option_list_item/option_group" >
        <fo:list-item-label xsl:use-attribute-sets="option-list-item-label" role="options and arguments">
            <fo:block xsl:use-attribute-sets="option-list-item-label-block">
                <xsl:apply-templates />
            </fo:block>
        </fo:list-item-label>
    </xsl:template>

    <xsl:template match="option_group/option" >
        <xsl:if test="preceding-sibling::option">
            <xsl:value-of select="$options-separator"/>
        </xsl:if>
        <xsl:apply-templates />
    </xsl:template>


    <xsl:template match="option_list/option_list_item/description" >
        <fo:list-item-body xsl:use-attribute-sets="option-list-item-body"  role="description">
            <xsl:apply-templates />
        </fo:list-item-body>
    </xsl:template>

    <xsl:template match="option_list_item/description/paragraph" >
        <fo:block xsl:use-attribute-sets="option-list-item-body-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_string" >
        <xsl:for-each select="parent::option">
            <xsl:if test="preceding-sibling::option">
                <xsl:text> </xsl:text>
            </xsl:if>
        </xsl:for-each>
        <fo:inline xsl:use-attribute-sets="option-inline" role="option">
            <xsl:apply-templates />
        </fo:inline>
    </xsl:template>

    <xsl:template match="option_argument" >
        <xsl:value-of select="@delimiter"/>
        <fo:inline xsl:use-attribute-sets="option-argument-inline" role="option-arg">
            <xsl:apply-templates />
        </fo:inline>
    </xsl:template>


    <xsl:template match="option_string" >
        <fo:inline xsl:use-attribute-sets="option-inline" role="option">
            <xsl:apply-templates />
        </fo:inline>
    </xsl:template>

</xsl:stylesheet> 
