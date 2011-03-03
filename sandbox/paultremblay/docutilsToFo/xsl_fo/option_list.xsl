<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <!-- For the option list. "The provisional-distance-between-starts
    property of the list-block specifies the distance bewteen the start of the
    label (the bullet, for example) and the actual start of the list content"
    (Pawson, 100) 

    Element is fo:block-list
    -->
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


    <!--for the text of each option in an option list. Element is fo:inline.-->
    <xsl:attribute-set name="option-inline">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
    </xsl:attribute-set>

    <!--for the text of each option argument in an option list. Element is fo:inline.-->
    <xsl:attribute-set name="option-argument-inline">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
	<xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>


    <!--option_list as definition list-->


    <!--for the options-list . Element is fo:block.`doc`-->
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
        <xsl:choose>
            <xsl:when test="$option-list-format = 'both'"><!--only used in testing-->
                <xsl:call-template name="option-list-as-list"/>
                <xsl:call-template name="option-list-as-definition-list"/>
            </xsl:when>
            <xsl:when test="$option-list-format = 'list'">
                <xsl:call-template name="option-list-as-list"/>
            </xsl:when>
            <xsl:when test="$option-list-format = 'definition'">
                <xsl:call-template name="option-list-as-definition-list"/>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="option-list-as-list">
        <fo:list-block xsl:use-attribute-sets="option-list-block" role="option-list">
            <xsl:apply-templates mode="list"/>
        </fo:list-block>
    </xsl:template>

    <xsl:template match="option_list/option_list_item" mode="list">
        <fo:list-item xsl:use-attribute-sets="option-list-item">
            <xsl:apply-templates mode="list"/>
        </fo:list-item>
    </xsl:template>

    <!--first item, may want different spacing-->
    <xsl:template match="option_list/option_list_item[1]" mode="list" priority="2">
        <fo:list-item xsl:use-attribute-sets="option-first-list-item">
            <xsl:apply-templates mode="list"/>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="option_list_item/option_group" mode="list">
        <fo:list-item-label xsl:use-attribute-sets="option-list-item-label" role="options and arguments">
            <fo:block xsl:use-attribute-sets="option-list-item-label-block">
                <xsl:apply-templates mode="list"/>
            </fo:block>
        </fo:list-item-label>
    </xsl:template>

    <xsl:template match="option_group/option" mode="list">
        <xsl:if test="preceding-sibling::option">
            <xsl:value-of select="$options-separator"/>
        </xsl:if>
        <xsl:apply-templates mode="list"/>
    </xsl:template>


    <xsl:template match="option_list/option_list_item/description" mode="list">
        <fo:list-item-body xsl:use-attribute-sets="option-list-item-body"  role="description">
            <xsl:apply-templates mode="list"/>
        </fo:list-item-body>
    </xsl:template>

    <xsl:template match="option_list_item/description/paragraph" mode="list">
        <fo:block xsl:use-attribute-sets="option-list-item-body-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_string" mode="list">
        <xsl:for-each select="parent::option">
            <xsl:if test="preceding-sibling::option">
                <xsl:text> </xsl:text>
            </xsl:if>
        </xsl:for-each>
        <fo:inline xsl:use-attribute-sets="option-inline" role="option">
            <xsl:apply-templates mode="list"/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="option_argument" mode="list">
        <xsl:value-of select="@delimiter"/>
        <fo:inline xsl:use-attribute-sets="option-argument-inline" role="option-arg">
            <xsl:apply-templates mode="list"/>
        </fo:inline>
    </xsl:template>

    <!--==================================================================================-->
    <!--templates for making defintion layout-->


    <xsl:template name="option-list-as-definition-list">
        <fo:block role = "option-list" xsl:use-attribute-sets="option-list-definition-block">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_list/option_list_item[1]" mode="definition" priority="2">
        <fo:block role="item" xsl:use-attribute-sets = "option-list-first-item-block">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_list/option_list_item" mode="definition" priority="1">
        <fo:block role="item" xsl:use-attribute-sets = "option-list-item-block">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_group" mode="definition">
        <fo:block role="option-group" xsl:use-attribute-sets = "option-group-block">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_group/option" mode="definition">
        <xsl:if test="preceding-sibling::option">
            <xsl:value-of select="$options-separator"/>
        </xsl:if>
        <xsl:apply-templates mode="definition"/>
    </xsl:template>

    <xsl:template match="option_list_item/description" mode="definition">
        <fo:block role="option-list-description" xsl:use-attribute-sets="option-list-description-block">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_list_item/description/paragraph[1]" mode="definition" priority="2">
        <fo:block role="option-list-description" xsl:use-attribute-sets="option-list-first-paragraph-block">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_list_item/description/paragraph" mode="definition" priority="1">
        <fo:block role="option-list-description" xsl:use-attribute-sets="option-list-paragraph-block">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_string" mode="definition">
        <fo:inline xsl:use-attribute-sets="option-inline" role="option">
            <xsl:apply-templates mode="list"/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="option_argument" mode="definition">
        <xsl:value-of select="@delimiter"/>
        <fo:inline xsl:use-attribute-sets="option-argument-inline" role="option-arg">
            <xsl:apply-templates mode="list"/>
        </fo:inline>
    </xsl:template>

    
</xsl:stylesheet> 
