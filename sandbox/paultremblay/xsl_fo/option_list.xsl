<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->


    <!--for the block that formats each option in an option list. Element is fo:block.`doc`-->
    <xsl:attribute-set name="option-paragraph">
    </xsl:attribute-set>

    <!--for the block that formats each description in an option list. Element is fo:block.`doc`-->
    <xsl:attribute-set name="description-paragraph">
    </xsl:attribute-set>

    <!--for the text of each option in an option list. Element is fo:inline.`doc`-->
    <xsl:attribute-set name="option-string">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
    </xsl:attribute-set>

    <!--for the text of each option argument in an option list. Element is fo:inline.`doc`-->
    <xsl:attribute-set name="option-argument-string">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
	<xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <!--for the option list. Element is fo:list-block.`doc`-->
    <xsl:attribute-set name="option-list">
        <xsl:attribute name="start-indent">0mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">50mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--for the option list item. Element is fo:list-item.`doc`-->
    <xsl:attribute-set name="option-list-item">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <!--for the option list item last item. Space may be different.  Element is fo:list-item.`doc`-->
    <xsl:attribute-set name="option-list-last-item">
        <xsl:attribute name="space-after">0pt</xsl:attribute> 
    </xsl:attribute-set>

    <!--for the option list item last item. Element is fo:list-item-label.`doc`-->
    <xsl:attribute-set name="option-list-item-label">
        <xsl:attribute name="end-indent">label-end()</xsl:attribute>
    </xsl:attribute-set>

    <!--for the option list item body. Element is fo:list-item-body.`doc`-->
    <xsl:attribute-set name="option-list-item-body">
        <xsl:attribute name="start-indent">body-start()</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block in the options label. Element is fo:block.`doc`-->
    <xsl:attribute-set name="options-string-block">
    </xsl:attribute-set>

    <!--option_list as definition list-->


    <!--for the options-list . Element is fo:block.`doc`-->
    <xsl:attribute-set name="option-list_d">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block for each item (option with description) in an option list. Element is fo:block.`doc`-->
    <xsl:attribute-set name="option-and-desc">
        <xsl:attribute name="space-before">8pt</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block for each item (option with description) in an option list. 
    May need special space for first item.
    Element is fo:block.`doc`-->
    <xsl:attribute-set name="option-and-desc-first" use-attribute-sets="option-and-desc">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block for each option. Element is fo:block.`doc`-->
    <xsl:attribute-set name="options">
        <xsl:attribute name="keep-with-next">always</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block for paragraphs in descrition describing optons. Element is fo:block.`doc`-->
    <xsl:attribute-set name="description-paragraph_d">
        <xsl:attribute name="start-indent">16pt</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block for paragraphs describing options. 
    This attribute set for all but first of such paragraphs
    Element is fo:block.`doc`-->
    <xsl:attribute-set name="description-other-paragraph_d" use-attribute-sets="description-paragraph_d">
        <xsl:attribute name="space-before">8pt</xsl:attribute>
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
        <fo:list-block xsl:use-attribute-sets="option-list" role="option-list">
            <xsl:apply-templates mode="list"/>
        </fo:list-block>
    </xsl:template>

    <xsl:template match="option_list/option_list_item" mode="list">
        <fo:list-item xsl:use-attribute-sets="option-list-item">
            <xsl:apply-templates mode="list"/>
        </fo:list-item>
    </xsl:template>

    <!--last item, may be different for space-->
    <xsl:template match="option_list/option_list_item[last()]" mode="list">
        <fo:list-item xsl:use-attribute-sets="option-list-last-item">
            <xsl:apply-templates mode="list"/>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="option_list_item/option_group" mode="list">
        <fo:list-item-label xsl:use-attribute-sets="option-list-item-label">
            <fo:block xsl:use-attribute-sets="options-string-block">
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


    <xsl:template match="description" mode="list">
        <fo:list-item-body xsl:use-attribute-sets="option-list-item-body">
            <xsl:apply-templates mode="list"/>
        </fo:list-item-body>
    </xsl:template>

    <xsl:template match="option_list_item/description/paragraph" mode="list">
        <fo:block role="option-list-desc" xsl:use-attribute-sets="description-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_string" mode="list">
        <xsl:for-each select="parent::option">
            <xsl:if test="preceding-sibling::option">
                <xsl:text> </xsl:text>
            </xsl:if>
        </xsl:for-each>
        <fo:inline xsl:use-attribute-sets="option-string" role="option">
            <xsl:apply-templates mode="list"/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="option_argument" mode="list">
        <xsl:value-of select="@delimiter"/>
        <fo:inline xsl:use-attribute-sets="option-argument-string" role="option-arg">
            <xsl:apply-templates mode="list"/>
        </fo:inline>
    </xsl:template>

    <!--==================================================================================-->
    <!--templates for making defintion layout-->


    <xsl:template name="option-list-as-definition-list">
        <fo:block role = "option-list" xsl:use-attribute-sets="option_list_d">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_list_item[1]" mode="definition" priority="2">
        <fo:block role="option-and-desc" xsl:use-attribute-sets = "option-and-desc-first">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_list_item" mode="definition" priority="1">
        <fo:block role="option-and-desc" xsl:use-attribute-sets = "option-and-desc">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_group" mode="definition">
        <fo:block role="options" xsl:use-attribute-sets = "options">
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
        <xsl:apply-templates mode="definition"/>
    </xsl:template>

    <xsl:template match="option_list_item/description/paragraph[1]" mode="definition" priority="2">
        <fo:block role="option-list-desc" xsl:use-attribute-sets="description-paragraph_d">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_list_item/description/paragraph" mode="definition" priority="1">
        <fo:block role="option-list-desc" xsl:use-attribute-sets="description-other-paragraph_d">
            <xsl:apply-templates mode="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="option_string" mode="definition">
        <xsl:for-each select="parent::option">
        </xsl:for-each>
        <fo:inline xsl:use-attribute-sets="option-string" role="option">
            <xsl:apply-templates mode="list"/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="option_argument" mode="definition">
        <xsl:value-of select="@delimiter"/>
        <fo:inline xsl:use-attribute-sets="option-argument-string" role="option-arg">
            <xsl:apply-templates mode="list"/>
        </fo:inline>
    </xsl:template>

    
</xsl:stylesheet> 
