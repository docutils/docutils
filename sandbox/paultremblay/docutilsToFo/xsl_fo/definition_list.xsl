<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->
    <!--Definition list attriute sets-->

    <!--the block that wraps all the other blocks. Use to 
    contol space before the list and the the text before and after-->
    <xsl:attribute-set name="definition-list-block" >
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--controls properties of the whole item. Can be used to control space 
    between items.-->
    <xsl:attribute-set name="definition-list-item-block" >
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--Sames as definition-list-item-block, but for the first block.
    It inherits from the definition-list-item-block. 
    -->
    <xsl:attribute-set name="definition-list-item-first-block" use-attribute-sets="definition-list-item-block" >
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <!--for the bock of the the term. Can be used to control spacing between
    term and definition, but don't use with space before, or you won't be able
    to control spacing before list-->
    <xsl:attribute-set name="definition-term-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="definition-block">
    </xsl:attribute-set>


    <!--
    <xsl:attribute-set name="definition-term-inline">
        <xsl:attribute name="font-weight">bold</xsl:attribute> 
        <xsl:attribute name="role">definition-term-inline</xsl:attribute>
    </xsl:attribute-set>
    -->

    <xsl:attribute-set name="classifier-inline">
	<xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>


    <!--for all the blocks in the definition. There are not other blocks below
    (nested) in this one. Use to control the space between paragraphs by
    setting the space-bfore attribute. Don't use the space-after attribute, or
    you won't be able to contorl the spacing between items-->

    <xsl:attribute-set name="definition-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
        <xsl:attribute name="start-indent">30pt</xsl:attribute>
    </xsl:attribute-set>

    <!--
    Same as the definition-parapgraph-block, except this one contorls the first 
    parapgraph. It doesn't make sense to change any of the attributes here, since
    it inherits from the definitionn-paragraph-block.  -->
    <xsl:attribute-set name="definition-first-paragraph-block" use-attribute-sets="definition-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute> 
    </xsl:attribute-set>

    <!--END OF ATTRIBUTE SETS-->

    <xsl:template match="definition_list">
        <fo:block role="definition-list" xsl:use-attribute-sets="definition-list-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


    <xsl:template match="definition_list_item">
        <fo:block xsl:use-attribute-sets = "definition-list-item-block" role="definition-list-item">
            <fo:block role="term" xsl:use-attribute-sets="definition-term-block">
                <xsl:apply-templates select="term"/>
                <xsl:apply-templates select="classifier"/>
            </fo:block>
            <xsl:apply-templates select="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="definition_list_item[1]" priority="2">
        <fo:block xsl:use-attribute-sets = "definition-list-item-first-block" role="definition-list-item">
            <fo:block role="term" xsl:use-attribute-sets="definition-term-block">
                <xsl:apply-templates select="term"/>
                <xsl:apply-templates select="classifier"/>
            </fo:block>
            <xsl:apply-templates select="definition"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="definition_list_item/term">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="definition_list_item/definition">
        <fo:block role="definition" xsl:use-attribute-sets = "definition-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="definition/paragraph[1]" priority="2">
        <fo:block xsl:use-attribute-sets="definition-first-paragraph-block" 
            role="definition-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="definition/paragraph">
        <fo:block xsl:use-attribute-sets="definition-paragraph-block" 
            role="definition-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>
    
    <xsl:template match="classifier">
        <xsl:text> :</xsl:text>
        <fo:inline xsl:use-attribute-sets="classifier-inline">
            <xsl:apply-templates />
        </fo:inline>
    </xsl:template>


    <xsl:template match="list_item/definition_list" priority="2">
        <xsl:message terminate="yes">
            <xsl:text>FATAL: Should not have nested definition lists</xsl:text>
            <xsl:text>Processinng stylesheets now quiting</xsl:text>
        </xsl:message>
    </xsl:template>



    
</xsl:stylesheet> 
