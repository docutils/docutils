<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <!-- For the bullet list. "The provisional-distance-between-starts
    property of the list-block specifies the distance bewteen the start of the
    label (the bullet, for example) and the actual start of the list content"
    (Pawson, 100) 

    Element is fo:block-list
    -->
    <xsl:attribute-set name="bullet-list-block" >
        <xsl:attribute name="start-indent">5mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">5mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--for the bullet list item. FO element is list-item. docutils element is list_item.-->
    <xsl:attribute-set name="bullet-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <!--for the bullet list item last item. Space may be different.  Element is fo:list-item.-->
    <xsl:attribute-set name="bullet-first-list-item" use-attribute-sets="bullet-list-item">
        <xsl:attribute name="space-before">0pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="bullet-level2-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <!--for the bullet list item last item. Space may be different.  Element is fo:list-item.-->
    <xsl:attribute-set name="bullet-level2-first-list-item" use-attribute-sets="bullet-level2-list-item">
        <xsl:attribute name="space-before">0pt</xsl:attribute> 
    </xsl:attribute-set>

    <!--Element is fo:list-item-label.-->
    <xsl:attribute-set name="bullet-list-item-label">
        <xsl:attribute name="end-indent">label-end()</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block in the bullet label fo:list-item-lable/block. Element is fo:block.`doc`-->
    <xsl:attribute-set name="bullet-list-item-label-block">
    </xsl:attribute-set>

    <!--for the option list item body. Element is fo:list-item-body.-->
    <xsl:attribute-set name="bullet-list-item-body">
        <xsl:attribute name="start-indent">body-start()</xsl:attribute>
    </xsl:attribute-set>

    <!--for the block of the body (paragraphs) in an bullet list. Element is fo:block.-->
    <xsl:attribute-set name="bullet-list-item-body-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <!--same as above-->
    <xsl:attribute-set name="bullet-level2-list-block" >
        <xsl:attribute name="start-indent">15mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">5mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:template match="bullet_list">
        <fo:list-block xsl:use-attribute-sets="bullet-list-block" role="bullet-list">
            <xsl:apply-templates/>
        </fo:list-block>
    </xsl:template>

    <xsl:template name="make-bullet">
        <xsl:variable name="level" select="count(ancestor::bullet_list)"/>
        <xsl:choose>
            <xsl:when test="$level = 1">
                <xsl:choose>
                    <xsl:when test="$bullet-text != ''">
                        <xsl:value-of select="$bullet-text"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="../@bullet"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
                <xsl:choose>
                    <xsl:when test="$bullet-text-level2 != ''">
                        <xsl:value-of select="$bullet-text-level2"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="../@bullet"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="bullet_list/list_item">
        <fo:list-item xsl:use-attribute-sets="bullet-list-item">
            <fo:list-item-label xsl:use-attribute-sets="bullet-list-item-label">
                <fo:block xsl:use-attribute-sets = "bullet-list-item-label-block"><xsl:call-template name="make-bullet"/></fo:block>
            </fo:list-item-label>
            <fo:list-item-body xsl:use-attribute-sets="bullet-list-item-body">
                <xsl:apply-templates/>
            </fo:list-item-body>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="bullet_list/list_item[1]" priority="2">
        <fo:list-item xsl:use-attribute-sets="bullet-first-list-item">
            <fo:list-item-label xsl:use-attribute-sets="bullet-list-item-label">
                <fo:block xsl:use-attribute-sets = "bullet-list-item-label-block"><xsl:call-template name="make-bullet"/></fo:block>
            </fo:list-item-label>
            <fo:list-item-body xsl:use-attribute-sets="bullet-list-item-body">
                <xsl:apply-templates/>
            </fo:list-item-body>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="bullet_list/list_item/bullet_list/list_item" priority="3">
        <fo:list-item xsl:use-attribute-sets="bullet-level2-list-item">
            <fo:list-item-label xsl:use-attribute-sets="bullet-list-item-label">
                <fo:block xsl:use-attribute-sets = "bullet-list-item-label-block"><xsl:call-template name="make-bullet"/></fo:block>
            </fo:list-item-label>
            <fo:list-item-body xsl:use-attribute-sets="bullet-list-item-body">
                <xsl:apply-templates/>
            </fo:list-item-body>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="bullet_list/list_item/bullet_list/list_item[1]" priority="4">
        <fo:list-item xsl:use-attribute-sets="bullet-level2-first-list-item">
            <fo:list-item-label xsl:use-attribute-sets="bullet-list-item-label">
                <fo:block xsl:use-attribute-sets = "bullet-list-item-label-block"><xsl:call-template name="make-bullet"/></fo:block>
            </fo:list-item-label>
            <fo:list-item-body xsl:use-attribute-sets="bullet-list-item-body">
                <xsl:apply-templates/>
            </fo:list-item-body>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="bullet_list/list_item/paragraph">
        <fo:block xsl:use-attribute-sets="bullet-list-item-body-block" role="bullet-list-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="list_item/bullet_list" priority="2">
        <xsl:variable name="level" select="count(ancestor::list_item)"/>
        <xsl:choose>
            <xsl:when test="$level = 1">
                <fo:list-block xsl:use-attribute-sets="bullet-level2-list-block" role="bullet-list2">
                    <xsl:apply-templates/>
                </fo:list-block>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message>
                    <xsl:text>Cannot format lists more than 2 levels deep</xsl:text>
                </xsl:message>
                <xsl:choose>
                    <xsl:when test="$strict='True'">
                        <xsl:message terminate="yes">Processinng stylesheets now quiting</xsl:message>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:message>Not formatting text</xsl:message>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


</xsl:stylesheet> 
