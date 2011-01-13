<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->
    <!--TODO

    Custom bulles (rather than default) by means of a parameter-->


    <xsl:attribute-set name="bullet-list" >
        <xsl:attribute name="start-indent">5mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">5mm</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="bullet-list-item">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="bullet-list-last-item" use-attriubte-sets="bullet-list-item">
        <xsl:attribute name="space-after">0pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="bullet-list-item-label">
        <xsl:attribute name="end-indent">label-end()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="bullet-list-item-body">
        <xsl:attribute name="start-indent">body-start()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="bullet-list-paragraph">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="bullet-list-level2" >
        <xsl:attribute name="start-indent">15mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">5mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

        <!--
        "The provisional-distance-between-starts property of the 
        list-block specifies the distance bewteen the start of the
        label (the bullet, for example) and the actual start of
        the list content" (Pawson, 100).

        -->

    <xsl:template match="bullet_list">
        <fo:list-block xsl:use-attribute-sets="bullet-list">
            <xsl:apply-templates/>
        </fo:list-block>
    </xsl:template>

    <xsl:template match="list_item">
        <fo:list-item xsl:use-attribute-sets="bullet-list-item">
            <fo:list-item-label xsl:use-attribute-sets="bullet-list-item-label">
                <fo:block><xsl:value-of select="../@bullet"/></fo:block>
            </fo:list-item-label>
            <fo:list-item-body xsl:use-attribute-sets="bullet-list-item-body">
                <xsl:apply-templates/>
            </fo:list-item-body>
        </fo:list-item>
    </xsl:template>

    <!--last item, may be different for space-->
    <xsl:template match="list_item[last()]">
        <fo:list-item xsl:use-attribute-sets="bullet-list-last-item">
            <fo:list-item-label xsl:use-attribute-sets="bullet-list-item-label">
                <fo:block><xsl:value-of select="../@bullet"/></fo:block>
            </fo:list-item-label>
            <fo:list-item-body xsl:use-attribute-sets="bullet-list-item-body">
                <xsl:apply-templates/>
            </fo:list-item-body>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="list_item/paragraph">
        <fo:block xsl:use-attribute-sets="bullet-list-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="list_item/bullet_list" priority="2">
        <xsl:variable name="level" select="count(ancestor::list_item)"/>
        <xsl:choose>
            <xsl:when test="$level = 1">
                <fo:list-block xsl:use-attribute-sets="bullet-list-level2">
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
