<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->
    <xsl:attribute-set name="enumerated-list-block" >
        <xsl:attribute name="start-indent">5mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">10mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-level2-list-block" >
        <xsl:attribute name="start-indent">15mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">10mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="enumerated-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-first-list-item" use-attribute-sets="enumerated-list-item">
        <xsl:attribute name="space-before">0pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-level2-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-level2-first-list-item" 
        use-attribute-sets="enumerated-level2-list-item">
        <xsl:attribute name="space-before">0pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-list-item-label">
        <xsl:attribute name="end-indent">label-end()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-list-item-body">
        <xsl:attribute name="start-indent">body-start()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-list-item-body-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

    
    <xsl:template match="enumerated_list">
        <fo:list-block xsl:use-attribute-sets="enumerated-list-block">
            <xsl:apply-templates/>
        </fo:list-block>
    </xsl:template>

    <xsl:template name="make-enum-list-contents">
        <xsl:variable name="format">
            <xsl:variable name="desc" select="../@enumtype"/>
            <xsl:choose>
                <xsl:when test="$desc = 'arabic'">
                    <xsl:text>1</xsl:text>
                </xsl:when>
                <xsl:when test="$desc = 'upperalpha'">
                    <xsl:text>A</xsl:text>
                </xsl:when>
                <xsl:when test="$desc = 'loweralpha'">
                    <xsl:text>a</xsl:text>
                </xsl:when>
                <xsl:when test="$desc = 'lowerroman'">
                    <xsl:text>i</xsl:text>
                </xsl:when>
                <xsl:when test="$desc = 'upperroman'">
                    <xsl:text>I</xsl:text>
                </xsl:when>
            </xsl:choose>
        </xsl:variable>
        <fo:list-item-label xsl:use-attribute-sets="enumerated-list-item-label">
            <fo:block>
                <xsl:value-of select="../@prefix"/>
                <xsl:number  from="enumerated_list" format="{$format}"/>
                <xsl:value-of select="../@suffix"/>
            </fo:block>
        </fo:list-item-label>
        <fo:list-item-body xsl:use-attribute-sets="enumerated-list-item-body">
            <xsl:apply-templates/>
        </fo:list-item-body>
    </xsl:template>

    <xsl:template match="enumerated_list/list_item">
        <fo:list-item xsl:use-attribute-sets="enumerated-list-item">
            <xsl:call-template name="make-enum-list-contents"/>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="enumerated_list/list_item/enumerated_list/list_item" priority="3">
        <fo:list-item xsl:use-attribute-sets="enumerated-level2-list-item">
            <xsl:call-template name="make-enum-list-contents"/>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="enumerated_list/list_item[1]" priority="2">
        <fo:list-item xsl:use-attribute-sets="enumerated-first-list-item">
            <xsl:call-template name="make-enum-list-contents"/>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="enumerated_list/list_item/enumerated_list/list_item[1]" priority="4">
        <fo:list-item xsl:use-attribute-sets="enumerated-level2-first-list-item">
            <xsl:call-template name="make-enum-list-contents"/>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="enumerated_list/list_item/paragraph">
        <fo:block xsl:use-attribute-sets="enumerated-list-item-body-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="list_item/enumerated_list" priority="2">
        <xsl:variable name="level" select="count(ancestor::list_item)"/>
        <xsl:choose>
            <xsl:when test="$level = 1">
                <fo:list-block xsl:use-attribute-sets="enumerated-level2-list-block">
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


