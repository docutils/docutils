<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->
    <!--TODO

    Custom bullets (rather than default) by means of a parameter-->

        <!--
        "The provisional-distance-between-starts property of the 
        list-block specifies the distance bewteen the start of the
        label (the bullet, for example) and the actual start of
        the list content" (Pawson, 100).

        -->

    <!--Attribute sets for Bullet List-->

    <xsl:attribute-set name="bullet-list" >
        <xsl:attribute name="start-indent">5mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">5mm</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="bullet-list-item">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="bullet-list-last-item" use-attribute-sets="bullet-list-item">
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

    <xsl:attribute-set name="enumerated-list" >
        <xsl:attribute name="start-indent">5mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">15mm</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-list-item">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-list-last-item" use-attribute-sets="enumerated-list-item">
        <xsl:attribute name="space-after">0pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-list-item-label">
        <xsl:attribute name="end-indent">label-end()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-list-item-body">
        <xsl:attribute name="start-indent">body-start()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-list-paragraph">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="enumerated-list-level2" >
        <xsl:attribute name="start-indent">15mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">15mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--Definition list attriute sets-->

    <xsl:attribute-set name="definition-list" >
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="term" >
    </xsl:attribute-set>

    <xsl:attribute-set name="term-last" use-attribute-sets="term" >
    </xsl:attribute-set>

    <xsl:attribute-set name="definition-list-paragraph">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
        <xsl:attribute name="start-indent">30pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="definition-list-last-paragraph" use-attribute-sets="definition-list-paragraph">
        <xsl:attribute name="space-after">0pt</xsl:attribute> 
    </xsl:attribute-set>


    <xsl:attribute-set name="definition-term">
        <xsl:attribute name="font-weight">bold</xsl:attribute> 
    </xsl:attribute-set>

    <!--FIELD LIST attribute sets-->

    <xsl:attribute-set name="field-list" >
        <xsl:attribute name="start-indent">0mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">30mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="field-list-item">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="field-list-last-item" use-attribute-sets="enumerated-list-item">
        <xsl:attribute name="space-after">0pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="field-list-item-label">
        <xsl:attribute name="end-indent">label-end()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="field-list-item-body">
        <xsl:attribute name="start-indent">body-start()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="field-list-paragraph">
        <xsl:attribute name="space-after">12pt</xsl:attribute> 
    </xsl:attribute-set>

    <xsl:attribute-set name="field-name">
        <xsl:attribute name="font-weight">bold</xsl:attribute> 
    </xsl:attribute-set>




    <!--END attribute sets-->

    <xsl:template match="bullet_list">
        <fo:list-block xsl:use-attribute-sets="bullet-list">
            <xsl:apply-templates/>
        </fo:list-block>
    </xsl:template>

    <xsl:template match="bullet_list/list_item">
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
    <xsl:template match="bullet_list/list_item[last()]">
        <fo:list-item xsl:use-attribute-sets="bullet-list-last-item">
            <fo:list-item-label xsl:use-attribute-sets="bullet-list-item-label">
                <fo:block><xsl:value-of select="../@bullet"/></fo:block>
            </fo:list-item-label>
            <fo:list-item-body xsl:use-attribute-sets="bullet-list-item-body">
                <xsl:apply-templates/>
            </fo:list-item-body>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="bullet_list/list_item/paragraph">
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

    <!--enumerated list-->
    
    <xsl:template match="enumerated_list">
        <fo:list-block xsl:use-attribute-sets="enumerated-list">
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

    <xsl:template match="enumerated_list/list_item[last()]">
        <fo:list-item xsl:use-attribute-sets="enumerated-list-last-item">
            <xsl:call-template name="make-enum-list-contents"/>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="enumerated_list/list_item/paragraph">
        <fo:block xsl:use-attribute-sets="bullet-list-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="list_item/enumerated_list" priority="2">
        <xsl:variable name="level" select="count(ancestor::list_item)"/>
        <xsl:choose>
            <xsl:when test="$level = 1">
                <fo:list-block xsl:use-attribute-sets="enumerated-list-level2">
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

    <!--DEFINITION LIST-->

    <xsl:template match="definition_list">
        <fo:block role="defintion-list" xsl:use-attribute-sets="definition-list">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


    <xsl:template match="definition_list_item">
        <fo:block xsl:use-attribute-sets = "definition-term" role="term">
            <xsl:apply-templates select="term" mode = "term"/>
            <xsl:apply-templates select="classifier" mode = "classifier"/>
        </fo:block>
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="definition/paragraph">
        <xsl:variable name="position">
            <xsl:for-each select="ancestor::definition_list_item">
                <xsl:if test = "not(following-sibling::definition_list_item)">
                    <xsl:text>last</xsl:text>
                </xsl:if>
            </xsl:for-each>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$position = 'last'">
                <fo:block xsl:use-attribute-sets="definition-list-last-paragraph" 
                    role="definition-paragraph">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:otherwise>
                <fo:block xsl:use-attribute-sets="definition-list-paragraph" 
                    role="definition-paragraph">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <xsl:template match="list_item/definition_list" priority="2">
        <xsl:message terminate="yes">
            <xsl:text>FATAL: Should not have nested defintiion lists</xsl:text>
            <xsl:text>Processinng stylesheets now quiting</xsl:text>
        </xsl:message>
    </xsl:template>

    <xsl:template match="definition">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="term" mode="term">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="classifier" mode="classifier">
        <xsl:text> :</xsl:text>
        <fo:inline xsl:use-attribute-sets="emphasis">
            <xsl:apply-templates mode="classifier"/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="classifier|term"/>



    <!--start a list-->
    <xsl:template match="field_list">
        <fo:list-block xsl:use-attribute-sets="field-list">
            <xsl:apply-templates/>
        </fo:list-block>
    </xsl:template>

    <xsl:template match="field">
        <fo:list-item xsl:use-attribute-sets="field-list-item">
            <xsl:apply-templates/>
        </fo:list-item>
    </xsl:template>

    <!--last item, may be different for space-->
    <xsl:template match="field_list/field[last()]">
        <fo:list-item xsl:use-attribute-sets="field-list-last-item">
            <xsl:apply-templates/>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="field_list/field/field_body/paragraph">
        <fo:block xsl:use-attribute-sets="field-list-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="field_body">
        <fo:list-item-body xsl:use-attribute-sets="field-list-item-body">
            <xsl:apply-templates/>
        </fo:list-item-body>
    </xsl:template>

    <xsl:template match="field_name">
        <fo:list-item-label xsl:use-attribute-sets="field-list-item-label">
            <fo:block xsl:use-attribute-sets="field-name">
                <xsl:apply-templates/>
            </fo:block>
        </fo:list-item-label>
    </xsl:template>


</xsl:stylesheet> 
