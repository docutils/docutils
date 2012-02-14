<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id: bibliographic_fields.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->


    <!--the automatic page break creates problems for simple page
    layouts when no page break should occurr-->
    <xsl:attribute-set name="bibliographic-fields-list-block" >
        <xsl:attribute name="start-indent">0mm</xsl:attribute>
        <xsl:attribute name="provisional-distance-between-starts">30mm</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="bibliographic-fields-list-item">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="bibliographic-fields-first-list-item" use-attribute-sets = "bibliographic-fields-list-item">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="bibliographic-fields-list-item-label">
        <xsl:attribute name="end-indent">label-end()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="bibliographic-fields-item-body">
        <xsl:attribute name="start-indent">body-start()</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="bibliographic-fields-item-label-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute> 
    </xsl:attribute-set>


    <xsl:attribute-set name="bibliographic-fields-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="bibliographic-first-fields-block" use-attribute-sets = "bibliographic-fields-block">
    </xsl:attribute-set>

    <!--attribute set for Bibliographic address value. Address must preserve white space.  
    Element is fo:block. `docutils`-->
    <xsl:attribute-set name="address-value-block" use-attribute-sets="bibliographic-fields-block">
        <xsl:attribute name="white-space">pre</xsl:attribute>
    </xsl:attribute-set>


    <!--=================END OF ATTRIBUTE SETS=====================================================-->


    <!--only apply temlates if docinfo won't be written to front matter-->
    <xsl:template match="docinfo">
        <fo:list-block role="field-list" xsl:use-attribute-sets="bibliographic-fields-list-block">
            <xsl:apply-templates />
        </fo:list-block>
    </xsl:template>


    <xsl:template name="make-list-item">
        <xsl:param name="role"/>
        <xsl:param name="label-text"/>
        <xsl:param name="has-children"/>
        <xsl:param name="address"/>
        <xsl:choose>
            <xsl:when test="preceding-sibling::*">
                <fo:list-item role="{$role}" xsl:use-attribute-sets="bibliographic-fields-list-item">
                    <xsl:call-template name="list-item-insides">
                        <xsl:with-param name="role" select="$role"/>
                        <xsl:with-param name="label-text" select="$label-text"/>
                        <xsl:with-param name="has-children" select="$has-children"/>
                        <xsl:with-param name="address" select="$address"/>
                    </xsl:call-template>
                </fo:list-item>
            </xsl:when>
            <xsl:otherwise>
                <fo:list-item role="{$role}" xsl:use-attribute-sets="bibliographic-fields-first-list-item">
                    <xsl:call-template name="list-item-insides">
                        <xsl:with-param name="role" select="$role"/>
                        <xsl:with-param name="label-text" select="$label-text"/>
                        <xsl:with-param name="has-children" select="$has-children"/>
                        <xsl:with-param name="address" select="$address"/>
                    </xsl:call-template>
                </fo:list-item>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="list-item-insides">
        <xsl:param name="role"/>
        <xsl:param name="label-text"/>
        <xsl:param name="has-children"/>
        <xsl:param name="address"/>
        <fo:list-item-label xsl:use-attribute-sets = "bibliographic-fields-list-item-label">
            <fo:block xsl:use-attribute-sets="bibliographic-fields-item-label-block">
                <xsl:value-of select="$label-text"/>
            </fo:block>
        </fo:list-item-label>
        <fo:list-item-body xsl:use-attribute-sets="bibliographic-fields-item-body">
            <xsl:choose>
                <xsl:when test="$has-children = 'True'">
                    <xsl:apply-templates />
                </xsl:when>
                <xsl:when test="$address = 'True'">
                    <fo:block xsl:use-attribute-sets = "address-value-block">
                        <xsl:apply-templates />
                    </fo:block>
                </xsl:when>
                <xsl:otherwise>
                    <fo:block xsl:use-attribute-sets="bibliographic-fields-block">
                        <xsl:apply-templates />
                    </fo:block>
                </xsl:otherwise>
            </xsl:choose>
        </fo:list-item-body>
    </xsl:template>

    <xsl:template match="docinfo/author" >
        <xsl:call-template name="make-list-item">
            <xsl:with-param name="label-text" select="$author-text"/>
            <xsl:with-param name="role" select="'authors'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/authors" >
        <xsl:call-template name="make-list-item">
            <xsl:with-param name="label-text" select="$authors-text"/>
            <xsl:with-param name="role" select="'author'"/>
            <xsl:with-param name="has-children" select="'True'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/authors/author" >
        <fo:block xsl:use-attribute-sets="bibliographic-fields-block">
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/organization" >
        <xsl:call-template name="make-list-item">
            <xsl:with-param name="label-text" select="$organization-text"/>
            <xsl:with-param name="role" select="'organization'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/contact" >
        <xsl:call-template name="make-list-item">
            <xsl:with-param name="label-text" select="$contact-text"/>
            <xsl:with-param name="role" select="'contact'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/status" >
        <xsl:call-template name="make-list-item">
            <xsl:with-param name="label-text" select="$status-text"/>
            <xsl:with-param name="role" select="'status'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/copyright" >
        <xsl:call-template name="make-list-item">
            <xsl:with-param name="label-text" select="$copyright-text"/>
            <xsl:with-param name="role" select="'copyright'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/address" >
        <xsl:call-template name="make-list-item">
            <xsl:with-param name="label-text" select="$address-text"/>
            <xsl:with-param name="role" select="'address'"/>
            <xsl:with-param name="address" select="'True'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/version" >
        <xsl:call-template name="make-list-item">
            <xsl:with-param name="label-text" select="$version-text"/>
            <xsl:with-param name="role" select="'version'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/revision" >
        <xsl:call-template name="make-list-item">
            <xsl:with-param name="label-text" select="$revision-text"/>
            <xsl:with-param name="role" select="'revision'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/date" >
        <xsl:call-template name="make-list-item">
            <xsl:with-param name="label-text" select="$version-text"/>
            <xsl:with-param name="role" select="'version'"/>
        </xsl:call-template>
    </xsl:template>


    <!--custom bibliographic fields-->


    <xsl:template match="document/docinfo/field" >
        <xsl:call-template name="make-list-item">
            <xsl:with-param name="label-text">
                <xsl:value-of select="field_name"/>
                <xsl:text>:</xsl:text>
            </xsl:with-param>
            <xsl:with-param name="role" select="'generic-field'"/>
            <xsl:with-param name="has-children" select="'True'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="document/docinfo/field/field_name" >
    </xsl:template>

    <xsl:template match="document/docinfo/field/field_body" >
        <xsl:apply-templates />
    </xsl:template>

    <xsl:template match="docinfo/field/field_body/paragraph" priority="2" >
        <fo:block xsl:use-attribute-sets="bibliographic-fields-block">
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/field/field_body/paragraph[1]" priority="3" >
        <fo:block xsl:use-attribute-sets="bibliographic-first-fields-block">
            <xsl:apply-templates />
        </fo:block>
    </xsl:template>


</xsl:stylesheet>

