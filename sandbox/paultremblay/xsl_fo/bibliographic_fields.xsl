<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!--label and value-->
    <!-- $Id$ -->

    <!--Attribute sets for Bibliographic Fields-->

    <!--attribute set for Bibliographic table of all values.  Element is fo:table. `docutils`-->
    <xsl:attribute-set name="docinfo-table">
        <xsl:attribute name="inline-progression-dimension">6in</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for Bibliographic cells for labels.  Element is fo:table-cell. `docutils`-->
    <xsl:attribute-set name="docinfo-label-cell" >
        <xsl:attribute name="padding">3pt</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for Bibliographic cells for values.  Element is fo:table-cell. `docutils`-->
    <xsl:attribute-set name="docinfo-value-cell">
        <xsl:attribute name="padding">3pt</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for Bibliographic labes, such as Author, Organization, etc. 
    Element is fo:block. `docutils`-->
    <xsl:attribute-set name="docinfo-label">
        <xsl:attribute name="space-after">0pt</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for Bibliographic values, such as Paul Tremblay, Open Source, etc. 
    Element is fo:block. `docutils`-->
    <xsl:attribute-set name="docinfo-value">
        <xsl:attribute name="space-after">0pt</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for Bibliographic address value. Address must preserve white space.  
    Element is fo:block. `docutils`-->
    <xsl:attribute-set name="address-value" use-attribute-sets="docinfo-value">
        <xsl:attribute name="white-space">pre</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for Bibliographic label row.  Element is fo:table-column. `docutils`-->
    <xsl:attribute-set name="docinfo-label-row" use-attribute-sets="docinfo-value">
        <xsl:attribute name="column-width">1.5in</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for Bibliographic value row.  Element is fo:table-column. `docutils`-->
    <xsl:attribute-set name="docinfo-value-row" use-attribute-sets="docinfo-value">
        <xsl:attribute name="column-width">4in</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for after docinfo.  There is an empty block just for the layout in which docinfo
    gets put into the front matter. This block should have the break-after = page value
    to force a break. Element is fo:block. `docutils`-->
    <xsl:attribute-set name="after-docinfo">
        <xsl:attribute name="break-after">page</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for dedication paragraph. Element is fo:block-->
    <xsl:attribute-set name="dedication-paragraph">
    </xsl:attribute-set>

    <!--attribute set for abstract paragraph. Element is fo:block-->
    <xsl:attribute-set name="abstract-paragraph">
    </xsl:attribute-set>

    <!--attribute set for abstract title. Element is fo:block-->
    <xsl:attribute-set name="abstract-title">
    </xsl:attribute-set>

    <!--attribute set for dedication title. Element is fo:block-->
    <xsl:attribute-set name="dedication-title">
    </xsl:attribute-set>

    <!--attribute set for dedication wrapper block (to be able to force a break after). Element is fo:block-->
    <xsl:attribute-set name="dedication">
        <xsl:attribute name="break-after">page</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for abstract wrapper block (to be able to force a break after). Element is fo:block-->
    <xsl:attribute-set name="abstract">
        <xsl:attribute name="break-after">page</xsl:attribute>
    </xsl:attribute-set>

    <xsl:template name="docinfo">
        <fo:table  table-layout="fixed" xsl:use-attribute-sets="docinfo-table">
            <fo:table-column column-number="1" xsl:use-attribute-sets="docinfo-label-row"/>
            <fo:table-column column-number="2" xsl:use-attribute-sets="docinfo-value-row"/>
            <fo:table-body>
                <xsl:apply-templates/>
            </fo:table-body>
        </fo:table>
    </xsl:template>

    <!--only apply temlates if docinfo won't be written to front matter-->
    <xsl:template match="docinfo">
        <xsl:if test="$page-sequence-type = 'body' or $page-sequence-type = 'toc-body'">
            <xsl:call-template name="docinfo"/>
        </xsl:if>
    </xsl:template>

    <xsl:template match="docinfo" mode="front">
        <xsl:call-template name="docinfo"/>
        <fo:block xsl:use-attribute-sets = "after-docinfo"/>
    </xsl:template>

    <xsl:template name="make-docinfo-row">
        <xsl:param name="label-text"/>
        <xsl:param name="role"/>
        <xsl:param name="has-children"/>
        <xsl:param name="address"/>
        <fo:table-row role="{$role}">
            <fo:table-cell xsl:use-attribute-sets = "docinfo-label-cell">
                <fo:block xsl:use-attribute-sets="docinfo-label">
                    <xsl:value-of select="$label-text"/>
                </fo:block>
            </fo:table-cell>
            <fo:table-cell xsl:use-attribute-sets="docinfo-value-cell">
                <xsl:choose>
                    <xsl:when test="$has-children = 'True'">
                        <xsl:apply-templates/>
                    </xsl:when>
                    <xsl:when test="$address = 'True'">
                        <fo:block xsl:use-attribute-sets = "address-value">
                            <xsl:apply-templates/>
                        </fo:block>
                    </xsl:when>
                    <xsl:otherwise>
                        <fo:block xsl:use-attribute-sets = "docinfo-value">
                            <xsl:apply-templates/>
                        </fo:block>
                    </xsl:otherwise>
                </xsl:choose>
            </fo:table-cell>
        </fo:table-row>
    </xsl:template>

    <xsl:template match="docinfo/author">
        <xsl:call-template name="make-docinfo-row">
            <xsl:with-param name="label-text" select="$author-text"/>
            <xsl:with-param name="role" select="'author'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/authors">
        <xsl:call-template name="make-docinfo-row">
            <xsl:with-param name="label-text" select="$authors-text"/>
            <xsl:with-param name="role" select="'author'"/>
            <xsl:with-param name="has-children" select="'True'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/authors/author">
        <fo:block xsl:use-attribute-sets = "docinfo-value">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/organization">
        <xsl:call-template name="make-docinfo-row">
            <xsl:with-param name="label-text" select="$organization-text"/>
            <xsl:with-param name="role" select="'organization'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/contact">
        <xsl:call-template name="make-docinfo-row">
            <xsl:with-param name="label-text" select="$contact-text"/>
            <xsl:with-param name="role" select="'contact'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/status">
        <xsl:call-template name="make-docinfo-row">
            <xsl:with-param name="label-text" select="$status-text"/>
            <xsl:with-param name="role" select="'status'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/copyright">
        <xsl:call-template name="make-docinfo-row">
            <xsl:with-param name="label-text" select="$copyright-text"/>
            <xsl:with-param name="role" select="'copyright'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/address">
        <xsl:call-template name="make-docinfo-row">
            <xsl:with-param name="label-text" select="$address-text"/>
            <xsl:with-param name="role" select="'address'"/>
            <xsl:with-param name="address" select="'True'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/version">
        <xsl:call-template name="make-docinfo-row">
            <xsl:with-param name="label-text" select="$version-text"/>
            <xsl:with-param name="role" select="'version'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="docinfo/date">
        <xsl:call-template name="make-docinfo-row">
            <xsl:with-param name="label-text" select="$version-text"/>
            <xsl:with-param name="role" select="'version'"/>
        </xsl:call-template>
    </xsl:template>

    <!--custom bibliographic fields-->

    <xsl:template match="document/docinfo/field">
        <xsl:call-template name="make-docinfo-row">
            <xsl:with-param name="label-text">
                <xsl:value-of select="field_name"/>
            </xsl:with-param>
            <xsl:with-param name="role" select="'generic-field'"/>
            <xsl:with-param name="has-children" select="'True'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="document/docinfo/field/field_name">
    </xsl:template>

    <xsl:template match="document/docinfo/field/field_body">
        <xsl:apply-templates/>
    </xsl:template>

    <!--the value of the field goes to the right of the table-->
    <xsl:template match="docinfo/field/field_body/paragraph" priority="2">
        <fo:block xsl:use-attribute-sets = "docinfo-value">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <!--abstract and dedication-->
    <!--ony process if not already processed in front matter-->
    <xsl:template match="topic[@classes='dedication']">
        <xsl:if test="$page-sequence-type = 'body' or $page-sequence-type = 'toc-body'">
            <xsl:apply-templates/>
        </xsl:if> 
    </xsl:template>

    <xsl:template match="topic[@classes='dedication']" mode="front">
        <fo:block role="dedication" xsl:use-attribute-sets="dedication">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="topic[@classes='dedication']/title">
        <fo:block role="dedication-title" xsl:use-attribute-sets="dedication-title">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="topic[@classes='dedication']/paragraph">
        <fo:block role="dedication-paragraph" xsl:use-attribute-sets="dedication-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


    <xsl:template match="topic[@classes='abstract']">
        <xsl:if test="$page-sequence-type = 'body' or $page-sequence-type = 'toc-body'">
            <xsl:apply-templates/>
        </xsl:if> 
    </xsl:template>

    <xsl:template match="topic[@classes='abstract']" mode="front">
        <fo:block role="abstract" xsl:use-attribute-sets="abstract">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="topic[@classes='abstract']/title">
        <fo:block role="abstract-title" xsl:use-attribute-sets="abstract-title">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="topic[@classes='abstract']/paragraph">
        <fo:block role="abstract-paragraph" xsl:use-attribute-sets = "abstract-paragraph">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>
    
</xsl:stylesheet>
