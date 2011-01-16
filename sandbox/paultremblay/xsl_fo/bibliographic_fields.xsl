<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!--label and value-->
    <!-- $Id$ -->
    <!--Parameters for docinfo fields. These parameters are used to fill in the text before such
    fields as author, etc.`docutils`-->
    <xsl:param name="author-text">Author: </xsl:param>
    <xsl:param name="authors-text">Authors: </xsl:param>
    <xsl:param name="organization-text">Organization: </xsl:param>
    <xsl:param name="contact-text">Contact: </xsl:param>
    <xsl:param name="status-text">Status: </xsl:param>
    <xsl:param name="copyright-text">Copyright: </xsl:param>
    <xsl:param name="address-text">Address: </xsl:param>
    <xsl:param name="version-text">Version: </xsl:param>
    <xsl:param name="date-text">Date: </xsl:param>

    <!--Attribute sets for Bibliographic Fields-->

    <!--attribute set for Bibliographic table of all values.  Element is fo:table. `docutils`-->
    <xsl:attribute-set name="docinfo-table">
        <xsl:attribute name="inline-progression-dimension">6in</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for Bibliographic cells for labels.  Element is fo:table-cell. `docutils`-->
    <xsl:attribute-set name="docinfo-label-cell" >
        <xsl:attribute name="padding">5pt</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for Bibliographic cells for values.  Element is fo:table-cell. `docutils`-->
    <xsl:attribute-set name="docinfo-value-cell">
        <xsl:attribute name="padding">5pt</xsl:attribute>
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


    <xsl:template match="docinfo">
        <fo:table  table-layout="fixed" xsl:use-attribute-sets="docinfo-table">
            <fo:table-column column-number="1" xsl:use-attribute-sets="docinfo-label-row"/>
            <fo:table-column column-number="2" xsl:use-attribute-sets="docinfo-value-row"/>
            <fo:table-body>
                <xsl:apply-templates/>
            </fo:table-body>
        </fo:table>
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

    <!--longer bibliographic elements-->

    <xsl:template match="topic[@classes='dedication']">
        
    </xsl:template>

    <xsl:template match="topic[@classes='dedication']/title">
        
    </xsl:template>

    <xsl:template match="topic[@classes='dedication']/paragraph">
        
    </xsl:template>

    <xsl:template match="topic[@classes='abstract']">
        
    </xsl:template>

    <xsl:template match="topic[@classes='abstract']/title">
        
    </xsl:template>

    <xsl:template match="topic[@classes='abstract']/paragraph">
        
    </xsl:template>
    
</xsl:stylesheet>
