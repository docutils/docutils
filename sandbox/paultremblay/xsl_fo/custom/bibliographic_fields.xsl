<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    

    <xsl:attribute-set name="author-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="authors-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="date-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="organization-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="contact-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="status-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="copyright-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="version-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="revision-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="address-block">
        <xsl:attribute name="white-space">pre</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="Some-custom-field">
    </xsl:attribute-set>

    <xsl:template match="docinfo">
        <xsl:apply-templates/> 
    </xsl:template>

    <xsl:template match="docinfo/author|docinfo/authors/author">
        <fo:block role="author" xsl:use-attribute-sets="author-block">
            <xsl:value-of select="$author-text"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/authors">
        <fo:block role="authors" xsl:use-attribute-sets="authors-block">
            <xsl:value-of select="$authors-text"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/date">
        <fo:block role="date" xsl:use-attribute-sets="date-block">
            <xsl:value-of select="$date-text"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/organization">
        <fo:block role="organization" xsl:use-attribute-sets="organization-block">
            <xsl:value-of select="$organization-text"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/contact">
        <fo:block role="contact" xsl:use-attribute-sets="contact-block">
            <xsl:value-of select="$contact-text"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/status">
        <fo:block role="status" xsl:use-attribute-sets="status-block">
            <xsl:value-of select="$status-text"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/copyright">
        <fo:block role="copyright" xsl:use-attribute-sets="copyright-block">
            <xsl:value-of select="$copyright-text"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/version">
        <fo:block role="version" xsl:use-attribute-sets="version-block">
            <xsl:value-of select="$version-text"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/revision">
        <fo:block role="revision" xsl:use-attribute-sets="revision-block">
            <xsl:value-of select="$revision-text"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/address">
        <fo:block role="address" xsl:use-attribute-sets="address-block">
            <xsl:value-of select="$address-text"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/field[field_name = 'Some-custom-field']" priority="2">
        <fo:block role="Some-custom-field" xsl:use-attribute-sets="Some-custom-field">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>
    
    <xsl:template match="docinfo/field">
        <fo:block role="custom-bib-info" xsl:use-attribute-sets="custom-bib-info">
            <xsl:value-of select="field_name"/>
            <xsl:text>:</xsl:text>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="docinfo/field/field_name"/>


    <xsl:template match="docinfo/field/field_body">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="docinfo/field/field_body/paragraph" priority="2">
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

