<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='bibliographic-fields-list-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">list-block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the bibliographic fields as a list. Since this element contains all the other list
            elements, it can be used to set values such as the font, background color,
            line-height, etc, for the entire list, as well as the space after and
            before.
        </block>
        <block>
            "The provisional-distance-between-starts property of the list-block
            specifies the distance bewteen the start of the label (the bullet, for
            example) and the actual start of the list content" (Pawson, 100). In 
            this case, that means the distance between the label (such as "Version",
            and the labels' value (such as "1.2").
        </block>
    </xsl:template>
    <xsl:template match= "xsl:attribute-set[@name='bibliographic-fields-front-list-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">list-block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo</xsl:with-param> 
            <xsl:with-param name="inherits">bibliographic-fields-list-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            The attribute set has a single default attribute, 'break-after="page"', and inherits the rest
            of the properties from bibliographic-fields-list-block. It is applied
            *only* when the bibliographic-fields is located in the front section. If you did not wish 
            for a page break, create this attribute set with no attributes. 
        </block>
    </xsl:template>



    <xsl:template match= "xsl:attribute-set[@name='bibliographic-fields-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/author|docinfo/authors|docinfo/organization|etc.</xsl:with-param> 
        </xsl:call-template>
        <block>
            For the each item (author, authors, organization, contact, address, version, date, copyright, 
            custom field) in the bibliograhic fields. Use the 'space-after' attribute to control
            the spacing between each item.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='bibliographic-fields-list-item-label']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-label</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/author|docinfo/authors|docinfo/organization|etc.</xsl:with-param> 
        </xsl:call-template>
        <block>
            The default attribute end-indent = "label-end()" ensures that the label aligns properly.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='bibliographic-fields-item-label-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/author|docinfo/authors|docinfo/organization|etc.</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the block that wraps the the name of the field (such as Author, Version, etc).
        </block>
    </xsl:template>



    <xsl:template match= "xsl:attribute-set[@name='bibliographic-fields-item-body']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item-body</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/author|docinfo/authors|docinfo/organization|etc.</xsl:with-param> 
        </xsl:call-template>
        <block>
            The default of start-indent = "body-start()" ensures the correct 
            alignment of the labels.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='bibliographic-fields-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/author|docinfo/authors|docinfo/organization|etc.</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the blocks (docutilis paragraphs) of the value of the field. Use the 
            'space-after' attribute to control the spacing between a multi-paragraph 
            description.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='address-value-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/address</xsl:with-param> 
            <xsl:with-param name="inherits">bibliographic-fields-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the blocks (docutilis paragraphs) of the address field, which has to preserve 
            the white space, according to the docutils specs. Since this inherits from the 
            bibliographic-fields-bloc, it doesn't make sense to change attributes here directly.
        </block>
    </xsl:template>


</xsl:stylesheet>
