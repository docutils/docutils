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


    <xsl:template match= "xsl:attribute-set[@name='bibliographic-fields-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/author|docinfo/authors|docinfo/organization|etc.</xsl:with-param> 
        </xsl:call-template>
        <block>
            For each item (author, authors, organization, contact, address, version, date, copyright, 
            custom field) in the bibliograhic fields. Use the 'space-after' attribute to control
            the spacing between each item.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='bibliographic-fields-first-list-item']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:list-item</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/author|docinfo/authors|docinfo/organization|etc.</xsl:with-param> 
            <xsl:with-param name="inherits">bibliographic-fields-list-item</xsl:with-param> 
        </xsl:call-template>
        <block>
            Same as above, but sets the space before to 0pt.
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

    <xsl:template match= "xsl:attribute-set[@name='bibliographic-first-fields-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/author|docinfo/authors|docinfo/organization|etc.</xsl:with-param> 
            <xsl:with-param name="inherits">bibliographic-fields-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Same as above, but for the first such paragraph.
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

    <xsl:template match= "xsl:attribute-set[@name='author-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/author</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the author element, when bibliograhic info is not formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='authors-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/authors</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the authors element, when bibliograhic info is not formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='date-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/date</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the date element, when bibliograhic info is not formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='organization-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/organization</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the organization element, when bibliograhic info is not formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='contact-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/contact</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the contact element, when bibliograhic info is not formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='status-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/status</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the status element, when bibliograhic info is not formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='copyright-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/copyright</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the copyright element, when bibliograhic info is not formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='version-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/version</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the version element, when bibliograhic info is not formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='revision-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/revision</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the revision element, when bibliograhic info is not formatted as a list.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='address-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">docinfo/address</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the address element, when bibliograhic info is not formatted as a list.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='custom-bib-info1']" priority="3">
        <xsl:call-template name="make-title">
            <xsl:with-param name="level">5</xsl:with-param>
            <xsl:with-param name="text">Custom bibliographic fields</xsl:with-param>
        </xsl:call-template>
        <block>
            <xsl:text>:fo: fo:block</xsl:text> 
        </block>
        <block first-line-indent="-9">
            <xsl:text>:docutils: docinfo/field</xsl:text> 
        </block>
        <block>
            The following attribute sets are identical in nature:
        </block>
        <block>* custom-bib-info1</block>
        <block>* custom-bib-info2</block>
        <block>* custom-bib-info3</block>
        <block>* custom-bib-info4</block>
        <block>* custom-bib-info5</block>
        <block>* custom-bib-info6</block>
        <block>* custom-bib-info7</block>
        <block>* custom-bib-info8</block>
        <block>* custom-bib-info9</block>
        <block>* custom-bib-info10</block>
        <block>
            These attribute-sets format the custom bibliographic fields. ``'custom-bib-info1'`` refers to the first
            occurrence of such a field, ``'custom-bib-info2'`` to the second, and so fourth.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='custom-bib-info2']|
        xsl:attribute-set[@name='custom-bib-info3']|
        xsl:attribute-set[@name='custom-bib-info4']|
        xsl:attribute-set[@name='custom-bib-info5']|
        xsl:attribute-set[@name='custom-bib-info6']|
        xsl:attribute-set[@name='custom-bib-info7']|
        xsl:attribute-set[@name='custom-bib-info8']|
        xsl:attribute-set[@name='custom-bib-info9']|
        xsl:attribute-set[@name='custom-bib-info10'] " priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='Some-custom-field']" priority = "3"/>

</xsl:stylesheet>
