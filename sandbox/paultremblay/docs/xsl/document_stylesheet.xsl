<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:include href="root.xsl"/>
    <xsl:include href="page.xsl"/>
    <xsl:include href="bibliographic_fields.xsl"/>
    <xsl:include href="front_matter.xsl"/>
    <xsl:include href="header_footer.xsl"/>
    <xsl:include href="toc.xsl"/>
    <xsl:include href="section.xsl"/>
    <xsl:include href="body_elements.xsl"/>
    <xsl:include href="bullet_list.xsl"/>
    <xsl:include href="enumerated_list.xsl"/>
    <xsl:include href="option_list.xsl"/>
    <xsl:include href="definition_list.xsl"/>
    <xsl:include href="field_list.xsl"/>
    <xsl:include href="line_block.xsl"/>
    <xsl:include href="table.xsl"/>
    <xsl:include href="table_extended.xsl"/>
    <xsl:include href="table_extended2.xsl"/>
    <xsl:include href="table_long.xsl"/>
    <xsl:include href="footnote.xsl"/>
    <xsl:include href="admonitions.xsl"/>
    <xsl:include href="image_figure.xsl"/>
    <xsl:include href="body_directives.xsl"/>
    <xsl:include href="inline.xsl"/>
    <xsl:include href="comment.xsl"/>
    <xsl:include href="utils.xsl"/>

    <xsl:output method="xml"/>

    <xsl:template match="xsl:stylesheet">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="/">
        <doc>
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">1</xsl:with-param>
                <xsl:with-param name="text">XSL-FO Documentation</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">2</xsl:with-param>
                <xsl:with-param name="text">Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>.. contents:: Table of Contents</block>

            <!--root attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Root Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets root elements. Use these attribute sets to format
                the defaults in a document, such as font, font-size, or line-height.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/root.xsl')/xsl:stylesheet"/>

            <xsl:call-template name="make-title">
            <!--page attribute sets-->
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Page Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for page. These attributes control the formatting of 
                the actual pages: the paper size and margins.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/page.xsl')/xsl:stylesheet"/>

            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Bibliograhic Fields Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the bibliograhic fields. These attributes control the formatting of 
               bibliographic fields.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/bibliographic_fields.xsl')/xsl:stylesheet"/>

            <!--front matter attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Front Matter Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the dedication and abstract. 
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/front_matter.xsl')/xsl:stylesheet"/>

            <!--header footer-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Header and Footer Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the headers and footers. Since headers and footers often need
                very special formatting, the stylesheets allow for the formatting of up to three
                paragraphs for each header and footer. The first refers to the first that occurrs in 
                the document, the second to the second, and the third to the third.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/header_footer.xsl')/xsl:stylesheet"/>

            <!--TOC attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">TOC Matter Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the TOC. 
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/toc.xsl')/xsl:stylesheet"/>

            <!--Section attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Section Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the section titles. 
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/section.xsl')/xsl:stylesheet"/>

            <!--body-element attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Body Elements</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for body elements, including the document title and subtitle; the
                default paragraph; the transition element; and the literal block.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/body_elements.xsl')/xsl:stylesheet"/>

            <!--bullet-list attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">bullet list</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the bullet list.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/bullet_list.xsl')/xsl:stylesheet"/>

            <!--enumerated-list attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">enumerated list</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the enumerated list.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/enumerated_list.xsl')/xsl:stylesheet"/>

            <!--definition-list attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">definition list</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the definition list.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/definition_list.xsl')/xsl:stylesheet"/>

            <!--field-list attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">field list</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the field list.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/field_list.xsl')/xsl:stylesheet"/>

            <!--option-list attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">option list as list</xsl:with-param>
            </xsl:call-template>
            <block>
                Since an option list can be rendered as either a traditonal list, or a
                definition list, there are two sets of attribute sets. These attribute sets
                are used for the options list when it is rendered as a list.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/option_list.xsl')/xsl:stylesheet"/>

            <!--line-block attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Line Block</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the line block.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/line_block.xsl')/xsl:stylesheet"/>

            <!--table attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Table</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the Table.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/table.xsl')/xsl:stylesheet"/>

            <!--table extended attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Table Extended</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the Extended Tables.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/table_extended.xsl')/xsl:stylesheet"/>

            <!--table extended 2 attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Table Extended 2 (Borderless)</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the Extended 2 (Borderless) Tables.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/table_extended2.xsl')/xsl:stylesheet"/>

            <!--table long attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Table Long</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the Long Tables.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/table_long.xsl')/xsl:stylesheet"/>




            <!--footnote attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Footnote</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for footnotes, endnotes, and the endnotes title.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/footnote.xsl')/xsl:stylesheet"/>

            <!--admonitions attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Admonitions</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for Admonitions. By default, the admontioins have a 
                border around them. Each admonition gets its title from the parameter
                of that name. For example, the danger admonitions title gets its title
                from the 'danger-title' parameter, the caution from the `caution-title`
                paramter, and so fourth.
            </block>
            <block>
                Although each admonition and each admonition title has its own attribute-set,
                by default they all inherit these values from two default attribute sets. (See
                below.) Each of these areas can thus be customized. In contrast, all the paragrahs
                in each admonition are identical.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/admonitions.xsl')/xsl:stylesheet"/>

            <!--image and figure attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Image and Figure</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for Images and Figures. The image property of
                ``alt`` and ``target`` are ignored by the stylesheets, since
                they have no use in PDF. In addtion, if the ``align`` is set
                to ``top`` or ``bottom``, both properties that have no meaning
                for PDF, the stylesheets will report an error, and if
                ``strict`` is set to ``true``, quit. 
            </block>

            <block>
                Likwise, the figure ``figwidth`` property will be ignored, since there 
                is not way to implement this property directy in FO. 
            </block>
            <block>
                In order to control the scaling, alignment, and width of images and
                figures, it is better to use the attribute sets than to try to set these
                properties in RST. The regions of 'image', 'figure', 'caption', and 'legend'
                are all wrapped in blocks. Use the attribute sets for these blocks to control the 
                properties.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/image_figure.xsl')/xsl:stylesheet"/>

            <!--body_directive attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Body Elements Directives</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for Body Elements Directives. 
            </block>

            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/body_directives.xsl')/xsl:stylesheet"/>

            <!--option-inline attribute sets-->
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Inline</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for all the inline elements. The parameter 'footnote-style' 
                controls the style of the footnote. The paramater 'footnote-placement' determines
                whether the footnotes that are numbered will be placed as footnotes or endnotes. 
            </block>
            <block>
                The parameter 'space-between-foototes' controls the space between footnotes. 
                (Becuase of a flaw(?) in FOP, an attribute set could not be used.) This parameter
                has no effect on the space between endnotes.
            </block>
            <xsl:apply-templates select="document('../../docutilsToFo/xsl_fo/inline.xsl')/xsl:stylesheet"/>
        </doc>
    </xsl:template>

    <xsl:template match="xsl:attribute-set" priority="2">
        <xsl:message>
            <xsl:text>no match for "</xsl:text>
            <xsl:value-of select="@name"/>
            <xsl:text>"</xsl:text>
        </xsl:message>
    </xsl:template>

    <xsl:template match="@*|node()"/>

    
</xsl:stylesheet>
