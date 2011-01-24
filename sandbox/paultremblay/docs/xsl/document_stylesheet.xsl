<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:include href="root.xsl"/>
    <xsl:include href="page.xsl"/>
    <xsl:include href="bibliographic_fields.xsl"/>
    <xsl:include href="front_matter.xsl"/>
    <xsl:include href="toc.xsl"/>
    <xsl:include href="bullet_list.xsl"/>
    <xsl:include href="enumerated_list.xsl"/>
    <xsl:include href="option_list.xsl"/>
    <xsl:include href="definition_list.xsl"/>
    <xsl:include href="field_list.xsl"/>
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

            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Root Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets root elements. Use these attribute sets to format
                the defaults in a document, such as font, font-size, or line-height.
            </block>
            <xsl:apply-templates select="document('../../xsl_fo/root.xsl')/xsl:stylesheet"/>

            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Page Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for page. These attributes control the formatting of 
                the actual pages: the paper size and margins.
            </block>
            <xsl:apply-templates select="document('../../xsl_fo/page.xsl')/xsl:stylesheet"/>

            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Bibliograhic Fields Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the bibliograhic fields. These attributes control the formatting of 
               bibliographic fields.
            </block>
            <xsl:apply-templates select="document('../../xsl_fo/bibliographic_fields.xsl')/xsl:stylesheet"/>

            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">Front Matter Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the dedication and abstract. 
            </block>
            <xsl:apply-templates select="document('../../xsl_fo/front_matter.xsl')/xsl:stylesheet"/>

            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">TOC Matter Attribute Sets</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the TOC. 
            </block>
            <xsl:apply-templates select="document('../../xsl_fo/toc.xsl')/xsl:stylesheet"/>

            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">bullet list</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the bullet list.
            </block>
            <xsl:apply-templates select="document('../../xsl_fo/bullet_list.xsl')/xsl:stylesheet"/>

            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">enumerated list</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the enumerated list.
            </block>
            <xsl:apply-templates select="document('../../xsl_fo/enumerated_list.xsl')/xsl:stylesheet"/>

            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">definition list</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the definition list.
            </block>
            <xsl:apply-templates select="document('../../xsl_fo/definition_list.xsl')/xsl:stylesheet"/>

            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">field list</xsl:with-param>
            </xsl:call-template>
            <block>
                Attribute sets for the field list.
            </block>
            <xsl:apply-templates select="document('../../xsl_fo/field_list.xsl')/xsl:stylesheet"/>

            <xsl:call-template name="make-title">
                <xsl:with-param name="level">3</xsl:with-param>
                <xsl:with-param name="text">option list as list</xsl:with-param>
            </xsl:call-template>
            <block>
                Since an option list can be rendered as either a traditonal list, or a
                definition list, there are two sets of attribute sets. These attribute sets
                are used for the options list when it is rendered as a list.
            </block>
            <xsl:apply-templates select="document('../../xsl_fo/option_list.xsl')/xsl:stylesheet"/>
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
