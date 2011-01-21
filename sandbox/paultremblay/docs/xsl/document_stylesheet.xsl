<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:include href="utils.xsl"/>
    <xsl:include href="option_list.xsl"/>
    <xsl:include href="root.xsl"/>
    <xsl:include href="comment.xsl"/>

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
