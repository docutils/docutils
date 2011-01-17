<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
>
    <!-- $Id:$ -->

    <xsl:include href = "parameters.xsl"/>
    <xsl:include href = "page.xsl"/>
    <xsl:include href = "header_footer.xsl"/>
    <xsl:include href = "section.xsl"/>
    <xsl:include href = "body_elements.xsl"/>
    <xsl:include href = "root.xsl"/>
    <xsl:include href = "inline.xsl"/>
    <xsl:include href = "misc.xsl"/>
    <xsl:include href = "lists.xsl"/>
    <xsl:include href = "util.xsl"/>
    <xsl:include href = "toc.xsl"/>
    <xsl:include href = "bibliographic_fields.xsl"/>
    <xsl:include href = "error.xsl"/>




    <xsl:output method="xml" encoding="UTF-8"/>

    <xsl:template match="/">
        <xsl:element name="fo:root">
            <xsl:call-template name="make-pages">
                <xsl:with-param name="page-layout" select="$page-layout"/>
            </xsl:call-template>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>


    <xsl:template match="*">
        <xsl:message>
            <xsl:text>no match for </xsl:text>
            <xsl:value-of select="name(.)"/>
        </xsl:message>
        <xsl:choose>
            <xsl:when test="$strict='True'">
                <xsl:message terminate="yes">
                    <xsl:text>Processing XSLT Stylesheets now quiting</xsl:text>
                </xsl:message>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message>
                    <xsl:text>Not processing test in this element.</xsl:text>
                </xsl:message>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


</xsl:stylesheet>

