<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template match="section[not(@classes = 'appendix')]">
        <xsl:choose >
            <xsl:when test="$docbook-type = 'book' and
                not(parent::section)">
                <xsl:call-template name = "make-chapter"/>
                
            </xsl:when>
            <xsl:otherwise test="$docbook-type = 'article'">
                <xsl:call-template name = "make-section"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <xsl:template name="make-section">
        <xsl:call-template name="page-break-before-section">
            <xsl:with-param name="classes" select="@classes"/>
        </xsl:call-template>
        <xsl:element name="d:section">
            <xsl:call-template name="make-id"/>
            <xsl:if test="@classes">
                <xsl:attribute name="role">
                    <xsl:value-of select="@classes"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates/>
        </xsl:element>
        <xsl:call-template name="page-break-after-section">
            <xsl:with-param name="classes" select="@classes"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="make-chapter">
        <xsl:element name="d:chapter">
            <xsl:call-template name="make-id"/>
            <xsl:if test="@classes">
                <xsl:attribute name="role">
                    <xsl:value-of select="@classes"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

        

    <xsl:template match="section/title">
        <d:title>
            <xsl:apply-templates/>
        </d:title>
    </xsl:template>


    <xsl:template match="section/title/generated"/>

    <xsl:template name="page-break-before-section">
        <xsl:param name="classes"/>
    </xsl:template>

    <xsl:template name="page-break-after-section">
        <xsl:param name="classes"/>
    </xsl:template>

</xsl:stylesheet>
