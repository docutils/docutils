<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template name="make-info">
        <d:info>
            <d:title>
                <xsl:if test="not(docinfo) and not(title)">
                    <xsl:value-of select="@source"/>
                </xsl:if>
                <xsl:apply-templates select="title" mode="with-info"/>
            </d:title>
            <xsl:apply-templates select="docinfo" mode="with-info"/>
        </d:info>
    </xsl:template>

    <xsl:template match="/document/title|/document/docinfo|document/docinfo/organization"/>

    <xsl:template match="/document/title" mode="with-info">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="document/docinfo" mode="with-info">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="document/docinfo/author">
        <d:author>
            <d:personname>
                <xsl:apply-templates/>
            </d:personname>
            <xsl:apply-templates select="../organization" mode="with-author"/>
        </d:author>
    </xsl:template>

    <xsl:template match="organization" mode="with-author">
        <d:affiliation>
            <d:orgname>
                <xsl:apply-templates/>
            </d:orgname>
        </d:affiliation>
    </xsl:template>
    
</xsl:stylesheet>
