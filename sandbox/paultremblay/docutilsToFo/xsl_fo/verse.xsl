<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:str="http://exslt.org/strings"
    extension-element-prefixes="str"
    version="1.1"
    >

    <xsl:attribute-set name="stanza-title-block">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="space-before">12</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>



    <xsl:template name="count-lines">
        <xsl:variable name="top-block-id">
            <xsl:for-each select="ancestor::line_block[last()]">
                <xsl:value-of select="generate-id()"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:number from="line_block[generate-id() = $top-block-id]" 
            count="line[normalize-space(.) != ''][not(contains(., 'rst-title'))]" level="any"/>
    </xsl:template>

    <xsl:template match="line[contains(., 'rst-title: ')]">
        <fo:block xsl:use-attribute-sets="stanza-title-block" role="stanza-title">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="line[contains(., 'rst-title:')]/text()">
        <xsl:value-of select="str:replace(., 'rst-title:', '')"/>
    </xsl:template>




</xsl:stylesheet>
