<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:str="http://exslt.org/strings"
    extension-element-prefixes="str"
    version="1.1" >
    
    <xsl:template match="root">
        <root>
            <xsl:apply-templates select="attribute-set">
                <xsl:sort select="@name"/>
            </xsl:apply-templates>
            <xsl:apply-templates select="extend"/>
        </root>
    </xsl:template>

    <xsl:template match="attribute-set">
        <xsl:copy-of select="."/>
    </xsl:template>

    <xsl:template match="extend">
        <xsl:variable name="name" select="@name"/>
        <xsl:variable name="inherit-name" select="@inherit-name"/>
        <xsl:choose>
            <xsl:when test="preceding::extend[@name = $name][@inherit-name = $inherit-name]">
            </xsl:when>
            <xsl:otherwise>
                <xsl:copy-of select="."/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

</xsl:stylesheet> 
