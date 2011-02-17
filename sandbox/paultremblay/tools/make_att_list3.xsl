<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:str="http://exslt.org/strings"
    extension-element-prefixes="str"
    version="1.1"
    >
    
    <xsl:template match = "root" priority = "2">
        <root>
            <xsl:apply-templates select="define"/>
            <xsl:apply-templates select="extend"/>
        </root>
    </xsl:template>

    <xsl:template match="attribute|extend" priority="2">
        <xsl:copy-of select="."/>
    </xsl:template>


    <xsl:template match="define" priority="2">
        <xsl:variable name="name" select="str:replace(@name, '-', '_')"/>
        <xsl:variable name="a-name" select="@name"/>
        <xsl:if test = "not(preceding::define[@name = $a-name])">
            <attribute-set name="{$name}">
                <xsl:apply-templates/>
            </attribute-set>
        </xsl:if>
    </xsl:template>

    <xsl:template match="extend_" priority="2">
        <xsl:variable name="name" select="@name"/>
        <xsl:if test = "not(preceding::extend[@name = $name])">
            <xsl:copy-of select="."/>
        </xsl:if>
    </xsl:template>

    <xsl:template match="*">
        <xsl:message>
            No match for
            <xsl:value-of select="name(.)"/>
        </xsl:message>
    </xsl:template>

</xsl:stylesheet> 
