<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:g = "http://relaxng.org/ns/structure/1.0"
    xmlns:str="http://exslt.org/strings"
    extension-element-prefixes="str"


    version="1.1"
    >
    
    <xsl:output method="text"/>
    <xsl:template match = "root">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="attribute-set">
        <xsl:variable name="name" select="str:replace(@name, '.attlist', '_attlist')"/>
        <xsl:value-of select="$name"/>
        <xsl:text> = [&#xA;</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>]&#xA;</xsl:text>
    </xsl:template>


    <xsl:template match="attribute" priority="2">
        <xsl:text>'</xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:text>',&#xA;</xsl:text>
    </xsl:template>

    <xsl:template match="attribute[last()]" priority="3">
        <xsl:text>'</xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:text>'</xsl:text>
    </xsl:template>

    <xsl:template match="extend">
        <xsl:variable name="name" select="str:replace(@name, '.attlist', '_attlist')"/>
        <xsl:value-of select="$name"/>
        <xsl:text>.extend(</xsl:text>
        <xsl:variable name="inherit-name" select="str:replace(@inherit-name, '.attlist', '_attlist')"/>
        <xsl:value-of select="$inherit-name"/>
        <xsl:text>)&#xA;</xsl:text>
    </xsl:template>


    <xsl:template match="@*|node()"/>


</xsl:stylesheet> 
