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

    <xsl:template match="define" priority="2">
        <!--
        <xsl:variable name="name" select="str:replace(@name, '-', '_')"/>
        -->
        <xsl:value-of select="$name"/>
        <xsl:text>  = [ &#xA; </xsl:text>
        <xsl:apply-templates select="attribute"/>
        <xsl:text>]&#xA; </xsl:text>
        <xsl:for-each select="define">
            <xsl:value-of select = "$name"/>
            <xsl:text>.extend(</xsl:text>
            <xsl:variable name="inherit-name" select="str:replace(@name, '-', '_')"/>
            <xsl:value-of select="$inherit-name"/>
            <xsl:text>)&#xA;</xsl:text>
        </xsl:for-each>
        <xsl:apply-templates select="define"/>
    </xsl:template>

    <xsl:template match="attribute" priority="2">
        <xsl:text>'</xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:text>',&#xA;</xsl:text>
    </xsl:template>

    <xsl:template match="@*|node()"/>


</xsl:stylesheet> 
