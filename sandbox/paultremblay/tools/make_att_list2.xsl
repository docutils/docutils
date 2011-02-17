<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:str="http://exslt.org/strings"
    extension-element-prefixes="str"


    version="1.1"
    >
    
    <xsl:template match = "root">
        <root>
        <xsl:apply-templates select="define/define/define/define/define/define/define"/>
        <xsl:apply-templates select="define/define/define/define/define/define"/>
        <xsl:apply-templates select="define/define/define/define/define"/>
        <xsl:apply-templates select="define/define/define/define"/>
        <xsl:apply-templates select="define/define/define"/>
        <xsl:apply-templates select="define/define"/>
        <xsl:apply-templates select="define"/>
        </root>
    </xsl:template>

    <xsl:template match="define" priority="2">
        <xsl:variable name="name" select="str:replace(@name, '-', '_')"/>
        <define name="{@name}">
                <xsl:apply-templates select="attribute"/>
        </define>
        <xsl:for-each select="define">
            <xsl:variable name="inherit-name" select="str:replace(@name, '-', '_')"/>
            <extend name = "{$name}" inherit-name = "{$inherit-name}"/>
        </xsl:for-each>
        <xsl:apply-templates select="define"/>
    </xsl:template>


    <xsl:template match ="define/define/define/define/define/define/define">
        <xsl:message terminate = "yes">Too deep quiting.</xsl:message>
    </xsl:template>


    <xsl:template match="attribute" priority="2">
        <attribute name="{@name}">
            <xsl:apply-templates/>
        </attribute>
    </xsl:template>

    <xsl:template match="@*|node()"/>


</xsl:stylesheet> 
