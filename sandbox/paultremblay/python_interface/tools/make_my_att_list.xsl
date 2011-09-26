<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <xsl:output method="text"/>

    <xsl:template match="/|/xsl:stylesheet" priority="2">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="xsl:attribute-set" priority="2">
        <xsl:variable name="name" select="@name"/>
        <xsl:variable name="element">
            <xsl:for-each select="/">
                <xsl:for-each select="descendant::*[@xsl:use-attribute-sets = $name][1]">
                    <xsl:value-of select="local-name(.)"/>
                </xsl:for-each>
            </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="type">
            <xsl:choose>
                
                <xsl:when test="$name = 'default-cell'">
                    <xsl:text>table-cell</xsl:text>
                </xsl:when>
                <xsl:when test="$name = 'default-flow'">
                    <xsl:text>flow</xsl:text>
                </xsl:when>
                <xsl:when test="$name = 'default-page-sequence'">
                    <xsl:text>page-sequence</xsl:text>
                </xsl:when>
                <xsl:when test="$name = 'paper-size-simple-page-master' or
                    $name =  'default-simple-page-master'
                    
                    ">
                    <xsl:text>simple-page-master</xsl:text>
                </xsl:when>
                <xsl:when test="$name = 'default-footnote-label-inline'">
                    <xsl:text>inline</xsl:text>
                </xsl:when>
                <xsl:when test="$name = 'default-admonition-outer-block' or
                    $name = 'default-admonition-title-block' or 
                    $name = 'default-section-title-block' or
                    $name = 'toc-entry-defaults-block'
                    ">
                    <xsl:text>block</xsl:text>
                </xsl:when>
                <xsl:when test = "not(normalize-space($element))">
                    <xsl:message terminate = "no">
                        <xsl:text>No match for </xsl:text>
                        <xsl:value-of select="$name"/>
                        <xsl:text>&#xA;</xsl:text>
                    </xsl:message>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$element"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="$name"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="$type"/>
        <xsl:text>&#xA;</xsl:text>
    </xsl:template>

    <xsl:template match="@*|node()"/>

    
</xsl:stylesheet>

