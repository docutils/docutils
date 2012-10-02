<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template match="figure[image/@alt]" priority="2">
        <d:figure>
            <xsl:call-template name="make-id"/>
            <d:title>
                <xsl:value-of select="image/@alt"/>
            </d:title>
            <xsl:apply-templates/>
        </d:figure>
        <xsl:apply-templates select="legend" mode="after-figure"/>
    </xsl:template>
    
    <xsl:template match="figure">
        <d:informalfigure>
            <xsl:call-template name="make-id"/>
            <xsl:apply-templates/>
        </d:informalfigure>
        <xsl:apply-templates select="legend" mode="after-figure"/>
    </xsl:template>


    <xsl:template match="image">
        <xsl:variable name="format">
            <xsl:call-template name="get-fileformat">
                <xsl:with-param name="uri" select="@uri"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="uri">
            <xsl:choose>
                <xsl:when test="$draft_image != ''">
                    <xsl:variable name="basename" select="substring-before(@uri, '.')"/>
                    <xsl:variable name="ext" select="substring-after(@uri, '.')"/>
                    <xsl:value-of select="concat($basename, $draft_image, '.', $ext)"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="@uri"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <d:mediaobject>
            <d:imageobject>
                <xsl:variable name="path" select="$uri"/>
                <d:imagedata  format="{$format}" fileref="{$path}" width="{$image-width}"/>
            </d:imageobject>
        </d:mediaobject>
    </xsl:template>


    <xsl:template match="caption">
        <xsl:element name="d:caption">
            <d:para>
                <xsl:apply-templates/>
            </d:para>
        </xsl:element>
    </xsl:template>

    <xsl:template match="legend"/>

    <xsl:template match="legend" mode="after-figure">
                <xsl:apply-templates/>
    </xsl:template>
    
</xsl:stylesheet>
