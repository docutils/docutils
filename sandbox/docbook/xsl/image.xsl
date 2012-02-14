<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:param name="image-width">5in</xsl:param>
    <xsl:template match="figure">
        <d:figure>
            <xsl:if test="caption/target">
                <xsl:attribute name="xml:id">
                    <xsl:value-of select="caption/target/@ids"/>
                </xsl:attribute>
            </xsl:if>
            <d:title>
                <xsl:value-of select="image/@alt"/>
            </d:title>
                <xsl:apply-templates/>
        </d:figure>
        <xsl:apply-templates select="legend" mode="after-figure"/>
    </xsl:template>
    


    <xsl:template match="image">
        <xsl:variable name="format">
            <xsl:text >PNG</xsl:text>
        </xsl:variable>
        <d:mediaobject>
            <d:imageobject>
                <xsl:variable name="path" select="@uri"/>
                <d:imagedata  format="{$format}" fileref="{$path}" width="{$image-width}"/>
                <!--
                <imagedata align="right" width="6in" format="PNG" fileref="figs/web/duck-small.png"/>
                -->
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
