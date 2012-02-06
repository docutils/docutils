<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

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
            <d:mediaobject>
                <xsl:apply-templates/>
            </d:mediaobject>
        </d:figure>
        <xsl:apply-templates select="legend" mode="after-figure"/>
    </xsl:template>
    


    <xsl:template match="figure/image">
        <d:imageobject>
            <xsl:variable name="path" select="@uri"/>
                <d:imagedata  format="PNG" fileref="{$path}"/>
            <!--
            <imagedata align="right" width="6in" format="PNG" fileref="figs/web/duck-small.png"/>
            -->
        </d:imageobject>
    </xsl:template>


    <xsl:template match="caption">
        <xsl:element name="d:caption">
            <xsl:if test="target">
                <!--id should probably be with figure or image-->
                <xsl:attribute name="xml:id">
                    <xsl:value-of select="target/@ids"/>
                </xsl:attribute>
            </xsl:if>
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
