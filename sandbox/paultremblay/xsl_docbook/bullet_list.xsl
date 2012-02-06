<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template match="bullet_list">
        <d:itemizedlist>
            <xsl:apply-templates/>
        </d:itemizedlist>
    </xsl:template>

    <xsl:template match="enumerated_list">
        <d:orderedlist numeration="{@enumtype}">
            <xsl:apply-templates/>
        </d:orderedlist>
    </xsl:template>

    <xsl:template match="list_item">
        <d:listitem>
            <xsl:apply-templates/>
        </d:listitem>
    </xsl:template>

    
</xsl:stylesheet>
