<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template match="section[@classes='glossary']" mode="top">
        <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="section[@classes='glossary']"/>

    <xsl:template match="field_list">
        <d:glosslist>
            <xsl:apply-templates/>
        </d:glosslist>
    </xsl:template>

    <xsl:template match="field">
        <d:glossentry>
            <xsl:apply-templates/>
        </d:glossentry>
    </xsl:template>

    <xsl:template match="field_name">
        <d:glossterm>
            <xsl:apply-templates/>
        </d:glossterm>
    </xsl:template>

    <xsl:template match="field_body">
        <d:glossdef>
            <xsl:apply-templates/>
        </d:glossdef>
    </xsl:template>

    <xsl:template match="field_body/paragraph">
        <d:para>
            <xsl:apply-templates/>
        </d:para>
    </xsl:template>

    <xsl:template match="section[@classes='glossary']/title" priority="2"/>

    
</xsl:stylesheet>
