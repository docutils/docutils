<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <!--RST elements:
         attention => sidebar
         caution => caution
         danger => warning
         error => errorcode
         hint => tip
         important => important   
         note => note
         tip => tip
         warning = warning>

         -->
    <xsl:template match="attention">
        <d:sidebar>
            <xsl:apply-templates/>
        </d:sidebar>
    </xsl:template>

    <xsl:template match="caution">
        <d:caution>
            <xsl:apply-templates/>
        </d:caution>
    </xsl:template>

    <xsl:template match="danger">
        <d:warning>
            <xsl:apply-templates/>
        </d:warning>
    </xsl:template>


    <xsl:template match="error">
        <d:para>
            <d:errortext>
                <xsl:apply-templates/>
            </d:errortext>
        </d:para>
    </xsl:template>

    <xsl:template match="hint">
        <d:tip>
            <xsl:apply-templates/>
        </d:tip>
    </xsl:template>

    <xsl:template match="important">
        <d:important>
            <xsl:apply-templates/>
        </d:important>
    </xsl:template>

    <xsl:template match="tip">
        <d:tip>
            <xsl:apply-templates/>
        </d:tip>
    </xsl:template>

    <xsl:template match="note">
        <d:note>
            <xsl:apply-templates/>
        </d:note>
    </xsl:template>

    <xsl:template match="warning">
        <d:warning>
            <xsl:apply-templates/>
        </d:warning>
    </xsl:template>

    <xsl:template match="admonition[@classes]" priority="2">
        <d:note role="{@classes}">
            <xsl:apply-templates/>
        </d:note>
    </xsl:template>

    <xsl:template match="admonition[@classes]/title">
        <d:title>
            <xsl:apply-templates/>
        </d:title>
    </xsl:template> 

</xsl:stylesheet>
