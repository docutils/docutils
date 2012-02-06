<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">


    <xsl:template match="field_list">
        <d:variablelist>
            <xsl:apply-templates/>
        </d:variablelist>
    </xsl:template>

    <xsl:template match="field">
        <d:varlistentry>
            <xsl:apply-templates/>
        </d:varlistentry>
    </xsl:template>

    <xsl:template match="field_name">
        <d:term>
            <xsl:apply-templates/>
        </d:term>
    </xsl:template>

    <xsl:template match="field_body">
        <d:listitem>
            <xsl:apply-templates/>
        </d:listitem>
    </xsl:template>

    <xsl:template match="field_body/paragraph">
        <d:para>
            <xsl:apply-templates/>
        </d:para>
    </xsl:template>


    
</xsl:stylesheet>
