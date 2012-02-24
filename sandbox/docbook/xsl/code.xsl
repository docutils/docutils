<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template match="doctest_block">
        <d:screen xml:space="preserve">
            <xsl:apply-templates/>
        </d:screen>
    </xsl:template>
    
    <xsl:template match="literal_block">
        <d:programlisting>
            <xsl:apply-templates/>
        </d:programlisting>
    </xsl:template>
</xsl:stylesheet>
