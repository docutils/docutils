<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template
        match="document/paragraph|section/paragraph|block_quote/paragraph|
        attention/paragraph|caution/paragraph|admonition/paragraph|
        danger/paragraph|hint/paragraph|important/paragraph|
        note/paragraph|tip/paragraph|warning/paragraph ">
        <d:para>
            <xsl:apply-templates/>
        </d:para>
    </xsl:template>

    <xsl:template match="section/block_quote|block_quote">
        <d:blockquote>
            <xsl:apply-templates select="attribution" mode="with-block-quote"/>
            <xsl:apply-templates/>
        </d:blockquote>
    </xsl:template>

    <xsl:template match="attribution" mode="with-block-quote">
        <d:attribution >
            <xsl:apply-templates/>
        </d:attribution>
    </xsl:template>

    <xsl:template match="error/paragraph">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="block_quote/attribution"/>
    
</xsl:stylesheet>
