<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">


    <xsl:template match="section[@classes='glossary']" priority="2">
        <xsl:if test="$glossary-break-before != 0">
                <xsl:processing-instruction name="hard-pagebreak"/>
        </xsl:if>
        <d:glossary>
            <xsl:apply-templates/>
        </d:glossary>
        <xsl:if test="$glossary-break-after != 0">
            <xsl:processing-instruction name="hard-pagebreak"/>
        </xsl:if>
    </xsl:template>

    <xsl:template match="section[@classes='glossary']/field_list" priority="2">
        <d:glosslist>
            <xsl:apply-templates mode="glossary"/>
        </d:glosslist>
    </xsl:template>

    <xsl:template match="field" mode="glossary">
        <d:glossentry>
            <xsl:apply-templates mode="glossary"/>
        </d:glossentry>
    </xsl:template>

    <xsl:template match="field_name" mode="glossary">
        <d:glossterm xsl:use-attribute-sets="glossterm">
            <xsl:apply-templates mode="glossary"/>
        </d:glossterm>
    </xsl:template>

    <xsl:template match="field_body" mode="glossary">
        <d:glossdef>
            <xsl:apply-templates/>
        </d:glossdef>
    </xsl:template>


    <xsl:template match="section[@classes='glossary']/title" priority="2"/>

    
</xsl:stylesheet>
