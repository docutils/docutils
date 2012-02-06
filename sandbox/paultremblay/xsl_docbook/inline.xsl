<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    xmlns:xlink = "http://www.w3.org/1999/xlink"
    version="1.1">

    <xsl:template match="emphasis">
        <d:emphasis>
            <xsl:apply-templates/>
        </d:emphasis>
    </xsl:template>

    <xsl:template match="literal">
        <d:literal>
            <xsl:apply-templates/>
        </d:literal>
    </xsl:template>

    <xsl:template match="inline[@classes='page-num']"/>

    <xsl:template match="reference[@refid]">
        <d:link linkend="{@refid}">
            <xsl:apply-templates/>
        </d:link>
    </xsl:template>

    <xsl:template match= "reference[@refuri]">
        <d:link xlink:href="{@refuri}">
            <xsl:apply-templates/>
        </d:link>
    </xsl:template> 

    <xsl:template match="target">
        <xsl:if test="parent::paragraph">
            <d:anchor>
                <xsl:attribute name="xml:id">
                    <xsl:value-of select="@refid"/>
                </xsl:attribute>
            </d:anchor>
        </xsl:if>
    </xsl:template>
    
</xsl:stylesheet>
