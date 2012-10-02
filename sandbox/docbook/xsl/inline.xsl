<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
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
        <xsl:choose >
            <xsl:when test="$link-type = 'xref'">
                <d:xref linkend="{@refid}"/>
            </xsl:when>
            <xsl:otherwise >
                <d:link linkend="{@refid}">
                    <xsl:apply-templates/>
                </d:link>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <xsl:template match="target">
        <xsl:if test="parent::paragraph">
            <d:anchor>
                <xsl:attribute name="xml:id">
                    <xsl:choose >
                        <xsl:when test="@refid">
                            <xsl:value-of select="@refid"/>
                        </xsl:when>
                        <xsl:otherwise >
                            <xsl:value-of select="@ids"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:attribute>
            </d:anchor>
        </xsl:if>
    </xsl:template>

    <xsl:template match="strong">
        <d:emphasis role="strong">
            <xsl:apply-templates/>
        </d:emphasis>
    </xsl:template>

    <xsl:template match="title_reference">
        <d:code>
            <xsl:apply-templates/>
        </d:code>
    </xsl:template>

    <xsl:template match="subscript">
        <d:subscript>
            <xsl:apply-templates/>
        </d:subscript>
    </xsl:template>

    <xsl:template match="superscript">
        <d:superscript>
            <xsl:apply-templates/>
        </d:superscript>
    </xsl:template>

    <xsl:template match="inline[@classes='page-break']">
        <xsl:processing-instruction name="hard-pagebreak"/>
    </xsl:template>

    <xsl:template match="inline[@classes = 'title']" priority="2"/>
    
</xsl:stylesheet>
