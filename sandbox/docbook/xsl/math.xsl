<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    xmlns:ml="http://www.w3.org/1998/Math/MathML"
    version="1.1">

    <xsl:template match="math">
        <d:inlineequation>
            <xsl:choose >
                <xsl:when test="ml:math">
                    <xsl:apply-templates/>
                </xsl:when>
                <xsl:otherwise >
                    <d:mathphrase>
                        <xsl:apply-templates/>
                    </d:mathphrase>
                </xsl:otherwise>
            </xsl:choose>
        </d:inlineequation>
    </xsl:template>

    <xsl:template match="math_block">
        <xsl:choose>
            <xsl:when test="ancestor::paragraph or ancestor::compound">
                <xsl:call-template name="inside-math-block"/>
            </xsl:when>
            <xsl:otherwise>
                <d:para>
                    <xsl:call-template name="inside-math-block"/>
                </d:para>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="inside-math-block">
        <d:equation>
            <xsl:call-template name="make-id"/>
            <xsl:choose >
                <xsl:when test="ml:math">
                    <xsl:apply-templates/>
                </xsl:when>
                <xsl:otherwise >
                    <d:mathphrase>
                        <xsl:apply-templates/>
                    </d:mathphrase>
                </xsl:otherwise>
            </xsl:choose>
        </d:equation>
    </xsl:template>

    <xsl:template match="ml:math">
        <xsl:copy-of select="."/>
    </xsl:template>
    
</xsl:stylesheet>
