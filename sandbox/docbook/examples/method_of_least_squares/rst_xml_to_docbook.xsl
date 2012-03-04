<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    xmlns:ml="http://www.w3.org/1998/Math/MathML"
    version="1.1">

    <xsl:import href="http://docutils.svn.sourceforge.net/viewvc/docutils/trunk/sandbox/docbook/xsl/docutils_to_docbook.xsl"/>

    <!--The three elements for each block of the address will be other, other, and other-->
    <xsl:param name="address-format">other, other, other</xsl:param>
    <xsl:param name="draft_image">_draft</xsl:param>

   <!--rewrite math_block to not number first equation in a series-->
    <xsl:template name="inside-math-block">
        <xsl:variable name="following-sibling">
            <xsl:for-each select="following-sibling::*[1]">
                <xsl:value-of select="name()"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$following-sibling = 'math_block'">
                <d:informalequation>
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
                </d:informalequation>
            </xsl:when>
            <xsl:otherwise>
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
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
   


</xsl:stylesheet>
