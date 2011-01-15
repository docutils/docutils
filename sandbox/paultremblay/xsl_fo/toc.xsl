<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <xsl:attribute-set name="toc-level1">
        <xsl:attribute name="text-align-last">justify</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-level2">
        <xsl:attribute name="text-align-last">justify</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-level3">
        <xsl:attribute name="text-align-last">justify</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-level4">
        <xsl:attribute name="text-align-last">justify</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-level5">
        <xsl:attribute name="text-align-last">justify</xsl:attribute>
    </xsl:attribute-set>

    <!--TOC title-->
    <xsl:template match="topic[@classes='contents']/title">
        <fo:block>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>



    <xsl:template match="topic[@classes = 'contents']/bullet_list">
        <xsl:apply-templates mode="toc"/>
    </xsl:template>

    <xsl:template match="topic[@classes='contents']/bullet_list/list_item"/>

    <xsl:template match="reference" mode="toc">
        <xsl:apply-templates/> 
    </xsl:template>



    <xsl:template match="list_item" mode="toc">
        <xsl:apply-templates mode="toc"/>
    </xsl:template>

    <xsl:template match="paragraph" mode="toc">
        <xsl:variable name="level">
        <xsl:value-of select="count(ancestor::bullet_list)"/>
        </xsl:variable> 
        <xsl:choose>
            <xsl:when test="$level = 1">
                <xsl:call-template name="toc-paragraph-level1"/>
            </xsl:when>
            <xsl:when test="$level = 2">
                <xsl:call-template name="toc-paragraph-level2"/>
            </xsl:when>
            <xsl:when test="$level = 3">
                <xsl:call-template name="toc-paragraph-level3"/>
            </xsl:when>
            <xsl:when test="$level = 4">
                <xsl:call-template name="toc-paragraph-level4"/>
            </xsl:when>
            <xsl:when test="$level = 5">
                <xsl:call-template name="toc-paragraph-level5"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message>
                    <xsl:text>Can't format paragraphs in TOC deeper than 5</xsl:text>
                </xsl:message>
                <xsl:if test="$strict='True'">
                    <xsl:message terminate = "yes">
                        <xsl:text>Terminating</xsl:text>
                    </xsl:message>
                </xsl:if>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="toc-paragraph-level1">
        <fo:block xsl:use-attribute-sets = "toc-level1">
            <fo:inline>
                <xsl:apply-templates mode="toc"/>
                <fo:leader leader-pattern="dots"/>
                <xsl:element name="fo:page-number-citation">
                    <xsl:attribute name="ref-id">
                        <xsl:value-of select="reference/@ids"/>
                    </xsl:attribute>
                </xsl:element>
            </fo:inline>
        </fo:block>
    </xsl:template>

    <xsl:template name="toc-paragraph-level2">
        <fo:block xsl:use-attribute-sets = "toc-level2">
            <fo:inline>
                <xsl:apply-templates mode="toc"/>
                <fo:leader leader-pattern="dots"/>
                <xsl:element name="fo:page-number-citation">
                    <xsl:attribute name="ref-id">
                        <xsl:value-of select="reference/@ids"/>
                    </xsl:attribute>
                </xsl:element>
            </fo:inline>
        </fo:block>
    </xsl:template>

    <xsl:template name="toc-paragraph-level3">
        <fo:block xsl:use-attribute-sets = "toc-level3">
            <fo:inline>
                <xsl:apply-templates mode="toc"/>
                <fo:leader leader-pattern="dots"/>
                <xsl:element name="fo:page-number-citation">
                    <xsl:attribute name="ref-id">
                        <xsl:value-of select="reference/@ids"/>
                    </xsl:attribute>
                </xsl:element>
            </fo:inline>
        </fo:block>
    </xsl:template>

    <xsl:template name="toc-paragraph-level4">
        <fo:block xsl:use-attribute-sets = "toc-level4">
            <fo:inline>
                <xsl:apply-templates mode="toc"/>
                <fo:leader leader-pattern="dots"/>
                <xsl:element name="fo:page-number-citation">
                    <xsl:attribute name="ref-id">
                        <xsl:value-of select="reference/@ids"/>
                    </xsl:attribute>
                </xsl:element>
            </fo:inline>
        </fo:block>
    </xsl:template>

    <xsl:template name="toc-paragraph-level5">
        <fo:block xsl:use-attribute-sets = "toc-level5">
            <fo:inline>
                <xsl:apply-templates mode="toc"/>
                <fo:leader leader-pattern="dots"/>
                <xsl:element name="fo:page-number-citation">
                    <xsl:attribute name="ref-id">
                        <xsl:value-of select="reference/@ids"/>
                    </xsl:attribute>
                </xsl:element>
            </fo:inline>
        </fo:block>
    </xsl:template>

         <xsl:template match="generated[@classes='sectnum'][ancestor::topic[@classes='contents']]">
            <xsl:variable name="num" select="concat(substring-before(., '&#x00a0;'), '.')"/>
                <xsl:call-template name="format-number">
                    <xsl:with-param name="string" select="$num"/>
                </xsl:call-template>
                <!--this is a hack here-->
                <xsl:text>&#x00a0;</xsl:text>
                <xsl:value-of select="substring-after(., '&#x00a0;')"/>
         </xsl:template>


</xsl:stylesheet> 
