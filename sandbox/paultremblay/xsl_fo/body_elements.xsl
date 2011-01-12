<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id:$ -->
    <xsl:attribute-set name="literal_block">
        <xsl:attribute name="font-family">Courier</xsl:attribute>
        <xsl:attribute name="font-size">8</xsl:attribute>
        <xsl:attribute name="white-space">pre</xsl:attribute>
    </xsl:attribute-set>

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

    <xsl:attribute-set name="transition">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>



    <!--default paragraphs-->
    <xsl:template match="section/paragraph|document/paragraph">
        <xsl:element name="fo:block">
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <!--TOC title-->
    <xsl:template match="topic[@classes='contents']/title">
        <fo:block>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


    <xsl:template match="section/title">
        <xsl:variable name="level">
            <xsl:value-of select="count(ancestor::section)"/>
        </xsl:variable>
        <fo:block id = "{@refid}">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="bullet_list">
        <xsl:choose>
            <xsl:when test="ancestor::topic[@classes='contents']">
                <xsl:apply-templates mode="toc"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates/>
            </xsl:otherwise>
        </xsl:choose>
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

    <xsl:template match="reference" mode="toc">
        <xsl:apply-templates/> 
    </xsl:template>

    <xsl:template match="literal_block">
        <fo:block xsl:use-attribute-sets="literal_block"><xsl:apply-templates/></fo:block>
    </xsl:template>

    <xsl:template match="transition">
        <fo:block xsl:use-attribute-sets = "transition" text-align="center">
            <!--
            <fo:inline><fo:leader leader-pattern="rule" leader-length="3in"/></fo:inline>
            -->
            <xsl:value-of select="$transition-text"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="comment|decoration/header|decoration/footer"/>
    
</xsl:stylesheet>
