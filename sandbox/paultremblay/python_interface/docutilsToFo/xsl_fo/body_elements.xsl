<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <xsl:attribute-set name="paragraph-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="first-paragraph-block" use-attribute-sets="paragraph-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="literal-block">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
        <xsl:attribute name="font-size">8</xsl:attribute>
        <xsl:attribute name="white-space">pre</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="transition-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="document-title-block">
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="font-size">24pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="document-title-page-block">
        <!--
        <xsl:attribute name="break-after">page</xsl:attribute>
        -->
    </xsl:attribute-set>

    <xsl:attribute-set name="document-subtitle-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="font-size">18pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="block-quote-outer-block">
        <xsl:attribute name="start-indent">20mm</xsl:attribute>
        <xsl:attribute name="end-indent">20mm</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="block-quote-paragraph-block">
        <!--
        <xsl:attribute name="start-indent">inherit</xsl:attribute>
        <xsl:attribute name="end-indent">inherit</xsl:attribute>
        -->
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="block-quote-first-paragraph-block" use-attribute-sets="block-quote-paragraph-block">
        <xsl:attribute name="space-before">0pt</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="block-quote-attribution-block">
        <xsl:attribute name="text-align">right</xsl:attribute>
    </xsl:attribute-set>

    <xsl:template name="get-preceding-sib">
        <xsl:choose>
            <xsl:when test="self::comment">
                <xsl:for-each select="preceding-sibling::*[1]">
                    <xsl:call-template name="get-preceding-sib"/>
                </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="name(.)"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!--default paragraphs-->
    <xsl:template match="section/paragraph|document/paragraph|compound/paragraph">
        <xsl:variable name="prev-sib">
            <xsl:for-each select="preceding-sibling::*[1]">
                <xsl:call-template name="get-preceding-sib"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="not(preceding-sibling::*)">
                <xsl:call-template name="default-first-paragraph"/>
            </xsl:when>
            <xsl:when test="$prev-sib != 'paragraph'">
                <xsl:call-template name="default-first-paragraph"/>
            </xsl:when>
            <!--
            <xsl:when test="$prev-sib = 'topic' or $prev-sib = 'table'  
                or $prev-sib = 'attention' or $prev-sib = 'caution' or $prev-sib = 'danger'
                or $prev-sib = 'danger' or $prev-sib = 'error' or $prev-sib = 'hint'
                or $prev-sib = 'important' or $prev-sib = 'note' or $prev-sib = 'tip' or
                $prev-sib = 'warning' or $prev-sib = 'admonition' or $prev-sib = 'bullet_list'
                or $prev-sib = 'enumerated_list' or $prev-sib = 'definition_list' or
                $prev-sib = 'field_list' or $prev-sib = 'option_list' or $prev-sib = 'line_block'
                or $prev-sib = 'literal_block' or $prev-sib = 'doctest_block' or 
                $prev-sib = 'transition' or $prev-sib = 'title' or $prev-sib = 'subtitle'
                or $prev-sib = 'block_quote' or $prev-sib = 'sidebar' or $prev-sib = 'rubric'
                or $prev-sib = 'container' or $prev-sib = 'compound' or $prev-sib = 'decoration' 
                ">
                <xsl:call-template name="default-first-paragraph"/>
            </xsl:when>
            -->
            <xsl:otherwise>
                <xsl:call-template name="default-paragraph"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="default-first-paragraph">
        <xsl:choose>
            <xsl:when test="@ids">
                <fo:block role="first-paragraph" xsl:use-attribute-sets="first-paragraph-block" id="{@ids}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:otherwise>
                <fo:block role="first-paragraph" xsl:use-attribute-sets="first-paragraph-block">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="default-paragraph">
        <xsl:choose>
            <xsl:when test="@ids">
                <fo:block role="paragraph" xsl:use-attribute-sets="paragraph-block" id="{@ids}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:otherwise>
                <fo:block role="paragraph" xsl:use-attribute-sets="paragraph-block">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="literal_block">
        <fo:block xsl:use-attribute-sets="literal-block" role="literal-block"><xsl:apply-templates/></fo:block>
    </xsl:template>


    <xsl:template match="doctest_block">
        <fo:block xsl:use-attribute-sets="literal-block" role="doctest-block"><xsl:apply-templates/></fo:block>
    </xsl:template>

    <xsl:template match="transition">
        <fo:block xsl:use-attribute-sets = "transition-block" role="transition">
            <!--
            <fo:inline><fo:leader leader-pattern="rule" leader-length="3in"/></fo:inline>
            -->
            <xsl:value-of select="$transition-text"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="document/subtitle">
        <xsl:if test="$title-pagination='with-body'">
            <fo:block xsl:use-attribute-sets="document-subtitle-block" role="subtitle">
                <xsl:apply-templates/>
            </fo:block>
        </xsl:if>
    </xsl:template>

    <xsl:template match="document/title">
        <xsl:if test="$title-pagination='with-body'">
            <fo:block xsl:use-attribute-sets="document-title-block" role="title">
                <xsl:apply-templates/>
            </fo:block>
        </xsl:if> 
    </xsl:template>

    <xsl:template match="document/title" mode="front">
        <fo:block xsl:use-attribute-sets="document-title-page-block" role="title-page">
            <fo:block xsl:use-attribute-sets="document-title-block" role="title">
                <xsl:apply-templates/>
            </fo:block>
            <xsl:apply-templates select="/document/subtitle" mode="front"/>
        </fo:block>
    </xsl:template>

    <xsl:template match="document/subtitle" mode="front">
        <fo:block xsl:use-attribute-sets="document-subtitle-block" role="subtitle">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[not(@classes)]">
        <fo:block role="block-quote" xsl:use-attribute-sets = "block-quote-outer-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[not(@classes)]/paragraph">
        <fo:block role="block-quote" xsl:use-attribute-sets = "block-quote-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[not(@classes)]/paragraph[1]" priority="2">
        <fo:block role="block-quote" xsl:use-attribute-sets = "block-quote-first-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="block_quote[not(@classes)]/attribution">
        <fo:block role="block-quote" xsl:use-attribute-sets = "block-quote-attribution-block">
            <xsl:value-of select="$text-before-block-quote-attribution"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="comment"/>
    
</xsl:stylesheet>
