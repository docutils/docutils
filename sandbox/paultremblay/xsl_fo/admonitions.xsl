<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">
    
    <!-- $Id: admonitions.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->

    <xsl:attribute-set name="default-admonition-outer-block">
        <xsl:attribute name="border-style">solid</xsl:attribute>
        <xsl:attribute name="border-width">1px</xsl:attribute>
        <xsl:attribute name="padding">6pt</xsl:attribute>
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
        <!--
        <xsl:attribute name="background-color">blue</xsl:attribute>
        -->
    </xsl:attribute-set>

    <xsl:attribute-set name="default-admonition-title-block">
        <xsl:attribute name="space-after">10pt</xsl:attribute>
        <xsl:attribute name="font-size">larger</xsl:attribute>
        <xsl:attribute name="color">red</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="attention-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="caution-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>

    
    <xsl:attribute-set name="danger-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="error-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="hint-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="important-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="note-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="tip-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="admonition-custom-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>


    <xsl:attribute-set name="warning-block" use-attribute-sets="default-admonition-outer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="attention-title-block" use-attribute-sets="default-admonition-title-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="caution-title-block" use-attribute-sets="default-admonition-title-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="danger-title-block" use-attribute-sets="default-admonition-title-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="error-title-block" use-attribute-sets="default-admonition-title-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="hint-title-block" use-attribute-sets="default-admonition-title-block">
        <xsl:attribute name="color">black</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="important-title-block" use-attribute-sets="default-admonition-title-block">
        <xsl:attribute name="color">black</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="note-title-block" use-attribute-sets="default-admonition-title-block">
        <xsl:attribute name="color">black</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="tip-title-block" use-attribute-sets="default-admonition-title-block">
        <xsl:attribute name="color">black</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="warning-title-block" use-attribute-sets="default-admonition-title-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="admonition-custorm-title-block" use-attribute-sets="default-admonition-title-block">
        <xsl:attribute name="color">black</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="admonition-paragraph-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="admonition-first-paragraph-block" use-attribute-sets="admonition-paragraph-block">
    </xsl:attribute-set>



    <xsl:template match="attention">
        <fo:block role="attention" xsl:use-attribute-sets="attention-block">
            <xsl:if test="$attention-title != ''">
                <fo:block role="attention-title" xsl:use-attribute-sets="attention-title-block">
                    <xsl:value-of select="$attention-title"/>
                </fo:block>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="caution">
        <fo:block role="caution" xsl:use-attribute-sets="caution-block">
            <xsl:if test="$caution-title != ''">
                <fo:block role="caution-title" xsl:use-attribute-sets="caution-title-block">
                    <xsl:value-of select="$caution-title"/>
                </fo:block>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="danger">
        <fo:block role="danger" xsl:use-attribute-sets="danger-block">
            <xsl:if test="$danger-title != ''">
                <fo:block role="danger-title" xsl:use-attribute-sets="danger-title-block">
                    <xsl:value-of select="$danger-title"/>
                </fo:block>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="error">
        <fo:block role="error" xsl:use-attribute-sets="error-block">
            <xsl:if test="$error-title != ''">
                <fo:block role="error-title" xsl:use-attribute-sets="error-title-block">
                    <xsl:value-of select="$error-title"/>
                </fo:block>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="hint">
        <fo:block role="hint" xsl:use-attribute-sets="hint-block">
            <xsl:if test="$hint-title != ''">
                <fo:block role="hint-title" xsl:use-attribute-sets="hint-title-block">
                    <xsl:value-of select="$hint-title"/>
                </fo:block>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="important">
        <fo:block role="important" xsl:use-attribute-sets="important-block">
            <xsl:if test="$important-title != ''">
                <fo:block role="important-title" xsl:use-attribute-sets="important-title-block">
                    <xsl:value-of select="$important-title"/>
                </fo:block>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="note">
        <fo:block role="note" xsl:use-attribute-sets="note-block">
            <xsl:if test="$note-title != ''">
                <fo:block role="note-title" xsl:use-attribute-sets="note-title-block">
                    <xsl:value-of select="$note-title"/>
                </fo:block>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="tip">
        <fo:block role="tip" xsl:use-attribute-sets="tip-block">
            <xsl:if test="$tip-title != ''">
                <fo:block role="tip-title" xsl:use-attribute-sets="tip-title-block">
                    <xsl:value-of select="$tip-title"/>
                </fo:block>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="warning">
        <fo:block role="warning" xsl:use-attribute-sets="warning-block">
            <xsl:if test="$warning-title != ''">
                <fo:block role="warning-title" xsl:use-attribute-sets="warning-title-block">
                    <xsl:value-of select="$warning-title"/>
                </fo:block>
            </xsl:if>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>
    <xsl:template match="admonition[@classes = 'admonition-custom']">
        <fo:block role="admonition-custotrm" xsl:use-attribute-sets="admonition-custom-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="admonition/title">
        <fo:block role="admonition-custorm-title" xsl:use-attribute-sets="admonition-custorm-title-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>



    <xsl:template match="attention/paragraph[1]| caution/paragraph[1]|danger/paragraph[1]|error/paragraph[1]|
        hint/paragraph[1]|important/paragraph[1]|note/paragraph[1]|tip/paragraph[1]|warning/paragraph[1]|
        admonition/paragraph[1] "
        priority="2">
        <fo:block role="admonition-paragraph" xsl:use-attribute-sets="admonition-first-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


    <xsl:template match="attention/paragraph| caution/paragraph|danger/paragraph|error/paragraph|
        hint/paragraph|important/paragraph|note/paragraph|tip/paragraph|warning/paragraph|admonition/paragraph">
        <fo:block role="admonition-paragraph" xsl:use-attribute-sets="admonition-paragraph-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


</xsl:stylesheet>
