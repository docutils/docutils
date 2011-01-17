<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id:$ -->

    <xsl:attribute-set name="literal_block">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
        <xsl:attribute name="font-size">8</xsl:attribute>
        <xsl:attribute name="white-space">pre</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="transition">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>



    <!--default paragraphs-->
    <xsl:template match="section/paragraph|document/paragraph">
        <fo:block role="paragraph">
            <xsl:apply-templates/>
        </fo:block>
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
