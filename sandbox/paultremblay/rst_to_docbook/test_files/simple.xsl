<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <!--

    This template is a simple test


    -->




    <xsl:template match = "element1">
        <element1-changed>
            <xsl:apply-templates/>
        </element1-changed>
    </xsl:template>

</xsl:stylesheet>
