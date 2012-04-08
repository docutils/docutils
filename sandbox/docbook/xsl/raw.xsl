<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1"
    >
    <xsl:template match="raw[@format='xml']">
        <xsl:copy-of select="child::*"/>
    </xsl:template>
    
</xsl:stylesheet> 
    

