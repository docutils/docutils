<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    xmlns:xlink = "http://www.w3.org/1999/xlink"
    version="1.1">

    <xsl:template match= "reference[@refuri]">
        <d:link xlink:href="{@refuri}">
            <xsl:apply-templates/>
        </d:link>
    </xsl:template> 
    
</xsl:stylesheet>
