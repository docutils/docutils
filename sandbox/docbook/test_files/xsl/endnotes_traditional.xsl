<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!--Note the order of importing matters here. Make sure you import footnotes_traditional.xsl after 
    you import endnotes.xsl-->

    <xsl:import href = "../../xsl_fo/docutils_to_fo.xsl"/>
    <xsl:import href = "../../xsl_fo/custom/endnotes.xsl"/>
    <xsl:import href = "../../xsl_fo/custom/footnotes_traditional.xsl"/>

    
</xsl:stylesheet> 
