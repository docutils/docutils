<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1"
>
    <!-- $Id: docutils_to_fo.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->

    <xsl:param name="docbook-type">article</xsl:param>
    <xsl:param name="image-width">5in</xsl:param>
    <xsl:param name="strict"></xsl:param>
    <xsl:param name="link-type">xref</xsl:param> <!--whether to use link or
    xref for links-->
    <xsl:param name="Appendix.title">Appendix</xsl:param>
    <xsl:param name="glossary-break-before">1</xsl:param> <!--break before-->
    <xsl:param name="glossary-break-after">1</xsl:param> <!--break before-->
    <xsl:param name="docinfo-by-author"/> <!--group other info with author or authors-->
    <xsl:param name="draft_image"/> <!--suffix for draft images-->
    <xsl:param name="address-format"/> <!--in draft mode-->

</xsl:stylesheet>

