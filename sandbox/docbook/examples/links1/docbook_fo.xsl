<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:import href="http://docbook.sourceforge.net/release/xsl-ns/current/fo/docbook.xsl"/>

    <xsl:param name="local.l10n.xml" select="document('')"/>  
    <l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0"> 
      <l:l10n language="en"> 
        <l:context name="xref-number">
           <l:template name="equation" text="%n"/>
        </l:context>
        <!--figure has title, unlike your informal equations-->
        <l:context name="xref-number-and-title">
           <l:template name="figure" text="%n: %t"/>
           <l:template name="table" text="%n: %t"/>
        </l:context>
      </l:l10n>
    </l:i18n>


</xsl:stylesheet>
