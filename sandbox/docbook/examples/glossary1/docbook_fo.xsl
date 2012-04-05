<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:import href="http://docbook.sourceforge.net/release/xsl-ns/current/fo/docbook.xsl"/>
    <xsl:param name="glosslist.as.blocks">1</xsl:param>

    <xsl:param name="glossterm.width">1in</xsl:param> <!--separation between term and def-->
    <xsl:param name="glossterm.separation">1cm</xsl:param><!--vertical distance-->


    <xsl:attribute-set name="glossterm.list.properties">
      <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="glossentry.list.item.properties">
      <xsl:attribute name="space-before.optimum">1in</xsl:attribute>
      <xsl:attribute name="space-before.minimum">0.8em</xsl:attribute>
      <xsl:attribute name="space-before.maximum">1.2in</xsl:attribute>
    </xsl:attribute-set>
    
    <!--for blocks-->
    <xsl:attribute-set name="glossdef.block.properties">
        <xsl:attribute name="space-after" >1in</xsl:attribute>
    </xsl:attribute-set>
    
    
    <!--for blocks-->
    <xsl:attribute-set name="glossterm.block.properties">
      <xsl:attribute name="font-weight">bold</xsl:attribute>
      <xsl:attribute name="space-after">5mm</xsl:attribute>
    </xsl:attribute-set>


    <xsl:param name="local.l10n.xml" select="document('')"/>  
    <l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0"> 
      <l:l10n language="en"> 
          <l:gentext key="glossary" text="Glossary Title"/>
      </l:l10n>
    </l:i18n>


</xsl:stylesheet>
