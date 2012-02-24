<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <!--
    <xsl:import href="/Users/cejohnsonlouisville/Documents/docbook-xsl-ns-1.76.1/fo/docbook.xsl"/>
    -->
    <xsl:import href="http://50.56.245.89/xsl-ns/fo/docbook.xsl"/>
    <xsl:import href="methods_title_page.xsl"/>
    <xsl:param name="generate.toc">
appendix  toc,title
article/appendix  nop
article   toc,title,figure,table
book      toc,title,figure,table,example,equation
chapter   toc,title
part      toc,title
preface   toc,title
qandadiv  toc
qandaset  toc
reference toc,title
sect1     toc
sect2     toc
sect3     toc
sect4     toc
sect5     toc
section   toc
set       toc,title
</xsl:param>

    <xsl:param name="section.autolabel" select="1"/>
    <xsl:param name="fop1.extensions" select="1"/>
    <xsl:param name="inline-orgname"></xsl:param>
    <xsl:param name="header.rule">0</xsl:param>
    <xsl:param name="footer.rule">0</xsl:param>



    <xsl:attribute-set name="article.appendix.title.properties">
      <xsl:attribute name="margin-{$direction.align.start}">
          <xsl:text >inherit</xsl:text>
      </xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="abstract.title.properties" >
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
    </xsl:attribute-set>
    <xsl:attribute-set name="formal.object.properties">
       <xsl:attribute name="keep-together.within-column">always</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="address">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="font-size">12pt</xsl:attribute>
    </xsl:attribute-set>


    <xsl:template match="d:street">
        <fo:block  xsl:use-attribute-sets="address">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="d:city" >
        <fo:block  xsl:use-attribute-sets="address">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>



   <xsl:template match="d:affiliation/d:orgname" mode="titlepage.mode">
       <fo:block>
           <xsl:apply-templates/>
       </fo:block>
   </xsl:template>

    <xsl:template match="d:author" mode="titlepage.mode">
      <fo:block>
        <xsl:call-template name="anchor"/>
        <xsl:choose>
          <xsl:when test="d:orgname">
            <xsl:apply-templates/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="person.name"/>
            <xsl:if test="d:affiliation/d:orgname">
                <xsl:if test="$inline-orgname != ''">
                    <xsl:text>, </xsl:text>
                </xsl:if>
              <xsl:apply-templates select="d:affiliation/d:orgname" mode="titlepage.mode"/>
            </xsl:if>
            <xsl:if test="d:email|d:affiliation/d:address/d:email">
              <xsl:text> </xsl:text>
              <xsl:apply-templates select="(d:email|d:affiliation/d:address/d:email)[1]"/>
            </xsl:if>
          </xsl:otherwise>
        </xsl:choose>
      </fo:block>
    </xsl:template>

    

   
   <xsl:attribute-set name="toc.line.properties">
    <xsl:attribute name="text-align-last">justify</xsl:attribute>
    <xsl:attribute name="text-align">inherit</xsl:attribute>
   </xsl:attribute-set>

   <xsl:template name="header.content_">
      <xsl:param name="pageclass" select="''"/>
      <xsl:param name="sequence" select="''"/>
      <xsl:param name="position" select="''"/>
      <xsl:param name="gentext-key" select="''"/>
      <xsl:choose>
          <xsl:when test="$pageclass = 'body'  and $position = 'left'">
              <fo:block text-align="center">
                    <xsl:apply-templates select="." mode="title.markup"/>
              </fo:block>
          </xsl:when>
          <xsl:when test="$pageclass = 'long.table' and $position = 'left'">
              <fo:block text-align="center">
                  <xsl:text>Executive Report</xsl:text>
              </fo:block>
          </xsl:when>
          <xsl:when test="$pageclass = 'long.table' and $position = 'after-rule' and ($sequence = 'even' or $sequence = 'odd') ">
              <fo:block space-before="10pt">
                <xsl:apply-templates select="self::d:table|descendant::d:table[1]" mode="object.title.markup"/>
                <xsl:text> (cont.)</xsl:text>
              </fo:block>
          </xsl:when>
      </xsl:choose>
   </xsl:template>  


   
   <!--STANDARD STUFF-->

    <xsl:template match="processing-instruction('hard-pagebreak')">
        <fo:block break-after='page'/>
    </xsl:template>
   
   



</xsl:stylesheet>
