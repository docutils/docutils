<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:import href="http://docbook.sourceforge.net/release/xsl-ns/current/fo/docbook.xsl"/>
    <xsl:param name="glosslist.as.blocks">1</xsl:param>

    <xsl:param name="fop1.extensions" select="1"/><!--for FOP-->
    <xsl:param name="glossterm.width">1in</xsl:param> <!--separation between term and def-->
    <xsl:param name="glossterm.separation">1cm</xsl:param><!--vertical distance-->
    <xsl:param name="section.autolabel" select="1"/> <!--automoatic numbering-->


    <!--For glass as list, (not used)
    <xsl:attribute-set name="glossterm.list.properties">
      <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="glossentry.list.item.properties">
      <xsl:attribute name="space-before.optimum">1in</xsl:attribute>
      <xsl:attribute name="space-before.minimum">0.8em</xsl:attribute>
      <xsl:attribute name="space-before.maximum">1.2in</xsl:attribute>
    </xsl:attribute-set>
    -->
    
    <!--GLOSSARY (as blocks)-->
    <xsl:attribute-set name="glossdef.block.properties">
        <xsl:attribute name="space-after" >1in</xsl:attribute>
    </xsl:attribute-set>
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

    <!--TABLES-->

    <!--no table frame-->
    <xsl:param name="default.table.frame">none</xsl:param>
    <xsl:attribute-set name="table.caption">
        <xsl:attribute name="start-indent">inherit</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>
    <xsl:attribute-set name="table.table.properties">
        <!--will keep tables on same page; don't want because am using long tables-->
        <!--
       <xsl:attribute name="keep-together.within-column">always</xsl:attribute>
       -->
    </xsl:attribute-set>

    <!--TITLES-->

    <xsl:attribute-set name="abstract.title.properties" >
        <xsl:attribute name="font-size">18pt</xsl:attribute>
        <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
    </xsl:attribute-set>
    <xsl:attribute-set name="formal.object.properties">
       <xsl:attribute name="keep-together.within-column">always</xsl:attribute>
    </xsl:attribute-set>


    <!--HEADER-->
   <xsl:template name="header.content">
      <xsl:param name="pageclass" select="''"/>
      <xsl:param name="sequence" select="''"/>
      <xsl:param name="position" select="''"/>
      <xsl:param name="gentext-key" select="''"/>
      <xsl:choose>
          <xsl:when test="$pageclass = 'body'  and $sequence = 'first'">
              <fo:block text-align="center">
                  <xsl:text >First Header</xsl:text>
              </fo:block>
          </xsl:when>
          <xsl:when test="$pageclass = 'body'  and $position = 'left'">
              <fo:block text-align="center">
                    <xsl:apply-templates select="." mode="title.markup"/>
              </fo:block>
          </xsl:when>
      </xsl:choose>
   </xsl:template>  

   <!--STANDARD STUFF-->
    <xsl:template match="processing-instruction('hard-pagebreak')">
        <fo:block break-after='page'/>
    </xsl:template>
   
    <!--CUSTOM TOC (only used with custom page sequence)-->
   <xsl:template name="make.article.tocs">
      <xsl:variable name="lot-master-reference">
        <xsl:call-template name="select.pagemaster">
          <xsl:with-param name="pageclass" select="'lot'"/>
        </xsl:call-template>
      </xsl:variable>

      <xsl:variable name="toc.params">
        <xsl:call-template name="find.path.params">
          <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
        </xsl:call-template>
      </xsl:variable>

      <xsl:if test="contains($toc.params, 'toc')">
        <xsl:call-template name="page.sequence">
          <xsl:with-param name="master-reference"
                          select="$lot-master-reference"/>
          <xsl:with-param name="element" select="'toc'"/>
          <xsl:with-param name="gentext-key" select="'TableofContents'"/>
          <xsl:with-param name="initial-page-number" select="1"/>
          <xsl:with-param name="content">
           <xsl:call-template name="component.toc">
             <xsl:with-param name="toc.title.p"
                             select="contains($toc.params, 'title')"/>
            </xsl:call-template>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>


      <xsl:if test="contains($toc.params,'figure') and .//d:figure">
        <xsl:call-template name="page.sequence">
          <xsl:with-param name="master-reference"
                          select="$lot-master-reference"/>
          <xsl:with-param name="element" select="'toc'"/>
          <xsl:with-param name="gentext-key" select="'ListofFigures'"/>
          <xsl:with-param name="initial.page.number" select="''"/>
          <xsl:with-param name="content">
            <xsl:call-template name="list.of.titles">
              <xsl:with-param name="titles" select="'figure'"/>
              <xsl:with-param name="nodes" select=".//d:figure"/>
            </xsl:call-template>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>

      <xsl:if test="contains($toc.params,'table') and .//d:table">
        <xsl:call-template name="page.sequence">
          <xsl:with-param name="master-reference"
                          select="$lot-master-reference"/>
          <xsl:with-param name="element" select="'toc'"/>
          <xsl:with-param name="gentext-key" select="'ListofTables'"/>
          <xsl:with-param name="content">
            <xsl:call-template name="list.of.titles">
              <xsl:with-param name="titles" select="'table'"/>
              <xsl:with-param name="nodes" select=".//d:table"/>
            </xsl:call-template>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>

      <xsl:if test="contains($toc.params,'example') and .//d:example">
        <xsl:call-template name="page.sequence">
          <xsl:with-param name="master-reference"
                          select="$lot-master-reference"/>
          <xsl:with-param name="element" select="'toc'"/>
          <xsl:with-param name="gentext-key" select="'ListofExample'"/>
          <xsl:with-param name="content">
            <xsl:call-template name="list.of.titles">
              <xsl:with-param name="titles" select="'example'"/>
              <xsl:with-param name="nodes" select=".//d:example"/>
            </xsl:call-template>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>

      <xsl:if test="contains($toc.params,'equation') and 
                     .//d:equation[d:title or d:info/d:title]">
        <xsl:call-template name="page.sequence">
          <xsl:with-param name="master-reference"
                          select="$lot-master-reference"/>
          <xsl:with-param name="element" select="'toc'"/>
          <xsl:with-param name="gentext-key" select="'ListofEquations'"/>
          <xsl:with-param name="content">
            <xsl:call-template name="list.of.titles">
              <xsl:with-param name="titles" select="'equation'"/>
              <xsl:with-param name="nodes" 
                              select=".//d:equation[d:title or d:info/d:title]"/>
            </xsl:call-template>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>

      <xsl:if test="contains($toc.params,'procedure') and 
                     .//d:procedure[d:title or d:info/d:title]">
        <xsl:call-template name="page.sequence">
          <xsl:with-param name="master-reference"
                          select="$lot-master-reference"/>
          <xsl:with-param name="element" select="'toc'"/>
          <xsl:with-param name="gentext-key" select="'ListofProcedures'"/>
          <xsl:with-param name="content">
            <xsl:call-template name="list.of.titles">
              <xsl:with-param name="titles" select="'procedure'"/>
              <xsl:with-param name="nodes" 
                              select=".//d:procedure[d:title or d:info/d:title]"/>
            </xsl:call-template>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>
       
   </xsl:template>
   <!--END CUSTOM TOC-->
   
   
    <xsl:template name="calsTable">
      <xsl:variable name="keep.together">
        <xsl:call-template name="pi.dbfo_keep-together"/>
      </xsl:variable>

      <xsl:for-each select="d:tgroup">

        <fo:table xsl:use-attribute-sets="table.table.properties">
          <xsl:if test="$keep.together != ''">
            <xsl:attribute name="keep-together.within-column">
              <xsl:value-of select="$keep.together"/>
            </xsl:attribute>
          </xsl:if>
          <xsl:call-template name="table.frame"/>
          <xsl:if test="following-sibling::d:tgroup">
            <xsl:attribute name="border-bottom-width">0pt</xsl:attribute>
            <xsl:attribute name="border-bottom-style">none</xsl:attribute>
            <xsl:attribute name="padding-bottom">0pt</xsl:attribute>
            <xsl:attribute name="margin-bottom">0pt</xsl:attribute>
            <xsl:attribute name="space-after">0pt</xsl:attribute>
            <xsl:attribute name="space-after.minimum">0pt</xsl:attribute>
            <xsl:attribute name="space-after.optimum">0pt</xsl:attribute>
            <xsl:attribute name="space-after.maximum">0pt</xsl:attribute>
          </xsl:if>
          <xsl:if test="preceding-sibling::d:tgroup">
            <xsl:attribute name="border-top-width">0pt</xsl:attribute>
            <xsl:attribute name="border-top-style">none</xsl:attribute>
            <xsl:attribute name="padding-top">0pt</xsl:attribute>
            <xsl:attribute name="margin-top">0pt</xsl:attribute>
            <xsl:attribute name="space-before">0pt</xsl:attribute>
            <xsl:attribute name="space-before.minimum">0pt</xsl:attribute>
            <xsl:attribute name="space-before.optimum">0pt</xsl:attribute>
            <xsl:attribute name="space-before.maximum">0pt</xsl:attribute>
          </xsl:if>

          <xsl:apply-templates select="."/>
        </fo:table>

        <xsl:for-each select="d:mediaobject|d:graphic">
          <xsl:apply-templates select="."/>
        </xsl:for-each>

      </xsl:for-each>
      <xsl:apply-templates select="d:caption"/>
    </xsl:template>

   <xsl:template match="d:caption">
       <fo:block xsl:use-attribute-sets="table.caption">
            <xsl:attribute name="keep-together.within-column">always</xsl:attribute>
            <xsl:apply-templates/>
       </fo:block>
   </xsl:template>

   <xsl:template
       match="d:para[../../@role='continuation-label']" priority="2">
       <xsl:for-each select="preceding::d:table[1]">
           <fo:block>
               <xsl:apply-templates select="self::d:table" mode="object.title.markup"/>
               <xsl:text > (cont.)</xsl:text>
           </fo:block>
       </xsl:for-each>
       
   </xsl:template>

</xsl:stylesheet>
