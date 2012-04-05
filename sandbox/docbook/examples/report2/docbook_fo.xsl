<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:import href="http://docbook.sourceforge.net/release/xsl-ns/current/fo/docbook.xsl"/>
    <xsl:import href="title_page.xsl"/>
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
<xsl:param name="default.table.frame">none</xsl:param>

    <xsl:param name="section.autolabel" select="1"/>
    <xsl:param name="fop1.extensions" select="1"/>


    <xsl:param name="debug"></xsl:param>
    <xsl:param name="long.table"></xsl:param>

    <xsl:attribute-set name="article.appendix.title.properties">
      <xsl:attribute name="margin-{$direction.align.start}">
          <xsl:text >inherit</xsl:text>
      </xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="table.caption">
        <xsl:attribute name="start-indent">inherit</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="abstract.title.properties" >
        <xsl:attribute name="font-size">18pt</xsl:attribute>
        <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
    </xsl:attribute-set>
    <xsl:attribute-set name="formal.object.properties">
       <xsl:attribute name="keep-together.within-column">always</xsl:attribute>
    </xsl:attribute-set>
    <xsl:attribute-set name="table.table.properties">
       <xsl:attribute name="keep-together.within-column">always</xsl:attribute>
    </xsl:attribute-set>


   <xsl:template match="d:article">
     <xsl:variable name="id">
       <xsl:call-template name="object.id"/>
     </xsl:variable>
   
     <xsl:call-template name="page.sequence">
       <xsl:with-param name="master-reference">titlepage</xsl:with-param>
       <xsl:with-param name="content">
         <fo:block id="{$id}"
                   xsl:use-attribute-sets="component.titlepage.properties">
           <xsl:call-template name="article.titlepage"/>
         </fo:block>
         <fo:block break-after="page"/>
       </xsl:with-param>
     </xsl:call-template>

     <xsl:call-template name="make.article.tocs"/>

   
     <xsl:call-template name="page.sequence">
       <xsl:with-param name="master-reference">body</xsl:with-param>
       <xsl:with-param name="content">
         <xsl:apply-templates
             select="*[not(self::bibliography)]|processing-instruction()"/>
       </xsl:with-param>
     </xsl:call-template>

     <xsl:if test="d:bibliography">
       <xsl:call-template name="page.sequence">
         <xsl:with-param name="master-reference">back</xsl:with-param>
         <xsl:with-param name="content">
           <xsl:apply-templates select="bibliography"/>
         </xsl:with-param>
       </xsl:call-template>
     </xsl:if>
   </xsl:template>

    

   <xsl:template name="page.number.format">
     <xsl:param name="element" select="local-name(.)"/>
     <xsl:param name="master-reference" select="''"/>
     <xsl:if test="$debug != ''">
     </xsl:if>
   
     <xsl:choose>
       <xsl:when test="$element = 'toc'">i</xsl:when>
       <xsl:when test="$element = 'preface'">i</xsl:when>
       <xsl:when test="$element = 'dedication'">i</xsl:when>
       <xsl:otherwise>1</xsl:otherwise>
     </xsl:choose>
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

    <xsl:template name="table.row.properties">
        <xsl:variable name="role" select="ancestor::d:table/@role"/>

      <xsl:variable name="tabstyle">
        <xsl:call-template name="tabstyle"/>
      </xsl:variable>

      <xsl:variable name="bgcolor">
        <xsl:call-template name="dbfo-attribute">
          <xsl:with-param name="pis" select="processing-instruction('dbfo')"/>
          <xsl:with-param name="attribute" select="'bgcolor'"/>
        </xsl:call-template>
      </xsl:variable>
      
      <xsl:variable name="rownum">
        <xsl:number from="tgroup" count="row"/>
      </xsl:variable>
      <!--
      <xsl:message >
          <xsl:if test="$role != 'receive-hours' and $role !=
              'long-metrics' and $role != 'cutover-costs'">
            <xsl:value-of select="$role"/>
              
          </xsl:if>
      </xsl:message>
      -->

      <xsl:choose >

          <xsl:when test="@role = 'continuation-label'">
              
          </xsl:when>
            <xsl:when test="parent::d:tfoot">
                <xsl:attribute name="border-before-style">solid</xsl:attribute>
                <xsl:attribute name="border-before-width">1</xsl:attribute>
            </xsl:when>
            <xsl:when test="parent::d:thead">
                <xsl:attribute name="border-after-style">solid</xsl:attribute>
                <xsl:attribute name="border-after-width">1</xsl:attribute>
            </xsl:when>
            <xsl:when test="position() = last() and ($role='receive-hours' or
                $role = 'cutover-costs' or $role = 'projected-summary')">
                <xsl:attribute name="border-before-style">solid</xsl:attribute>
                <xsl:attribute name="border-before-width">1</xsl:attribute>
                <xsl:attribute name="font-weight">bold</xsl:attribute>
            </xsl:when>
      </xsl:choose>

    </xsl:template>

    <xsl:template name="table.layout">
      <xsl:param name="table.content" select="NOTANODE"/>
      <xsl:choose>
          <xsl:when test="@role='overall-costs'">
              <fo:table table-layout="fixed" width="100%">
                <fo:table-column column-width="proportional-column-width(1)"/>
                <fo:table-body>
                  <fo:table-row >
                    <fo:table-cell display-align="center">
                        <xsl:copy-of select="$table.content"/>
                    </fo:table-cell>
                  </fo:table-row>
                </fo:table-body>
              </fo:table>
          </xsl:when>
          <xsl:when test="@role = 'overall-costs_'">
              <fo:table table-layout="fixed" width="100%">
                <fo:table-column column-width="proportional-column-width(1)"/>
                <fo:table-column column-width="100mm"/>
                <fo:table-column column-width="proportional-column-width(1)"/>
                <fo:table-body>
                  <fo:table-row>
                    <fo:table-cell column-number="2">
                        <xsl:copy-of select="$table.content"/>
                    </fo:table-cell>
                  </fo:table-row>
                </fo:table-body>
              </fo:table>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="$table.content"/>
          </xsl:otherwise>
      </xsl:choose>
    </xsl:template>

   
   <!--STANDARD STUFF-->

    <xsl:template match="processing-instruction('hard-pagebreak')">
        <fo:block break-after='page'/>
    </xsl:template>
   
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
