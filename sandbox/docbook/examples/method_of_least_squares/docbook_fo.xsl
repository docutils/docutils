<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <!--
    <xsl:import main stylesheets. My catolog actually overwrites the URL address and fetches a local stylesheet./>
    -->
    <xsl:import href="http://docbook.sourceforge.net/release/xsl-ns/current/fo/docbook.xsl"/>
    <xsl:import href="methods_title_page.xsl"/>
    <xsl:import href="bibliography.xsl"/>
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
    <xsl:param name="header.rule">0</xsl:param><!--no header rules at top of page-->
    <xsl:param name="footer.rule">0</xsl:param><!-- no footer rules at top of page-->
    <xsl:param name="autotoc.label.separator">&#160;&#160;&#160;</xsl:param><!--get rid of dot in TOC-->
    <xsl:param name="body.start.indent">0pt</xsl:param><!--no indent for body text-->
    <xsl:param name="text-indent">12pt</xsl:param><!--a default for first lines indent in paragraphs-->
    <xsl:param name="toc.indent.width">0</xsl:param>
    <xsl:param name="body.font.master">12</xsl:param><!--change default size of font from 10 to 12-->
    <xsl:param name="admon.textlabel" select="0"></xsl:param>


    <xsl:attribute-set name="abstract.title.properties" >
        <xsl:attribute name="font-size">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--change default for level one section titles-->
    <xsl:attribute-set name="section.title.level1.properties">
        <xsl:attribute name="font-size">14pt</xsl:attribute>
        <xsl:attribute name="space-after.optimum">12pt</xsl:attribute>
        <xsl:attribute name="space-after.minimum">10pt</xsl:attribute>
        <xsl:attribute name="space-after.maximum">14pt</xsl:attribute>
    </xsl:attribute-set> 

    <xsl:attribute-set name="address">
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="font-size">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--Since we are using first indent, set space between paragraphs to 0-->
    <xsl:attribute-set name="normal.para.spacing">
          <xsl:attribute name="space-before.optimum">0em</xsl:attribute>
          <xsl:attribute name="space-before.minimum">0.0em</xsl:attribute>
          <xsl:attribute name="space-before.maximum">0.0em</xsl:attribute>
    </xsl:attribute-set>

    <!--indent abstract-->
    <xsl:attribute-set name="abstract.properties">
        <xsl:attribute name="start-indent">0.5in</xsl:attribute>
        <xsl:attribute name="end-indent">0.2in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="informalfigure.properties">
        <xsl:attribute name="start-indent">.5in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="equation.properties">
        <xsl:attribute name="start-indent">1in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="informalequation.properties">
        <xsl:attribute name="start-indent">1in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="normal.para.spacing">
    </xsl:attribute-set>

    <xsl:attribute-set name="para.properties" use-attribute-sets="normal.para.spacing">
        <xsl:attribute name="text-indent">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="first.para.properties">
        <xsl:attribute name="text-indent">0pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="note">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="text-indent">0pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="exercise" use-attribute-sets="note">
    </xsl:attribute-set>

    <xsl:attribute-set name="admonition.properties">
        <xsl:attribute name="start-indent">0pt</xsl:attribute>
        <xsl:attribute name="text-indent">0pt</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="biblioentry.properties" use-attribute-sets="normal.para.spacing">
        <xsl:attribute name="start-indent">1.5in</xsl:attribute>
        <xsl:attribute name="text-indent">-1.5in</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>
    <xsl:attribute-set name="component.title.properties">
    </xsl:attribute-set>

    <xsl:attribute-set name="toc.line.properties">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="space-before">12pt</xsl:attribute>
    </xsl:attribute-set>


    <!--Change name of Table of Contents to Contents-->
    <!--Get rid of "." after titles-->
    <!--change the xref from Equation 2.5 to equation (2.5)-->
    <xsl:param name="local.l10n.xml" select="document('')"/>  
    <l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0"> 
      <l:l10n language="en"> 
          <l:gentext key="TableofContents" text="Contents"/>
        <l:context name="title-numbered">
            <l:template name="section" text="%n &#160;&#160;&#160;%t"/>
        </l:context>
        <l:context name="xref-number">
           <l:template name="equation" text="%n"/>
        </l:context>
      </l:l10n>
    </l:i18n>

    <!--change default numbering for eqations-->
    <xsl:template match="d:equation" mode="label.markup">
      <xsl:choose>
        <xsl:when test="@label">
          <xsl:value-of select="@label"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:variable name="section-label">
                <xsl:apply-templates select="ancestor::d:section" mode="label.markup"/>
            </xsl:variable>
            <xsl:value-of select="$section-label"/>
            <xsl:text>.</xsl:text>
            <xsl:variable name="num">
                <xsl:number format="1" from="d:article" level="any"/>
            </xsl:variable>
            <xsl:choose>
                <xsl:when test="$num = 1">
                    <xsl:value-of select="$num"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$num - 1"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template match="d:note|d:para[@role='exercise']" mode="label.markup">
      <xsl:choose>
        <xsl:when test="@label">
          <xsl:value-of select="@label"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:variable name="section-label">
                <xsl:apply-templates select="ancestor::d:section" mode="label.markup"/>
            </xsl:variable>
            <xsl:value-of select="$section-label"/>
            <xsl:text>.</xsl:text>
          <xsl:number format="1" from="d:section" count="d:note|d:para[@role='exercise']" level="any"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <!--put otheraddr in own blocks-->
    <xsl:template match="d:otheraddr">
        <fo:block  xsl:use-attribute-sets="address">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>


    <xsl:template match="d:note/d:para[1]" priority="2">
        <fo:block>
            <fo:inline font-weight="bold">
                <xsl:text>Remark </xsl:text> 
                <xsl:apply-templates select=".." mode="label.markup"/>
                <xsl:text>. </xsl:text>
            </fo:inline>
            <xsl:text> </xsl:text>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="d:para[@role='exercise']">
        <fo:block xsl:use-attribute-sets="exercise">
            <fo:inline font-weight="bold">
                <xsl:text>Exercise </xsl:text> 
                <xsl:apply-templates select="." mode="label.markup"/>
                <xsl:text>. </xsl:text>
            </fo:inline>
            <xsl:text> </xsl:text>
            <fo:inline font-style="italic">
                <xsl:apply-templates/>
            </fo:inline>
        </fo:block>
    </xsl:template>

    <xsl:template match="d:important/d:para">
        <fo:block>
            <xsl:attribute name="font-weight">bold</xsl:attribute>
            <xsl:attribute name="text-indent">12pt</xsl:attribute>
            <xsl:attribute name="space-before">12pt</xsl:attribute>
            <xsl:attribute name="space-after">12pt</xsl:attribute>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
   <xsl:template name="header.content"/><!--no header-->


   <!--the following three templates change the formtting for first paragraphs-->
    <xsl:template name="is.first.para">
        <xsl:choose>
            <xsl:when test="ancestor::d:note">T</xsl:when>
            <xsl:when test="preceding-sibling::d:para"/>
            <xsl:otherwise>T</xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="d:para">
        <xsl:variable name="is.first.para">
            <xsl:call-template name="is.first.para"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$is.first.para = ''">
                <xsl:call-template name="para"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="first-para"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="para">
        <xsl:variable name="keep.together">
            <xsl:call-template name="pi.dbfo_keep-together"/>
        </xsl:variable>
        <fo:block xsl:use-attribute-sets="para.properties">
            <xsl:if test="$keep.together != ''">
                <xsl:attribute name="keep-together.within-column">
                    <xsl:value-of select="$keep.together"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:call-template name="anchor"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template name="first-para">
        <xsl:variable name="keep.together">
            <xsl:call-template name="pi.dbfo_keep-together"/>
        </xsl:variable>
        <fo:block xsl:use-attribute-sets="first.para.properties">
            <xsl:if test="$keep.together != ''">
                <xsl:attribute name="keep-together.within-column">
                    <xsl:value-of select="$keep.together"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:call-template name="anchor"/>
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template name="toc.line">
      <xsl:param name="toc-context" select="NOTANODE"/>

      <xsl:variable name="id">
        <xsl:call-template name="object.id"/>
      </xsl:variable>

      <xsl:variable name="label">
        <xsl:apply-templates select="." mode="label.markup"/>
      </xsl:variable>

      <fo:block xsl:use-attribute-sets="toc.line.properties">
        <fo:inline keep-with-next.within-line="always">
          <fo:basic-link internal-destination="{$id}">
            <xsl:if test="$label != ''">
              <xsl:copy-of select="$label"/>
              <xsl:value-of select="$autotoc.label.separator"/>
            </xsl:if>
            <xsl:apply-templates select="." mode="titleabbrev.markup"/>
          </fo:basic-link>
        </fo:inline>
        <fo:inline keep-together.within-line="always">
          <xsl:text> </xsl:text>
          <fo:leader leader-alignment="reference-area"
                     keep-with-next.within-line="always"/>
          <xsl:text> </xsl:text> 
          <fo:basic-link internal-destination="{$id}">
            <fo:page-number-citation ref-id="{$id}"/>
          </fo:basic-link>
        </fo:inline>
      </fo:block>
    </xsl:template>


   
   <!--STANDARD STUFF-->

    <xsl:template match="processing-instruction('hard-pagebreak')">
        <fo:block break-after='page'/>
    </xsl:template>
   
   <xsl:attribute-set name="toc.line.properties">
    <xsl:attribute name="text-align-last">justify</xsl:attribute>
    <xsl:attribute name="text-align">inherit</xsl:attribute>
   </xsl:attribute-set>

   



</xsl:stylesheet>
