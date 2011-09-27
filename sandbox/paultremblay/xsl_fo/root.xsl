<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id: root.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->

    <!-- The stylesheet for handling root elements, such as document-->


    <!--attributes for the sequence of pages for the main body. -->
    <xsl:attribute-set name="page-sequence" >
        <xsl:attribute name= "format">1</xsl:attribute> 
        <xsl:attribute name= "initial-page-number">1</xsl:attribute> 
    </xsl:attribute-set>

    <!--default for fo:flow-->
    <xsl:attribute-set name="default-flow">
    </xsl:attribute-set>

    <!--region-body-->
    <xsl:attribute-set name="region-body">
        <xsl:attribute name="margin-top">.75in</xsl:attribute>
        <xsl:attribute name="margin-bottom">.75in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="region-before">
        <xsl:attribute name="extent">.75in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="region-after">
        <xsl:attribute name="extent">.75in</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="body-flow" use-attribute-sets="default-flow">
    </xsl:attribute-set>

    <xsl:attribute-set name="footnote-separator-flow">
    </xsl:attribute-set>

    <xsl:attribute-set name="footnote-separator-block">
        <!--putting space before can create way too much space-->
        <!--
        <xsl:attribute name="space-before">5mm</xsl:attribute>
        <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
        -->
    </xsl:attribute-set>

    <xsl:template match="/">
        <xsl:element name="fo:root">
            <xsl:call-template name="make-pages"/>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <!--title, bibliographic-info dedication, abstract toc-->
    <xsl:template match = "document">
        <xsl:call-template name='test-params'/>
        <fo:page-sequence master-reference="simple-page" xsl:use-attribute-sets="page-sequence">
            <xsl:apply-templates select="/document/decoration/header" mode="header"/>
            <xsl:apply-templates select="/document/decoration/footer" mode="footer"/>
            <xsl:call-template name="make-footnote-separator"/>
            <fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="body-flow">
                <xsl:apply-templates/>
            </fo:flow>
        </fo:page-sequence>
    </xsl:template>


    <xsl:template name="make-footnote-separator">
        <fo:static-content role = "footnote-separator" 
            xsl:use-attribute-sets = "footnote-separator-flow" flow-name="xsl-footnote-separator">
           <fo:block xsl:use-attribute-sets = "footnote-separator-block">
                <fo:leader leader-pattern="rule" leader-length="100%"/>
           </fo:block>
        </fo:static-content>
    </xsl:template>

    <xsl:attribute-set name="page-size">
	<xsl:attribute name="margin-left">1.0in</xsl:attribute>
	<xsl:attribute name="margin-right">1.0in</xsl:attribute>
	<xsl:attribute name="margin-top">1.0in</xsl:attribute>
	<xsl:attribute name="margin-bottom">1.0in</xsl:attribute>
    </xsl:attribute-set>


    <xsl:template name="make-pages">
         <fo:layout-master-set>
          <fo:simple-page-master master-name="simple-page"  xsl:use-attribute-sets="page-size">
              <xsl:variable name="top">
                  <xsl:choose>
                      <xsl:when test="/document/decoration/header and $spacing-header = ''">
                          <xsl:text>.75in</xsl:text>
                      </xsl:when>
                      <xsl:when test="document/decoration/header">
                          <xsl:value-of select="$spacing-header"/>
                      </xsl:when>
                      <xsl:otherwise>
                          <xsl:text>0in</xsl:text>
                      </xsl:otherwise>
                  </xsl:choose>
              </xsl:variable>
              <xsl:variable name="bottom">
                  <xsl:choose>
                      <xsl:when test="/document/decoration/footer and $spacing-footer = ''">
                          <xsl:text>.75in</xsl:text>
                      </xsl:when>
                      <xsl:when test="document/decoration/footer">
                          <xsl:value-of select="$spacing-footer"/>
                      </xsl:when>
                      <xsl:otherwise>
                          <xsl:text>0in</xsl:text>
                      </xsl:otherwise>
                  </xsl:choose>
              </xsl:variable>
            <fo:region-body margin-top="{$top}" margin-bottom="{$bottom}" xsl:use-attribute-sets="region-body"/>
            <xsl:choose>
               <xsl:when test = "/document/decoration/header and /document/decoration/footer">
                   <fo:region-before  xsl:use-attribute-sets="region-before"/>
                   <fo:region-after  xsl:use-attribute-sets="region-after"/>
               </xsl:when>
               <xsl:when test = "/document/decoration/header">
                   <fo:region-before  xsl:use-attribute-sets="region-before"/>
               </xsl:when>
               <xsl:when test = "/document/decoration/footer">
                   <fo:region-after  xsl:use-attribute-sets="region-after"/>
               </xsl:when>
           </xsl:choose>
          </fo:simple-page-master>
         </fo:layout-master-set>
    </xsl:template>


</xsl:stylesheet>
