<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id: docutils_to_fo.xsl 6604 2011-01-20 04:56:24Z paultremblay $ -->

    <!--
    TODO
    make the xsl:attribute-set name="region-body" functional. Right now, the 
    template make-region-body uses the region-body attribute set, even if 
    a toc were being written. Need to pass the parmater section and determine
    what type of attribute set to use.
    -->

    <!-- This template sets up the page styles, needed at the start of an 
    FO document.  

    The parameter layout-page is inherited from other the parameter stylesheet.
    This parameter must be
    1.blank (""). The stylesheet interprets an empty string as simple
    2. simple
    3. first
    4. odd-even
    5. first-odd-even

    The parameters suppress-first-header and suppress-first-footer are also inherited
    from the parameters stylesheet.
    -->

    <!--paper size for the whole document (Converstion only allows one 
    size, at least at this point-->
    <xsl:attribute-set name="paper-size-simple-page-master">
	<xsl:attribute name="page-width">8.5in</xsl:attribute>
	<xsl:attribute name="page-height">11in</xsl:attribute>
    </xsl:attribute-set>

    <!--default for all pages-->
    <xsl:attribute-set name="default-simple-page-master">
	<xsl:attribute name="margin-left">1.0in</xsl:attribute>
	<xsl:attribute name="margin-right">1.0in</xsl:attribute>
	<xsl:attribute name="margin-top">1.0in</xsl:attribute>
	<xsl:attribute name="margin-bottom">1.0in</xsl:attribute>
    </xsl:attribute-set>


    <!--properties for simple page.
    Use simple page when all the pages will be the same with 
    the same headers and footers-->
    <xsl:attribute-set name="simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master">
    </xsl:attribute-set>

    <!--properties for first page.
    Used for either the "first" layout, or "first-odd-even" layout.
    With the first, you can set different margins for the first page,
    and can suppress the header or footer on the first page-->
    <xsl:attribute-set name="first-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master">
    </xsl:attribute-set>

    <!--properties for the body page.
    The body page defines the pages in a "first" layout that are not the 
    first page-->
    <xsl:attribute-set name="body-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master" >
    </xsl:attribute-set>

    <!-- properties for the odd page.
    The odd-page is used in either a "odd-even" layout or "first-odd-even" 
    layout. With odd-even, you can specify different margins for odd and even
    pages. You cannot have different headers and footers-->
    <xsl:attribute-set name="odd-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master">
    </xsl:attribute-set>

    <!-- properties for the even page.
    See the odd page above-->
    <xsl:attribute-set name="even-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master">
    </xsl:attribute-set>

    <!--attribute sets for toc pages; traits are inherited from the body pages-->
    <xsl:attribute-set name="toc-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master"/>
    <xsl:attribute-set name="toc-first-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master"/>
    <xsl:attribute-set name="toc-body-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master"/>
    <xsl:attribute-set name="toc-even-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master"/>
    <xsl:attribute-set name="toc-odd-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master"/>

    <!--attribute sets for front-matter pages; traits are inherited from the page size only pages-->
    <xsl:attribute-set name="front-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master"/> 
    <xsl:attribute-set name="front-first-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master"/>
    <xsl:attribute-set name="front-body-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master"/>
    <xsl:attribute-set name="front-even-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master"/>
    <xsl:attribute-set name="front-odd-simple-page-master" use-attribute-sets="paper-size-simple-page-master default-simple-page-master"/>

    <!--the extent for the header at the top of the page-->
    <xsl:attribute-set name="header-region-before">
	<xsl:attribute name="extent">.75in</xsl:attribute>
    </xsl:attribute-set>

    <!--the extent for the footer at the top of the page-->
    <xsl:attribute-set name="footer-region-after">
	<xsl:attribute name="extent">.75in</xsl:attribute>
    </xsl:attribute-set>

    <!--Not used for anything at this point-->
    <xsl:attribute-set name="front-matter-region-body">
    </xsl:attribute-set>

    <!--Not used for anything at this point-->
    <xsl:attribute-set name="region-body">
    </xsl:attribute-set>


    <!--default spacing for footer and header spacing-->
    <xsl:variable name="default-spacing-header">.75in</xsl:variable>
    <xsl:variable name="default-spacing-footer">.75in</xsl:variable>


    <!--the main template. Calls on other templates to make fo:simple-page-master,
    then calls on other templates to make fo:page-sequence-master-->
    <xsl:template name="make-pages">
        <xsl:if test="$test='True'">
            <xsl:message>
                <xsl:text>value of $page-sequence-type="</xsl:text>
                <xsl:value-of select="$page-sequence-type"/>
                <xsl:text>"&#xA;</xsl:text> 
                <xsl:text>value of $page-layout="</xsl:text>
                <xsl:value-of select="$page-layout"/>
                <xsl:text>"&#xA;</xsl:text>
                <xsl:text>value of $layout-page="</xsl:text>
                <xsl:value-of select="$layout-page"/>
                <xsl:text>"</xsl:text>
            </xsl:message>
        </xsl:if>
        <fo:layout-master-set>
            <xsl:call-template name="page-properties"/>
            <xsl:call-template name="page-sequence"/>
            <xsl:if test= "$page-sequence-type='front-toc-body' or $page-sequence-type = 'front-body'">
                <xsl:call-template name="make-front-matter-pages"/> 
            </xsl:if>
            <xsl:if test= "$page-sequence-type='front-toc-body' or $page-sequence-type = 'toc-body'
                or $page-sequence-type='toc-combined-body'">
                <xsl:call-template name="make-toc-pages"/> 
            </xsl:if>
        </fo:layout-master-set>
    </xsl:template>

    <!--call on the appropriate template to make simpe-page-master, depending on 
    the parameter $layout-page-->
    <xsl:template name="page-properties">
        <!--set up the physical properties of the pages-->
        <xsl:choose>
            <xsl:when test="$layout-page = '' or $layout-page = 'simple'">
                <xsl:call-template name="make-simple-page"/>
            </xsl:when>
            <xsl:when test="$layout-page = 'first'">
                <xsl:call-template name="make-first-page"/>
                <xsl:call-template name="make-body-page"/>
            </xsl:when>
            <xsl:when test="$layout-page = 'odd-even'">
                <xsl:call-template name="make-odd-page"/>
                <xsl:call-template name="make-even-page"/>
            </xsl:when>
            <xsl:when test="$layout-page = 'first-odd-even'">
                <xsl:call-template name="make-first-page"/>
                <xsl:call-template name="make-odd-page"/>
                <xsl:call-template name="make-even-page"/>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <!--a single element, very tricky to create because footers or headers
    may or may not be present, and the user has to override the defaults-->
    <xsl:template name="make-region-body">
        <xsl:param name="page-type"/>
        <xsl:param name="section"/>
        <xsl:variable name="margin-top">
            <xsl:choose>
                <xsl:when test="$page-type = 'first' and /document/container[@classes = 'first-header']|/document/decoration/header">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'odd' and /document/container[@classes = 'odd-header']|/document/decoration/header">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'even' and /document/container[@classes = 'even-header']|/document/decoration/header">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'body' and /document/container[@classes = 'body-header']|/document/decoration/header">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'toc-first' and 
                    /document/container[@classes = 'toc-first-header']|/document/decoration/header">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'toc-odd' 
                    and /document/container[@classes = 'toc-odd-header']|/document/decoration/header">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'toc-even' 
                    and /document/container[@classes = 'toc-even-header']|/document/decoration/header">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'toc-body' 
                    and /document/container[@classes = 'toc-body-header']|/document/decoration/header">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'simple' and /document/decoration/header">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>0in</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="margin-bottom">
            <xsl:choose>
                <xsl:when test="$page-type = 'first' and /document/container[@classes = 'first-footer']|document/decoration/footer">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'odd' and /document/container[@classes = 'odd-footer']|document/decoration/footer">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'even' and /document/container[@classes = 'even-footer']|document/decoration/footer">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'body' and /document/container[@classes = 'body-footer']|document/decoration/footer">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'simple' and /document/decoration/footer">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'toc-first' 
                    and /document/container[@classes = 'toc-first-footer']|document/decoration/footer">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'toc-odd' 
                    and /document/container[@classes = 'toc-odd-footer']|document/decoration/footer">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'toc-even' 
                    and /document/container[@classes = 'toc-even-footer']|document/decoration/footer">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'toc-body' 
                    and /document/container[@classes = 'toc-body-footer']|document/decoration/footer">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:when test="$page-type = 'simple' and /document/decoration/footer">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>0in</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:choose>
            <xsl:when test="$custom-spc-header-footer = 'true'">
                <fo:region-body  xsl:use-attribute-sets="region-body"/>
            </xsl:when>
            <xsl:otherwise>
                <fo:region-body margin-top="{$margin-top}" margin-bottom="{$margin-bottom}" xsl:use-attribute-sets="region-body"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!--make the simple-page-master with the appropriate master-name
    As with the other templates that create simple-page-master, it determines if there are headers 
    and footers, and if so, makes room for them-->
    <xsl:template name="make-simple-page">
        <fo:simple-page-master xsl:use-attribute-sets="simple-page-master" master-name="simple-page">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'simple'"/>
            </xsl:call-template>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "header-region-before" region-name="simple-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "footer-region-after" region-name="simple-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <!--same as above, but this template also determines if no space should be created, based
    on the suppress-first-page-footer and suppress-first-page-header parameters-->
    <xsl:template name="make-first-page">
        <fo:simple-page-master xsl:use-attribute-sets="first-simple-page-master" master-name="first">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'first'"/>
            </xsl:call-template>
            <xsl:if test="document/container[@classes='first-header']|document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "header-region-before" region-name="first-header"/>
            </xsl:if>
            <xsl:if test="document/container[@classes = 'first-footer']|document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "footer-region-after" region-name="first-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="make-body-page">
        <fo:simple-page-master xsl:use-attribute-sets="body-simple-page-master" master-name="body">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'body'"/>
            </xsl:call-template>
            <xsl:if test="document/container[@classes = 'body-header']|document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "header-region-before" region-name = "body-header"/>
            </xsl:if>
            <xsl:if test="document/container[@classes='body-footer']|document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "footer-region-after" region-name = "body-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="make-odd-page">
        <fo:simple-page-master xsl:use-attribute-sets="odd-simple-page-master" master-name="odd">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'odd'"/>
            </xsl:call-template>
            <xsl:if test="document/container[@classes = 'odd-header']|document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "header-region-before" region-name="odd-header"/>
            </xsl:if>
            <xsl:if test="document/container[@classes = 'odd-footer']|document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "footer-region-after" region-name="odd-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="make-even-page">
        <fo:simple-page-master xsl:use-attribute-sets="even-simple-page-master" master-name="even">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'even'"/>
            </xsl:call-template>
            <xsl:if test="document/container[@classes='even-header']|document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "header-region-before" region-name="even-header"/>
            </xsl:if>
            <xsl:if test="document/container[@classes='even-footer']|document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "footer-region-after" region-name="even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>


    <!--Create the fo:page-sequence-master, depending on the parameter layout-page-->
    <xsl:template name="page-sequence">
        <xsl:choose>
            <xsl:when test="$layout-page = '' or $layout-page = 'simple'">
                <fo:page-sequence-master master-name = "pages" >
                    <fo:repeatable-page-master-reference master-reference = "simple-page"/>
                </fo:page-sequence-master>
            </xsl:when>
            <xsl:when test="$layout-page = 'first'">
                <fo:page-sequence-master master-name = "pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "first" page-position = "first"/>
                        <fo:conditional-page-master-reference master-reference = "body" page-position = "rest"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$layout-page = 'odd-even'">
                <fo:page-sequence-master master-name = "pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "odd" odd-or-even = "odd"/>
                        <fo:conditional-page-master-reference master-reference = "even" odd-or-even = "even"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$layout-page = 'first-odd-even'">
                <fo:page-sequence-master master-name = "pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "first" page-position = "first"/>
                        <fo:conditional-page-master-reference master-reference = "odd" odd-or-even = "odd"/>
                        <fo:conditional-page-master-reference master-reference = "even" odd-or-even = "even"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <!--TOC-->
    <!--Just a repeat of above-->


    <xsl:template name="make-toc-pages">
        <xsl:call-template name="toc-page-properties"/>
        <xsl:call-template name="toc-page-sequence"/>
    </xsl:template>

    <!--call on the appropriate template to make simpe-page-master, depending on 
    the parameter $layout-page-->
    <xsl:template name="toc-page-properties">
        <!--set up the physical properties of the pages-->
        <xsl:choose>
            <xsl:when test="$layout-page = '' or $layout-page = 'simple'">
                <xsl:call-template name="toc-make-simple-page"/>
            </xsl:when>
            <xsl:when test="$layout-page = 'first'">
                <xsl:call-template name="toc-make-first-page"/>
                <xsl:call-template name="toc-make-body-page"/>
            </xsl:when>
            <xsl:when test="$layout-page = 'odd-even'">
                <xsl:call-template name="toc-make-odd-page"/>
                <xsl:call-template name="toc-make-even-page"/>
            </xsl:when>
            <xsl:when test="$layout-page = 'first-odd-even'">
                <xsl:call-template name="toc-make-first-page"/>
                <xsl:call-template name="toc-make-odd-page"/>
                <xsl:call-template name="toc-make-even-page"/>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <!--make the simple-page-master for toc-->
    <xsl:template name="toc-make-simple-page">
        <fo:simple-page-master xsl:use-attribute-sets="toc-simple-page-master" master-name="toc-simple-page">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'simple'"/>
            </xsl:call-template>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "header-region-before" region-name="odd-even-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "footer-region-after" region-name="odd-even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <!--same as for non toc pages,-->
    <xsl:template name="toc-make-first-page">
        <fo:simple-page-master xsl:use-attribute-sets="toc-first-simple-page-master" master-name="toc-first">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'toc-first'"/>
            </xsl:call-template>
            <xsl:if test="document/container[@classes='toc-first-header']|document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "header-region-before" region-name="toc-first-header"/>
            </xsl:if>
            <xsl:if test="document/container[@classes = 'toc-first-footer']|document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "footer-region-after" region-name="toc-first-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="toc-make-body-page">
        <fo:simple-page-master xsl:use-attribute-sets="toc-body-simple-page-master" master-name="toc-body">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'toc-body'"/>
            </xsl:call-template>
            <xsl:if test="document/container[@classes = 'toc-body-header']|document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "header-region-before" region-name = "toc-body-header"/>
            </xsl:if>
            <xsl:if test="document/container[@classes='toc-body-footer']|document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "footer-region-after" region-name = "toc-body-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="toc-make-odd-page">
        <fo:simple-page-master xsl:use-attribute-sets="toc-odd-simple-page-master" master-name="toc-odd">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'toc-odd'"/>
            </xsl:call-template>
            <xsl:if test="document/container[@classes = 'toc-odd-header']|document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "header-region-before" region-name="toc-odd-header"/>
            </xsl:if>
            <xsl:if test="document/container[@classes = 'toc-odd-footer']|document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "footer-region-after" region-name="toc-odd-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="toc-make-even-page">
        <fo:simple-page-master xsl:use-attribute-sets="toc-even-simple-page-master" master-name="toc-even">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'toc-even'"/>
            </xsl:call-template>
            <xsl:if test="document/container[@classes='toc-even-header']|document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "header-region-before" region-name="toc-even-header"/>
            </xsl:if>
            <xsl:if test="document/container[@classes='toc-even-footer']|document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "footer-region-after" region-name="toc-even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>


    <!--Create the fo:page-sequence-master, depending on the parameter layout-page-->
    <xsl:template name="toc-page-sequence">
        <!--do I want these params? They seem unneeded and potentially trouble making-->
        <xsl:choose>
            <xsl:when test="$layout-page = '' or $layout-page = 'simple'">
                <fo:page-sequence-master master-name = "toc-pages" >
                    <fo:repeatable-page-master-reference master-reference = "toc-simple-page"/>
                </fo:page-sequence-master>
            </xsl:when>
            <xsl:when test="$layout-page = 'first'">
                <fo:page-sequence-master master-name = "toc-pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "toc-first" page-position = "first"/>
                        <fo:conditional-page-master-reference master-reference = "toc-body" page-position = "rest"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$layout-page = 'odd-even'">
                <fo:page-sequence-master master-name = "toc-pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "toc-odd" odd-or-even = "odd"/>
                        <fo:conditional-page-master-reference master-reference = "toc-even" odd-or-even = "even"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$layout-page = 'first-odd-even'">
                <fo:page-sequence-master master-name = "toc-pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "toc-first" page-position = "first"/>
                        <fo:conditional-page-master-reference master-reference = "toc-odd" odd-or-even = "odd"/>
                        <fo:conditional-page-master-reference master-reference = "toc-even" odd-or-even = "even"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
        </xsl:choose>
    </xsl:template>


    <!--FRONT MATTER (no page numbers)-->

    <xsl:template name="make-front-matter-pages">
        <xsl:call-template name="front-matter-page-properties"/>
        <xsl:call-template name="front-matter-page-sequence"/>
    </xsl:template>

    <!--call on the appropriate template to make simpe-page-master, depending on 
    the parameter $layout-page-->
    <xsl:template name="front-matter-page-properties">
        <!--set up the physical properties of the pages-->
        <xsl:choose>
            <xsl:when test="$layout-page = '' or $layout-page = 'simple'">
                <fo:simple-page-master xsl:use-attribute-sets="front-simple-page-master" master-name="front-matter-simple-page">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
            </xsl:when>
            <xsl:when test="$layout-page = 'first'">
                <fo:simple-page-master xsl:use-attribute-sets="front-first-simple-page-master" master-name="front-matter-first">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
                <fo:simple-page-master xsl:use-attribute-sets="front-body-simple-page-master" master-name="front-matter-body">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
            </xsl:when>
            <xsl:when test="$layout-page = 'odd-even'">
                <fo:simple-page-master xsl:use-attribute-sets="front-odd-simple-page-master" master-name="front-matter-odd">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
                <fo:simple-page-master xsl:use-attribute-sets="front-even-simple-page-master" master-name="front-matter-even">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
            </xsl:when>
            <xsl:when test="$layout-page = 'first-odd-even'">
                <fo:simple-page-master xsl:use-attribute-sets="front-first-simple-page-master" master-name="front-matter-first">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
                <fo:simple-page-master xsl:use-attribute-sets="front-odd-simple-page-master" master-name="front-matter-odd">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
                <fo:simple-page-master xsl:use-attribute-sets="front-even-simple-page-master" master-name="front-matter-even">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <!--Create the fo:page-sequence-master, depending on the parameter layout-page-->
    <xsl:template name="front-matter-page-sequence">
        <xsl:choose>
            <xsl:when test="$layout-page = 'simple'">
                <fo:page-sequence-master master-name = "front-matter-pages" >
                    <fo:repeatable-page-master-reference master-reference = "front-matter-simple-page"/>
                </fo:page-sequence-master>
            </xsl:when>
            <xsl:when test="$layout-page = 'first'">
                <fo:page-sequence-master master-name = "front-matter-pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "front-matter-first" page-position = "first"/>
                        <fo:conditional-page-master-reference master-reference = "front-matter-body" page-position = "rest"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$layout-page = 'odd-even'">
                <fo:page-sequence-master master-name = "front-matter-pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "front-matter-odd" odd-or-even = "odd"/>
                        <fo:conditional-page-master-reference master-reference = "front-matter-even" odd-or-even = "even"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$layout-page = 'first-odd-even'">
                <fo:page-sequence-master master-name = "front-matter-pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "front-matter-first" page-position = "first"/>
                        <fo:conditional-page-master-reference master-reference = "front-matter-odd" odd-or-even = "odd"/>
                        <fo:conditional-page-master-reference master-reference = "front-matter-even" odd-or-even = "even"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
        </xsl:choose>
    </xsl:template>


</xsl:stylesheet>
