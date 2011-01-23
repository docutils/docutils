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

    The parameter page-layout is inherited from other the parameter stylesheet.
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
    <xsl:attribute-set name="paper-size">
	<xsl:attribute name="page-width">8.5in</xsl:attribute>
	<xsl:attribute name="page-height">11in</xsl:attribute>
    </xsl:attribute-set>

    <!--default for all pages-->
    <xsl:attribute-set name="default-page-setup">
	<xsl:attribute name="margin-left">1.0in</xsl:attribute>
	<xsl:attribute name="margin-right">1.0in</xsl:attribute>
	<xsl:attribute name="margin-top">1.0in</xsl:attribute>
	<xsl:attribute name="margin-bottom">1.0in</xsl:attribute>
    </xsl:attribute-set>


    <!--properties for simple page.
    Use simple page when all the pages will be the same with 
    the same headers and footers-->
    <xsl:attribute-set name="simple-page" use-attribute-sets="paper-size default-page-setup">
    </xsl:attribute-set>

    <!--properties for first page.
    Used for either the "first" layout, or "first-odd-even" layout.
    With the first, you can set different margins for the first page,
    and can suppress the header or footer on the first page-->
    <xsl:attribute-set name="first-page" use-attribute-sets="paper-size default-page-setup">
    </xsl:attribute-set>

    <!--properties for the body page.
    The body page defines the pages in a "first" layout that are not the 
    first page-->
    <xsl:attribute-set name="body-page" use-attribute-sets="paper-size default-page-setup" >
    </xsl:attribute-set>

    <!-- properties for the odd page.
    The odd-page is used in either a "odd-even" layout or "first-odd-even" 
    layout. With odd-even, you can specify different margins for odd and even
    pages. You cannot have different headers and footers-->
    <xsl:attribute-set name="odd-page" use-attribute-sets="paper-size default-page-setup">
    </xsl:attribute-set>

    <!-- properties for the even page.
    See the odd page above-->
    <xsl:attribute-set name="even-page" use-attribute-sets="paper-size default-page-setup">
    </xsl:attribute-set>

    <!--attribute sets for toc pages; traits are inherited from the body pages-->
    <xsl:attribute-set name="toc-simple-page" use-attribute-sets="paper-size default-page-setup"/>
    <xsl:attribute-set name="toc-first-page" use-attribute-sets="paper-size default-page-setup"/>
    <xsl:attribute-set name="toc-body-page" use-attribute-sets="paper-size default-page-setup"/>
    <xsl:attribute-set name="toc-even-page" use-attribute-sets="paper-size default-page-setup"/>
    <xsl:attribute-set name="toc-odd-page" use-attribute-sets="paper-size default-page-setup"/>

    <!--attribute sets for front-matter pages; traits are inherited from the page size only pages-->
    <xsl:attribute-set name="front-matter-simple-page" use-attribute-sets="paper-size default-page-setup"/> 
    <xsl:attribute-set name="front-matter-first-page" use-attribute-sets="paper-size default-page-setup"/>
    <xsl:attribute-set name="front-matter-body-page" use-attribute-sets="paper-size default-page-setup"/>
    <xsl:attribute-set name="front-matter-even-page" use-attribute-sets="paper-size default-page-setup"/>
    <xsl:attribute-set name="front-matter-odd-page" use-attribute-sets="paper-size default-page-setup"/>
    <xsl:attribute-set name="front-matter-odd-page" use-attribute-sets="paper-size default-page-setup"/>

    <!--the extent for the header at the top of the page-->
    <xsl:attribute-set name="page-header">
	<xsl:attribute name="extent">.75in</xsl:attribute>
    </xsl:attribute-set>

    <!--the extent for the footer at the top of the page-->
    <xsl:attribute-set name="page-footer">
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
        <fo:layout-master-set>
            <xsl:call-template name="page-properties">
                <xsl:with-param name="page-layout" select="$page-layout"/> 
            </xsl:call-template>
            <xsl:call-template name="page-sequence">
                <xsl:with-param name="page-layout" select="$page-layout"/> 
            </xsl:call-template>
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
    the parameter $page-layout-->
    <xsl:template name="page-properties">
        <!--set up the physical properties of the pages-->
        <xsl:param name="page-layout"/>
        <xsl:choose>
            <xsl:when test="$page-layout = '' or $page-layout = 'simple'">
                <xsl:call-template name="make-simple-page"/>
            </xsl:when>
            <xsl:when test="$page-layout = 'first'">
                <xsl:call-template name="make-first-page"/>
                <xsl:call-template name="make-body-page"/>
            </xsl:when>
            <xsl:when test="$page-layout = 'odd-even'">
                <xsl:call-template name="make-odd-page"/>
                <xsl:call-template name="make-even-page"/>
            </xsl:when>
            <xsl:when test="$page-layout = 'first-odd-even'">
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
                <xsl:when test="$page-layout = 'first' and $suppress-first-page-header = 'True' and $page-type = 'first'">
                    <xsl:text>0in</xsl:text>
                </xsl:when>
                <xsl:when test="$page-layout = 'first-odd-even' and $suppress-first-page-header = 'True' and $page-type = 'first'">
                    <xsl:text>0in</xsl:text>
                </xsl:when>
                <xsl:when test="$spacing-header != ''">
                    <xsl:value-of select="$spacing-header"/>
                </xsl:when>
                <xsl:when test="/document/decoration/header">
                    <xsl:value-of select="$default-spacing-header"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>0in</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="margin-bottom">
            <xsl:choose>
                <xsl:when test="$page-layout = 'first' and $suppress-first-page-footer = 'True' and $page-type = 'first'">
                    <xsl:text>0in</xsl:text>
                </xsl:when>
                <xsl:when test="$page-layout = 'first-odd-even' and $suppress-first-page-footer = 'True' and $page-type = 'first'">
                    <xsl:text>0in</xsl:text>
                </xsl:when>
                <xsl:when test="$spacing-footer != ''">
                    <xsl:value-of select="$spacing-footer"/>
                </xsl:when>
                <xsl:when test="/document/decoration/footer">
                    <xsl:value-of select="$default-spacing-footer"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>0in</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <fo:region-body margin-top="{$margin-top}" margin-bottom="{$margin-bottom}" xsl:use-attribute-sets="region-body"/>
    </xsl:template>

    <!--make the simple-page-master with the appropriate master-name
    As with the other templates that create simple-page-master, it determines if there are headers 
    and footers, and if so, makes room for them-->
    <xsl:template name="make-simple-page">
        <fo:simple-page-master xsl:use-attribute-sets="simple-page" master-name="simple-page">  
            <xsl:call-template name="make-region-body"/>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="odd-even-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="odd-even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <!--same as above, but this template also determines if no space should be created, based
    on the suppress-first-page-footer and suppress-first-page-header parameters-->
    <xsl:template name="make-first-page">
        <fo:simple-page-master xsl:use-attribute-sets="first-page" master-name="first">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'first'"/>
            </xsl:call-template>
            <xsl:if test="document/decoration/header and $suppress-first-page-header != 'True'">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="first-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer and $suppress-first-page-footer != 'True'">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="first-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="make-body-page">
        <fo:simple-page-master xsl:use-attribute-sets="body-page" master-name="body">  
            <xsl:call-template name="make-region-body"/>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name = "odd-even-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name = "odd-even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="make-odd-page">
        <fo:simple-page-master xsl:use-attribute-sets="odd-page" master-name="odd">  
            <xsl:call-template name="make-region-body"/>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="odd-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="odd-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="make-even-page">
        <fo:simple-page-master xsl:use-attribute-sets="even-page" master-name="even">  
            <xsl:call-template name="make-region-body"/>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="even-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>


    <!--Create the fo:page-sequence-master, depending on the parameter page-layout-->
    <xsl:template name="page-sequence">
        <xsl:param name="page-layout"/>
        <xsl:param name="master-name" select="'pages'"/>
        <xsl:choose>
            <xsl:when test="$page-layout = '' or $page-layout = 'simple'">
                <fo:page-sequence-master master-name = "pages" >
                    <fo:repeatable-page-master-reference master-reference = "simple-page"/>
                </fo:page-sequence-master>
            </xsl:when>
            <xsl:when test="$page-layout = 'first'">
                <fo:page-sequence-master master-name = "pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "first" page-position = "first"/>
                        <fo:conditional-page-master-reference master-reference = "body" page-position = "rest"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$page-layout = 'odd-even'">
                <fo:page-sequence-master master-name = "pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "odd" odd-or-even = "odd"/>
                        <fo:conditional-page-master-reference master-reference = "even" odd-or-even = "even"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$page-layout = 'first-odd-even'">
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
    the parameter $page-layout-->
    <xsl:template name="toc-page-properties">
        <!--set up the physical properties of the pages-->
        <xsl:choose>
            <xsl:when test="$page-layout = '' or $page-layout = 'simple'">
                <xsl:call-template name="toc-make-simple-page"/>
            </xsl:when>
            <xsl:when test="$page-layout = 'first'">
                <xsl:call-template name="toc-make-first-page"/>
                <xsl:call-template name="toc-make-body-page"/>
            </xsl:when>
            <xsl:when test="$page-layout = 'odd-even'">
                <xsl:call-template name="toc-make-odd-page"/>
                <xsl:call-template name="toc-make-even-page"/>
            </xsl:when>
            <xsl:when test="$page-layout = 'first-odd-even'">
                <xsl:call-template name="toc-make-first-page"/>
                <xsl:call-template name="toc-make-odd-page"/>
                <xsl:call-template name="toc-make-even-page"/>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <!--make the simple-page-master for toc-->
    <xsl:template name="toc-make-simple-page">
        <fo:simple-page-master xsl:use-attribute-sets="toc-simple-page" master-name="toc-simple-page">  
            <xsl:call-template name="make-region-body"/>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="odd-even-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="odd-even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <!--same as for non toc pages,-->
    <xsl:template name="toc-make-first-page">
        <fo:simple-page-master xsl:use-attribute-sets="toc-first-page" master-name="toc-first">  
            <xsl:call-template name="make-region-body">
                <xsl:with-param name="page-type" select="'first'"/>
            </xsl:call-template>
            <xsl:if test="document/decoration/header and $suppress-first-page-header != 'True'">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="first-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer and $suppress-first-page-footer != 'True'">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="first-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="toc-make-body-page">
        <fo:simple-page-master xsl:use-attribute-sets="toc-body-page" master-name="toc-body">  
            <xsl:call-template name="make-region-body"/>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name = "odd-even-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name = "odd-even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="toc-make-odd-page">
        <fo:simple-page-master xsl:use-attribute-sets="toc-odd-page" master-name="toc-odd">  
            <xsl:call-template name="make-region-body"/>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="odd-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="odd-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="toc-make-even-page">
        <fo:simple-page-master xsl:use-attribute-sets="toc-even-page" master-name="toc-even">  
            <xsl:call-template name="make-region-body"/>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="even-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>


    <!--Create the fo:page-sequence-master, depending on the parameter page-layout-->
    <xsl:template name="toc-page-sequence">
        <!--do I want these params? They seem unneeded and potentially trouble making-->
        <xsl:param name="page-layout"/>
        <xsl:param name="master-name" select="'pages'"/>
        <xsl:choose>
            <xsl:when test="$page-layout = '' or $page-layout = 'simple'">
                <fo:page-sequence-master master-name = "toc-pages" >
                    <fo:repeatable-page-master-reference master-reference = "toc-simple-page"/>
                </fo:page-sequence-master>
            </xsl:when>
            <xsl:when test="$page-layout = 'first'">
                <fo:page-sequence-master master-name = "toc-pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "toc-first" page-position = "first"/>
                        <fo:conditional-page-master-reference master-reference = "toc-body" page-position = "rest"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$page-layout = 'odd-even'">
                <fo:page-sequence-master master-name = "toc-pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "toc-odd" odd-or-even = "odd"/>
                        <fo:conditional-page-master-reference master-reference = "toc-even" odd-or-even = "even"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$page-layout = 'first-odd-even'">
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
    the parameter $page-layout-->
    <xsl:template name="front-matter-page-properties">
        <!--set up the physical properties of the pages-->
        <xsl:choose>
            <xsl:when test="$page-layout = '' or $page-layout = 'simple'">
                <fo:simple-page-master xsl:use-attribute-sets="front-matter-simple-page" master-name="front-matter-simple-page">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
            </xsl:when>
            <xsl:when test="$page-layout = 'first'">
                <fo:simple-page-master xsl:use-attribute-sets="front-matter-first-page" master-name="front-matter-first-page">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
                <fo:simple-page-master xsl:use-attribute-sets="front-matter-body-page" master-name="front-matter-body-page">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
            </xsl:when>
            <xsl:when test="$page-layout = 'odd-even'">
                <fo:simple-page-master xsl:use-attribute-sets="front-matter-odd-page" master-name="front-matter-odd-page">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
                <fo:simple-page-master xsl:use-attribute-sets="front-matter-even-page" master-name="front-matter-even-page">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
            </xsl:when>
            <xsl:when test="$page-layout = 'first-odd-even'">
                <fo:simple-page-master xsl:use-attribute-sets="front-matter-first-page" master-name="front-matter-first-page">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
                <fo:simple-page-master xsl:use-attribute-sets="front-matter-odd-page" master-name="front-matter-odd-page">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
                <fo:simple-page-master xsl:use-attribute-sets="front-matter-even-page" master-name="front-matter-even-page">  
                    <fo:region-body xsl:use-attribute-sets="front-matter-region-body"/>
                </fo:simple-page-master>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <!--Create the fo:page-sequence-master, depending on the parameter page-layout-->
    <xsl:template name="front-matter-page-sequence">
        <xsl:choose>
            <xsl:when test="$page-layout = '' or $page-layout = 'simple'">
                <fo:page-sequence-master master-name = "front-matter-pages" >
                    <fo:repeatable-page-master-reference master-reference = "front-matter-simple-page"/>
                </fo:page-sequence-master>
            </xsl:when>
            <xsl:when test="$page-layout = 'first'">
                <fo:page-sequence-master master-name = "front-matter-pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "front-matter-first" page-position = "first"/>
                        <fo:conditional-page-master-reference master-reference = "front-matter-body" page-position = "rest"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$page-layout = 'odd-even'">
                <fo:page-sequence-master master-name = "toc-pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "front-matter-odd" odd-or-even = "odd"/>
                        <fo:conditional-page-master-reference master-reference = "front-matter-even" odd-or-even = "even"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$page-layout = 'first-odd-even'">
                <fo:page-sequence-master master-name = "toc-pages">
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
