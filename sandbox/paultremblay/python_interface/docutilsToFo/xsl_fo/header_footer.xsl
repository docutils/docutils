<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Date: 2011-01-09 02:51:33 -0500 (Sun, 09 Jan 2011) $ -->
    <!--
    This stylesheet handles headers and footers. It creates the fo:static-content 
    elements, and the child fo:block elements. Each paragraph up to three has its 
    own attriute set.
    -->

    <xsl:attribute-set name="header-block">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="first-header-block" use-attribute-sets = "header-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="odd-header-block" use-attribute-sets = "header-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="even-header-block" use-attribute-sets = "header-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="body-header-block" use-attribute-sets = "header-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="footer-block">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="first-footer-block" use-attribute-sets = "footer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="odd-footer-block" use-attribute-sets = "footer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="even-footer-block" use-attribute-sets = "footer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="body-footer-block" use-attribute-sets = "footer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-first-header-block" use-attribute-sets = "header-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-odd-header-block" use-attribute-sets = "header-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-even-header-block" use-attribute-sets = "header-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-body-header-block" use-attribute-sets = "header-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-first-footer-block" use-attribute-sets = "footer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-odd-footer-block" use-attribute-sets = "footer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-even-footer-block" use-attribute-sets = "footer-block">
    </xsl:attribute-set>

    <xsl:attribute-set name="toc-body-footer-block" use-attribute-sets = "footer-block">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
        <xsl:attribute name="space-before.conditionality">retain</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="paragraph-header-block">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="paragraph-footer-block">
        <xsl:attribute name="font-size">12pt</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
    </xsl:attribute-set>


    <xsl:template match="decoration">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="container[@classes='first-header']| 
            container[@classes='first-footer']|
            container[@classes='odd-header']|
            container[@classes='even-header']|
            container[@classes='odd-footer']|
            container[@classes='even-footer']|
            container[@classes='body-header']|
            container[@classes='body-footer']|
            container[@classes='toc-first-header']|
            container[@classes='toc-first-footer']|
            container[@classes='toc-odd-header']|
            container[@classes='toc-even-header']|
            container[@classes='toc-odd-footer']|
            container[@classes='toc-even-footer']|
            container[@classes='toc-body-header']|
            container[@classes='toc-body-footer'] "/>

    <xsl:template match="container[@classes='first-header']" mode= "header">
        <xsl:if test= "$layout-page = 'first' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="first-header">
                <fo:block role="header" xsl:use-attribute-sets="first-header-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='first-footer']" mode= "footer">
        <xsl:if test= "$layout-page = 'first' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="first-footer">
                <fo:block role="footer" xsl:use-attribute-sets="first-footer-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='odd-header']" mode= "header">
        <xsl:if test= "$layout-page = 'odd-even' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="odd-header">
                <fo:block role="header" xsl:use-attribute-sets="odd-header-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='even-header']" mode= "header">
        <xsl:if test= "$layout-page = 'odd-even' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="even-header">
                <fo:block role="header" xsl:use-attribute-sets="even-header-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='odd-footer']" mode= "footer">
        <xsl:if test= "$layout-page = 'odd-even' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="odd-footer">
                <fo:block role="footer" xsl:use-attribute-sets="odd-footer-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='even-footer']" mode= "footer">
        <xsl:if test= "$layout-page = 'odd-even' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="even-footer">
                <fo:block role="footer" xsl:use-attribute-sets="even-footer-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='body-header']" mode= "header">
        <xsl:if test= "$layout-page = 'first' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="body-header">
                <fo:block role="header" xsl:use-attribute-sets="body-header-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='body-footer']" mode= "footer">
        <xsl:if test= "$layout-page = 'first' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="body-footer">
                <fo:block role="footer" xsl:use-attribute-sets="body-footer-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='toc-first-header']" mode= "header">
        <xsl:if test= "$layout-page = 'first' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="toc-first-header">
                <fo:block role="header" xsl:use-attribute-sets="toc-first-header-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='toc-first-footer']" mode= "footer">
        <xsl:if test= "$layout-page = 'first' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="toc-first-footer">
                <fo:block role="footer" xsl:use-attribute-sets="toc-first-footer-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='toc-odd-header']" mode= "header">
        <xsl:if test= "$layout-page = 'odd-even' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="toc-odd-header">
                <fo:block role="header" xsl:use-attribute-sets="toc-odd-header-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='toc-even-header']" mode= "header">
        <xsl:if test= "$layout-page = 'odd-even' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="toc-even-header">
                <fo:block role="header" xsl:use-attribute-sets="toc-even-header-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='toc-odd-footer']" mode= "footer">
        <xsl:if test= "$layout-page = 'odd-even' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="toc-odd-footer">
                <fo:block role="footer" xsl:use-attribute-sets="toc-odd-footer-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='toc-even-footer']" mode= "footer">
        <xsl:if test= "$layout-page = 'odd-even' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="toc-even-footer">
                <fo:block role="footer" xsl:use-attribute-sets="toc-even-footer-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='toc-body-header']" mode= "header">
        <xsl:if test= "$layout-page = 'first' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="toc-body-header">
                <fo:block role="header" xsl:use-attribute-sets="toc-body-header-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>

    <xsl:template match="container[@classes='toc-body-footer']" mode= "footer">
        <xsl:if test= "$layout-page = 'first' or $layout-page = 'first-odd-even'">
            <fo:static-content flow-name="toc-body-footer">
                <fo:block role="footer" xsl:use-attribute-sets="toc-body-footer-block">
                    <xsl:apply-templates/>
                </fo:block>
            </fo:static-content>
        </xsl:if>
    </xsl:template>


    <xsl:template match="decoration/header" mode="header">
        <xsl:choose>
            <xsl:when test="$layout-page = '' or $layout-page = 'simple'">
                <fo:static-content flow-name="simple-header">
                    <fo:block role="header" xsl:use-attribute-sets="header-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$layout-page = 'odd-even'">
                <fo:static-content flow-name="odd-header">
                    <fo:block role="header" xsl:use-attribute-sets="header-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
                <fo:static-content flow-name="even-header">
                    <fo:block role="header" xsl:use-attribute-sets="header-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$layout-page = 'first'">
                <xsl:if test="$suppress-first-page-header != 'True'">
                    <fo:static-content flow-name = "first-header">
                        <fo:block role="header" xsl:use-attribute-sets="header-block">
                            <xsl:apply-templates/>
                        </fo:block>
                    </fo:static-content>
                </xsl:if>
                <fo:static-content flow-name = "body-header">
                    <fo:block role="header" xsl:use-attribute-sets="header-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$layout-page = 'first-odd-even'">
                <xsl:if test="$suppress-first-page-header != 'True'">
                    <fo:static-content flow-name = "first-header">
                        <fo:block role="header" xsl:use-attribute-sets="header-block">
                            <xsl:apply-templates/>
                        </fo:block>
                    </fo:static-content>
                </xsl:if>
                <fo:static-content flow-name = "odd-header">
                    <fo:block role="header" xsl:use-attribute-sets="header-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
                <fo:static-content flow-name = "even-header">
                    <fo:block role="header" xsl:use-attribute-sets="header-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="decoration/footer" mode="footer">
        <xsl:choose>
            <xsl:when test="$layout-page = '' or $layout-page = 'simple'">
                <fo:static-content flow-name="simple-footer">
                    <fo:block role="footer" xsl:use-attribute-sets="footer-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$layout-page = 'odd-even'">
                <fo:static-content flow-name="odd-footer">
                    <fo:block role="footer" xsl:use-attribute-sets="footer-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
                <fo:static-content flow-name="even-footer">
                    <fo:block role="footer" xsl:use-attribute-sets="footer-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$layout-page = 'first'">
                <xsl:if test="$suppress-first-page-footer != 'True'">
                    <fo:static-content flow-name = "first-footer">
                        <fo:block role="footer" xsl:use-attribute-sets="footer-block">
                            <xsl:apply-templates/>
                        </fo:block>
                    </fo:static-content>
                </xsl:if>
                <fo:static-content flow-name = "body-footer">
                    <fo:block role="footer" xsl:use-attribute-sets="footer-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
            </xsl:when>
            <xsl:when test="$layout-page = 'first-odd-even'">
                <xsl:if test="$suppress-first-page-footer != 'True'">
                    <fo:static-content flow-name = "first-footer">
                        <fo:block role="footer" xsl:use-attribute-sets="footer-block">
                            <xsl:apply-templates/>
                        </fo:block>
                    </fo:static-content>
                </xsl:if>
                <fo:static-content flow-name = "odd-footer">
                    <fo:block role="footer" xsl:use-attribute-sets="footer-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
                <fo:static-content flow-name = "even-footer">
                    <fo:block role="footer" xsl:use-attribute-sets="footer-block">
                        <xsl:apply-templates/>
                    </fo:block>
                </fo:static-content>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="inline[@classes='page-num']">
        <fo:page-number/>
    </xsl:template>

    <xsl:template match="decoration/header/paragraph">
        <fo:block xsl:use-attribute-sets="paragraph-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/footer/paragraph">
        <fo:block xsl:use-attribute-sets="paragraph-footer-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="container[@classes='first-header']/paragraph| 
        container[@classes='odd-header']/paragraph|
        container[@classes='even-header']/paragraph|
        container[@classes='body-header']/paragraph|
        container[@classes='toc-first-header']/paragraph|
        container[@classes='toc-odd-header']/paragraph|
        container[@classes='toc-even-header']/paragraph|
        container[@classes='toc-body-header']/paragraph" priority="3">

        <fo:block role = "header-paragraph" xsl:use-attribute-sets="paragraph-header-block">
            <xsl:apply-templates/>
        </fo:block>
        
    </xsl:template>

    <xsl:template match="container[@classes='first-footer']/paragraph|
        container[@classes='odd-footer']/paragraph|
        container[@classes='even-footer']/paragraph|
        container[@classes='body-footer']/paragraph|
        container[@classes='toc-first-footer']/paragraph|
        container[@classes='toc-odd-footer']/paragraph|
        container[@classes='toc-even-footer']/paragraph|
        container[@classes='toc-body-footer']/paragraph" priority="3">

        <fo:block role="footer-paragraph" xsl:use-attribute-sets="paragraph-footer-block">
            <xsl:apply-templates/>
        </fo:block>
        
    </xsl:template>

    <!--
    <xsl:template match="decoration/header/paragraph[2]">
        <fo:block xsl:use-attribute-sets="header-second-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/header/paragraph[3]">
        <fo:block xsl:use-attribute-sets="header-third-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/footer/paragraph[1]">
        <fo:block xsl:use-attribute-sets="footer-first-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/footer/paragraph[2]">
        <fo:block xsl:use-attribute-sets="footer-second-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="decoration/footer/paragraph[3]">
        <fo:block xsl:use-attribute-sets="footer-third-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>
    -->

    <xsl:template match="decoration/header|decoration/footer"/>

</xsl:stylesheet>
