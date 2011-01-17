<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <!--
    BEGIN LONG TEMPLATE
====================================================================================================
    -->
    <xsl:template name="generate-section-num">
        <xsl:variable name="level">
            <xsl:value-of select="count(ancestor::section)"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$level=1">
                <xsl:for-each select="parent::section">
                    <xsl:number format="{$number-section1}"/>
                </xsl:for-each>
            </xsl:when>
            <xsl:when test="$level = 2">
                <xsl:if test="$inherit-section-num = 'True'">
                    <xsl:for-each select="parent::section/parent::section">
                        <xsl:number format="{$number-section1}"/>
                    </xsl:for-each>
                </xsl:if>
                <xsl:for-each select="parent::section">
                    <xsl:number format="{$number-section2}"/>
                </xsl:for-each>
            </xsl:when>
            <xsl:when test="$level = 3">
                <xsl:if test="$inherit-section-num = 'True'">
                    <xsl:for-each select="parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section1}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section">
                        <xsl:number format="{$number-section2}"/>
                    </xsl:for-each>
                </xsl:if>
                <xsl:for-each select="parent::section">
                    <xsl:number format="{$number-section3}"/>
                </xsl:for-each>
            </xsl:when>
            <xsl:when test="$level = 4">
                <xsl:if test="$inherit-section-num = 'True'">
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section1}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section2}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section">
                        <xsl:number format="{$number-section3}"/>
                    </xsl:for-each>
                </xsl:if>
                <xsl:for-each select="parent::section">
                    <xsl:number format="{$number-section4}"/>
                </xsl:for-each>
            </xsl:when>
            <xsl:when test="$level = 5">
                <xsl:if test="$inherit-section-num = 'True'">
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section1}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section2}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section3}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section">
                        <xsl:number format="{$number-section4}"/>
                    </xsl:for-each>
                </xsl:if>
                <xsl:for-each select="parent::section">
                    <xsl:number format="{$number-section5}"/>
                </xsl:for-each>
            </xsl:when>
            <xsl:when test="$level = 6">
                <xsl:if test="$inherit-section-num = 'True'">
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section1}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section2}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section3}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section4}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section">
                        <xsl:number format="{$number-section5}"/>
                    </xsl:for-each>
                </xsl:if>
                <xsl:for-each select="parent::section">
                    <xsl:number format="{$number-section6}"/>
                </xsl:for-each>
            </xsl:when>
            <xsl:when test="$level = 7">
                <xsl:if test="$inherit-section-num = 'True'">
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section1}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section2}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section3}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section4}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::sectioin/parent::section/parent::section">
                        <xsl:number format="{$number-section5}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section">
                        <xsl:number format="{$number-section6}"/>
                    </xsl:for-each>
                </xsl:if>
                <xsl:for-each select="parent::section">
                    <xsl:number format="{$number-section7}"/>
                </xsl:for-each>
            </xsl:when>
            <xsl:when test="$level = 8">
                <xsl:if test="$inherit-section-num = 'True'">
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section1}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section2}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section3}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section4}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::sectioin/parent::section/parent::section">
                        <xsl:number format="{$number-section5}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section6}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section">
                        <xsl:number format="{$number-section7}"/>
                    </xsl:for-each>
                </xsl:if>
                <xsl:for-each select="parent::section">
                    <xsl:number format="{$number-section8}"/>
                </xsl:for-each>
            </xsl:when>
            <xsl:when test="$level = 9">
                <xsl:if test="$inherit-section-num = 'True'">
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section1}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section2}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section3}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section4}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::sectioin/parent::section/parent::section">
                        <xsl:number format="{$number-section5}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section6}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section/parent::section">
                        <xsl:number format="{$number-section7}"/>
                    </xsl:for-each>
                    <xsl:for-each select="parent::section/parent::section">
                        <xsl:number format="{$number-section8}"/>
                    </xsl:for-each>
                </xsl:if>
                <xsl:for-each select="parent::section">
                    <xsl:number format="{$number-section9}"/>
                </xsl:for-each>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <!--
    END LONG TEMPLATE
====================================================================================================
    -->

    <!--determine page sequence, to be used in other templates-->
    <xsl:variable name="page-sequence-type">
        <xsl:variable name="toc-exists">
            <xsl:if test="/document/topic[@classes='contents']">True</xsl:if>
        </xsl:variable>
        <xsl:variable name="dedication-exists">
            <xsl:if test="/document/topic[@classes='dedication']">True</xsl:if>
        </xsl:variable>
        <xsl:variable name="abstract-exists">
            <xsl:if test="/document/topic[@classes='dedication']">True</xsl:if>
        </xsl:variable>
        <xsl:variable name="need-toc-page-sequence">
            <xsl:choose>
                <xsl:when test="$toc-exists='True' and $toc-pagination = 'own-section'">True</xsl:when>
                <xsl:otherwise>False</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="need-front-matter-page-sequence">
            <xsl:choose>
                <xsl:when test="($dedication-exists='True' or $abstract-exists='True') and $front-matter-pagination = 'own-section'">True</xsl:when>
                <xsl:when test="($dedication-exists='True' or $abstract-exists='True') and $front-matter-pagination = 'with-toc'">True</xsl:when>
                <xsl:otherwise>False</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$need-toc-page-sequence = 'True' and $front-matter-pagination = 'with-toc'">
                <xsl:text>toc-combined-body</xsl:text>
            </xsl:when>
            <xsl:when test="$need-toc-page-sequence = 'True' and $need-front-matter-page-sequence = 'True'">
                <xsl:text>front-toc-body</xsl:text>
            </xsl:when>
            <xsl:when test="$need-toc-page-sequence = 'True' and $need-front-matter-page-sequence = 'False'">
                <xsl:text>toc-body</xsl:text>
            </xsl:when>
            <xsl:when test="$need-toc-page-sequence = 'False' and $need-front-matter-page-sequence = 'True'">
                <xsl:text>front-body</xsl:text>
            </xsl:when>
            <xsl:when test="$need-toc-page-sequence = 'False' and $need-front-matter-page-sequence = 'False'">
                <xsl:text>body</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message terminate = "yes">
                    <xsl:text>Stylsheet error: no page sequence found</xsl:text>
                </xsl:message>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>

    
</xsl:stylesheet> 
