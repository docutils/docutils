<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

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
    
</xsl:stylesheet> 
