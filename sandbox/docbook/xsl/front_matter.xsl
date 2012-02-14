<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template name="make-info">
        <d:info>
            <d:title>
                <xsl:if test="not(docinfo) and not(title)">
                    <xsl:value-of select="@source"/>
                </xsl:if>
                <xsl:apply-templates select="title" mode="with-info"/>
            </d:title>
            <xsl:apply-templates select="subtitle" mode="with-info"/>
            <xsl:apply-templates select="docinfo" mode="with-info"/>
            <xsl:apply-templates select="/document/topic[@classes='abstract']" mode="with-info"/>
        </d:info>
    </xsl:template>

    <xsl:template match="/document/title|/document/docinfo|document/docinfo/organization|contact|docinfo/address|
        docinfo/status|docinfo/date|docinfo/version|document/subtitle"/>

    <xsl:template match="/document/title" mode="with-info">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="/document/subtitle" mode="with-info">
        <d:subtitle>
            <xsl:apply-templates/>
        </d:subtitle>
    </xsl:template>

    <xsl:template match="document/docinfo" mode="with-info">
        <xsl:call-template name="authors"/>
        <xsl:call-template name="make-revhistory"/>
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template name="authors">
        <xsl:choose>
            <xsl:when test="count(author) &gt; 1">
                <d:authorgroup>
                    <xsl:apply-templates select="author" mode="author"/>
                </d:authorgroup>
            </xsl:when>
            <xsl:when test="count(author) = 1">
                <xsl:apply-templates select="author"/>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="author"/>

    <xsl:template match="author" mode="author">
        <d:author>
            <d:personname>
                <xsl:apply-templates/>
            </d:personname>
            <xsl:call-template name="organization"/>
            <xsl:call-template name="contact"/>
            <xsl:call-template name="address"/>
        </d:author>
    </xsl:template>

    <xsl:template name="organization">
        <xsl:variable name="name-pos">
            <xsl:for-each select="following-sibling::author[1]">
                <xsl:value-of select="count(preceding-sibling::*)"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="org-pos">
            <xsl:for-each select="following-sibling::organization[1]">
                <xsl:value-of select="count(preceding-sibling::*)"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:if test="$name-pos = '' or $org-pos &lt; $name-pos">
            <xsl:apply-templates select="following-sibling::organization[1]" mode="with-author"/>
        </xsl:if>
    </xsl:template>

    <xsl:template name="contact">
        <xsl:variable name="name-pos">
            <xsl:for-each select="following-sibling::author[1]">
                <xsl:value-of select="count(preceding-sibling::*)"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="contact-pos">
            <xsl:for-each select="following-sibling::contact[1]">
                <xsl:value-of select="count(preceding-sibling::*)"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:if test="$name-pos = '' or $contact-pos &lt; $name-pos">
            <xsl:apply-templates select="following-sibling::contact[1]" mode="with-author"/>
        </xsl:if>
    </xsl:template>

    <xsl:template name="address">
        <xsl:variable name="name-pos">
            <xsl:for-each select="following-sibling::author[1]">
                <xsl:value-of select="count(preceding-sibling::*)"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="address-pos">
            <xsl:for-each select="following-sibling::address[1]">
                <xsl:value-of select="count(preceding-sibling::*)"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:if test="$name-pos = '' or $address-pos &lt; $name-pos">
            <xsl:apply-templates select="following-sibling::address[1]" mode="with-author"/>
        </xsl:if>
    </xsl:template>

    <xsl:template match="docinfo/authors">
        <xsl:call-template name="error-message-generic">
            <xsl:with-param name="quit">true</xsl:with-param>
            <xsl:with-param name="msg">
                <xsl:text>Don't use authors: use multiple author instead</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="contact" mode="with-author">
        <d:email>
            <xsl:apply-templates/>
        </d:email>
    </xsl:template>

    <xsl:template match="address" mode="with-author">
        <d:address>
            <xsl:apply-templates/>
        </d:address>
    </xsl:template>

    <xsl:template match="contact/reference">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="organization" mode="with-author">
        <d:affiliation>
            <d:orgname>
                <xsl:apply-templates/>
            </d:orgname>
        </d:affiliation>
    </xsl:template>

    <xsl:template match="docinfo/field">
        <d:annotation role="docinfo">
            <xsl:apply-templates/>
        </d:annotation>
    </xsl:template>

    <xsl:template match="docinfo/field/field_name">
        <d:title>
            <xsl:apply-templates/>
        </d:title>
    </xsl:template>

    <xsl:template match="docinfo/field/field_body">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="docinfo/status_">
        <d:revhistory>
            <d:revision>
                <d:date>jun</d:date>
                <d:revremark>
                    <xsl:apply-templates/>
                </d:revremark>
            </d:revision>
        </d:revhistory>
    </xsl:template>

    <xsl:template name="make-revhistory">
        <xsl:choose>
            <xsl:when test="docinfo/date and not(docinfo/status) and not(docinfo/version)">
                <d:date>
                    <xsl:value-of select="docinfo/date"/>
                </d:date>
            </xsl:when>
            <xsl:when test="docinfo/status or docinfo/version">
                <d:revhistory>
                    <d:revision>
                        <xsl:if test="docinfo/revision">
                            <d:revnumber>
                                <xsl:value-of select="docinfo/revision"/>
                            </d:revnumber>
                        </xsl:if>
                        <d:date>
                            <xsl:call-template name="get-date">
                                <xsl:with-param name="date" select="docinfo/date"/>
                            </xsl:call-template>
                        </d:date>
                        <xsl:if test="docinfo/status">
                            <d:revremark>
                                <xsl:apply-templates select="docinfo/status" mode="with-revhistory"/>
                            </d:revremark>
                        </xsl:if>
                    </d:revision>
                </d:revhistory>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="status" mode="with-revhistory">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="copyright">
        <d:legalnotice>
            <d:para>
                <xsl:apply-templates/>
            </d:para>
        </d:legalnotice>
    </xsl:template>

    <xsl:template match="/document/topic[@classes='dedication']">
        <xsl:choose>
            <xsl:when test="$docbook-type = 'article'">
                <xsl:call-template name="error-message-generic">
                    <xsl:with-param name="msg">
                        <xsl:text>Cannot process dediction with dobook-type "article"</xsl:text>
                    </xsl:with-param>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <d:dedication>
                    <xsl:apply-templates />
                </d:dedication>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!--don't include title for dedication??-->
    <xsl:template match="/document/topic[@classes='dedication']/title|/document/topic[@classes='abstract']/title"/>

    <xsl:template match="/document/topic[@classes='abstract']"/>

    <xsl:template match="/document/topic[@classes='abstract']" mode="with-info">
        <d:abstract>
            <xsl:apply-templates/>
        </d:abstract>
    </xsl:template>

    <xsl:template match="/document/topic[@classes='contents']">
        <xsl:call-template name="error-message-generic">
            <xsl:with-param name="msg">
                <xsl:text>Ignoring restructed text TOC for docbook&#x0A;</xsl:text>
                <xsl:text>(TOC generated by docbook stylesheets)</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="/document/decoration">
        <xsl:call-template name="error-message-generic">
            <xsl:with-param name="msg">
                <xsl:text>Ignoring headers or footers docbook&#x0A;</xsl:text>
                <xsl:text>(customize docbook stylesheets instead)</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>


</xsl:stylesheet>
