<?xml version="1.0" encoding="utf-8"?>
<!--
    Author: David Priest
    Date: 2003-Jun-13
    Purpose: DocUtils XML -> DocUtils FO

    pdf.xsl is the root transformation for doc-utils
    transformed documentation.

    Here, we just set some saxon-specific and fop-specific variables that
    support formal publishing-style output.
 -->

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:fox="http://xml.apache.org/fop/extensions"
    xmlns:saxon="http://icl.com/saxon"
    xmlns:u="U"> <!-- for local utility functions -->

<!-- TOGGLES -->
    <xsl:variable name="publish_comments">not-enabled</xsl:variable>
    <xsl:variable name="publish_filenotes">not-enabled</xsl:variable> <!-- comments starting with "file notes" -->
    <xsl:variable name="publish_problematics">not-enabled</xsl:variable>
    <xsl:variable name="system_messages">not-enabled</xsl:variable>

<xsl:include href="./pdf.xsl" />

</xsl:stylesheet>
