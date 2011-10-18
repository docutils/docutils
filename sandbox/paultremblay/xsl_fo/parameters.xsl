
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id: lists.xsl 6552 2011-01-13 05:56:07Z paultremblay $ -->
<!--if set to True, stylesheets cause quit when error found-->
<xsl:param name="strict"/>


<xsl:param name="long-rows-first-page"></xsl:param>

<!--Parameters for docinfo fields. These parameters are used to fill in the text before such
fields as author, etc.-->
<xsl:param name="author-text">Author: </xsl:param>
<xsl:param name="authors-text">Authors: </xsl:param>
<xsl:param name="organization-text">Organization: </xsl:param>
<xsl:param name="contact-text">Contact: </xsl:param>
<xsl:param name="status-text">Status: </xsl:param>
<xsl:param name="copyright-text">Copyright: </xsl:param>
<xsl:param name="address-text">Address: </xsl:param>
<xsl:param name="version-text">Version: </xsl:param>
<xsl:param name="revision-text">Revision: </xsl:param>
<xsl:param name="date-text">Date: </xsl:param>


<!--text for  admontions-->
<xsl:param name="attention-title">Attention!</xsl:param>
<xsl:param name="caution-title">Caution!</xsl:param>
<xsl:param name="danger-title">!Danger!</xsl:param>
<xsl:param name="error-title">Error</xsl:param>
<xsl:param name="hint-title">Hint</xsl:param>
<xsl:param name="important-title">Important</xsl:param>
<xsl:param name="note-title">Note</xsl:param>
<xsl:param name="tip-title">Tip</xsl:param>
<xsl:param name="warning-title">Warning!</xsl:param>

<!--text to use for transition elements-->
<xsl:param name="transition-text">***</xsl:param>

<!--the format for the level 1 sectin title. If left blank, no number will be generated-->
<xsl:param name="number-section1">1</xsl:param>
<xsl:param name="number-section2">.1</xsl:param>
<xsl:param name="number-section3">.1</xsl:param>
<xsl:param name="number-section4">.1</xsl:param>
<xsl:param name="number-section5">.1</xsl:param>
<xsl:param name="number-section6">.1</xsl:param>
<xsl:param name="number-section7">.1</xsl:param>
<xsl:param name="number-section8">.1</xsl:param>
<xsl:param name="number-section9">.1</xsl:param>

<!--inherit section numbers from previous sections, such as 1.1.1-->
<xsl:param name="inherit-section-num">True</xsl:param>

<!--the default to use for a bullet list, rather than what the user specifies-->
<xsl:param name="bullet-text">&#x2022;</xsl:param>

<!--the default to use for a bullet list level 2, rather than what the user specifies-->
<!--
<xsl:param name="bullet-text-level2">&#x25E6;</xsl:param>
-->

<xsl:param name="bullet-text-level2">&#x00B0;</xsl:param>

<!--the text that separates options in the options_list,
for example, -f  -file. With "," as the value, these 
arguments become -f, -file -->
<xsl:param name="options-separator">, </xsl:param>


<!--whether to number verse. Value is either '', or
some integer, a multiple of the lines you want to number.
For example, '5' will number every fifth line-->
<xsl:param name="number-verse"></xsl:param>

<!--text to use before attribution in block quote-->
<xsl:param name="text-before-block-quote-attribution">&#x2014;</xsl:param>
<xsl:param name="text-before-epigraph-attribution">&#x2014;</xsl:param>
<xsl:param name="text-before-pull-quote-attribution">&#x2014;</xsl:param>

<!--where to place table title valid options are 'top' or 'bottom'-->
<xsl:param name="table-title-placement">bottom</xsl:param>


<!--This is a hack to fix the problem with fop ?? not
being able to space between footnotes. Instead, a blank
block is written with a no-break space character, and
the height of this block is determined by a font-size, 
here the space-between-footnotes-->
<xsl:param name="space-between-footnotes">5pt</xsl:param>

    
<!--whether to make internal links clickable ('link'), 
refer to a page number ('page'), or both ('page-link')-->
<xsl:param name="internal-link-type">link</xsl:param>

<!--only for testing purposes-->
<xsl:param name="test"/>

</xsl:stylesheet>
