
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id: lists.xsl 6552 2011-01-13 05:56:07Z paultremblay $ -->
<!--if set to True, stylesheets cause quit when error found-->
<xsl:param name="strict"/>


<!--layout is either 
simple (all pages the same)

first (different first page from rest)

odd-even (different for odd and even; headers and footers will be the same because of the limitations of rst)

first-odd-even (different first, odd, and even; headers and footers will be the same, with the option of suppressing the first header and footer)
-->
<xsl:param name="page-layout"></xsl:param>

<!--supress the header on the first page--> 
<xsl:param name="suppress-first-page-header"/>

<!--supress the footer on the first page--> 
<xsl:param name="suppress-first-page-footer"/>

<!--these are unfortunate parameters; they should be set to exactly the value of extent in 
the page-header or page-footer attribute sets-->
<xsl:param name="spacing-header"/>
<xsl:param name="spacing-footer"/>
<xsl:param name="custom-spacing-header-footer"/>


<!--with-front, with-toc, or with-body-->
<xsl:param name="title-pagination">with-front</xsl:param>

<!--with-front, with-toc, or with-body-->
<xsl:param name="bibliographic-pagination">with-front</xsl:param>

<!--with-front, with-toc, or with-body-->
<xsl:param name="dedication-pagination">with-front</xsl:param>

<!--with-front, with-toc, or with-body-->
<xsl:param name="abstract-pagination">with-front</xsl:param>
<!--either own-section, or with-body-->

<!--with-front, with-toc, or with-body-->
<xsl:param name="toc-pagination">with-toc</xsl:param>
<!--Note: utils.xsl has the code for the variable page-sequence-type-->

<!--the order for the front matter. Use a comma after each area, and 
make sure you incude all areas. Relevant areas are 
title, bibliographic, dedication, abstract, and toc
-->
<xsl:param name="front-order">title,bibliographic,dedication,abstract,toc</xsl:param>

<xsl:param name="bibliographic-format">list</xsl:param>


<!--cols for custom table, when needed-->
<xsl:param name="table1-cols"></xsl:param>
<xsl:param name="table2-cols"></xsl:param>
<xsl:param name="table3-cols"></xsl:param>
<xsl:param name="table4-cols"></xsl:param>
<xsl:param name="table5-cols"></xsl:param>
<xsl:param name="table6-cols"></xsl:param>
<xsl:param name="table7-cols"></xsl:param>
<xsl:param name="table8-cols"></xsl:param>
<xsl:param name="table9-cols"></xsl:param>
<xsl:param name="table10-cols"></xsl:param>
<xsl:param name="table11-cols"></xsl:param>
<xsl:param name="table12-cols"></xsl:param>
<xsl:param name="table13-cols"></xsl:param>
<xsl:param name="table14-cols"></xsl:param>
<xsl:param name="table15-cols"></xsl:param>
<xsl:param name="table16-cols"></xsl:param>
<xsl:param name="table17-cols"></xsl:param>
<xsl:param name="table18-cols"></xsl:param>
<xsl:param name="table19-cols"></xsl:param>
<xsl:param name="table20-cols"></xsl:param>
<xsl:param name="table21-cols"></xsl:param>
<xsl:param name="table22-cols"></xsl:param>
<xsl:param name="table23-cols"></xsl:param>
<xsl:param name="table24-cols"></xsl:param>
<xsl:param name="table25-cols"></xsl:param>
<xsl:param name="table26-cols"></xsl:param>
<xsl:param name="table27-cols"></xsl:param>
<xsl:param name="table28-cols"></xsl:param>
<xsl:param name="table29-cols"></xsl:param>
<xsl:param name="table30-cols"></xsl:param>
<xsl:param name="table-cols"></xsl:param>
<xsl:param name="table-borderless-cols"></xsl:param>
<xsl:param name="table-long-cols"></xsl:param>

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

<!--cutom bibliographic entry names-->
<xsl:param name="custom-bib-info1-name"></xsl:param>
<xsl:param name="custom-bib-info2-name"></xsl:param>
<xsl:param name="custom-bib-info3-name"></xsl:param>
<xsl:param name="custom-bib-info4-name"></xsl:param>
<xsl:param name="custom-bib-info5-name"></xsl:param>
<xsl:param name="custom-bib-info6-name"></xsl:param>
<xsl:param name="custom-bib-info7-name"></xsl:param>
<xsl:param name="custom-bib-info8-name"></xsl:param>
<xsl:param name="custom-bib-info9-name"></xsl:param>
<xsl:param name="custom-bib-info10-name"></xsl:param>

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

<!--how to format the options list. Acceptable values are
'list' and 'definition'. The 'list' values creates a bullet-type
list, with the options taking the place of bullets. The 'definiton' 
value creates a definition type list, with the options on a separate
block above the description `doc`-->
<xsl:param name="option-list-format">list</xsl:param>

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

<!--where to place footnote. Values are 'footnote' and
'endnote'. When 'footnote is choosen, footnotes are placed
at the bottom of each page. When 'endnote' is choosen, footnotes
are placed in the same place as in the RST document-->
<xsl:param name="footnote-placement">footnote</xsl:param>

<!--the style of the footnote. Valid values are 'list', for making 
the footnote into a list, with the number as the item; 'traditional', 
to make the footnote more traditional with a first line indent;-->
<xsl:param name="footnote-style">list</xsl:param>


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
