
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id: lists.xsl 6552 2011-01-13 05:56:07Z paultremblay $ -->
<!--if set to True, stylesheets cause quit when error found-->
<xsl:param name="strict"/>

<!--wheter to start new page when coming across sections one level deep
Not implemented yet-->
<xsl:param name="create-chapters"/>

<!--layout is either 
simple (all pages the same)

first (different first page from rest)

odd-even (different for odd and even; headers and footers will be the same because of the limitations of rst)

first-odd-even (different first, odd, and even; headers and footers will be the same, with the option of suppressing the first header and footer)

-->
<xsl:param name="page-layout">simple</xsl:param>


<!--supress the header on the first page--> 
<xsl:param name="suppress-first-page-header"/>

<!--supress the footer on the first page--> 
<xsl:param name="suppress-first-page-footer"/>

<!--these are unfortunate parameters; they should be set to exactly the value of extent in 
the page-header or page-footer attribute sets-->
<xsl:param name="spacing-header"/>
<xsl:param name="spacing-footer"/>

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

<!--Parameters for docinfo fields. These parameters are used to fill in the text before such
fields as author, etc.`docutils`-->
<xsl:param name="author-text">Author: </xsl:param>
<xsl:param name="authors-text">Authors: </xsl:param>
<xsl:param name="organization-text">Organization: </xsl:param>
<xsl:param name="contact-text">Contact: </xsl:param>
<xsl:param name="status-text">Status: </xsl:param>
<xsl:param name="copyright-text">Copyright: </xsl:param>
<xsl:param name="address-text">Address: </xsl:param>
<xsl:param name="version-text">Version: </xsl:param>
<xsl:param name="date-text">Date: </xsl:param>

<!--whether to make new pages for dedication and abstact-->
<!--either own-section, with-toc, with-body-->
<xsl:param name="front-matter-pagination">own-section</xsl:param>
<!--either own-section, or with-body-->
<xsl:param name="toc-pagination">own-section</xsl:param>
<!--Note: utils.xsl has the code for the variable page-sequence-type-->

<!--the text that separates options in the options_list,
for example, -f  -file. With "," as the value, thise 
arguments become -f, -file `doc`-->
<xsl:param name="options-separator">, </xsl:param>

<!--how to format the options list. Acceptable values are
'list' and 'definition'. The 'list' values creates a bullet-type
list, with the options taking the place of bullets. The 'definiton' 
value creates a definition type list, with the options on a separate
block above the description `doc`-->
<xsl:param name="option-list-format">list</xsl:param>
    
</xsl:stylesheet>
