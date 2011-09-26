<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:include href="comment.xsl"/>
    <xsl:include href="utils.xsl"/>

    <xsl:output method="xml"/>

    <xsl:template match="xsl:stylesheet">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="/">
        <doc>
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">1</xsl:with-param>
                <xsl:with-param name="text">XSL-FO Documentation</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="make-title">
                <xsl:with-param name="level">2</xsl:with-param>
                <xsl:with-param name="text">Parameters</xsl:with-param>
            </xsl:call-template>
            <block>.. contents:: Table of Contents</block>
            <xsl:apply-templates/>
        </doc>
    </xsl:template>

    <xsl:template match="xsl:param[@name='page-layout']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>simple, first, odd-even, first-odd-even</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            This parameter determines the page layout for the document. A value of
            ``simple`` will create a document with the same page layout for all 
            pages. A value of ``first`` creates a document with a dfferent page
            layout for the first page and for the rest of the pages. A value of
            ``odd-even`` creates a different layout for for odd and even pages. A value
            of ``first-odd-even`` creates a different layout for the first page, for
            odd pages, and for even pages.
        </block>
        <block>
            Because restructured text only allows one footer and header, the footer and header
            will be the same for bth odd and even pages. However, if the ``first`` or
            ``first-odd-even`` values is chosen, you can suppress the first footer and 
            header (see below).
        </block>
        <block>
            Using a value other than ``simple`` allows for different margins for different
            page sequences, depending on the value.
        </block>
    </xsl:template>

    <!--not used anymore-->
    <xsl:template match="xsl:param[@name='suppress-first-page-header']" priority = "3"/>
    <xsl:template match="xsl:param[@name='suppress-first-page-header_old']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>True, False, ''</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            If set to True, and the ``page-layout`` is set to``first``, or ``page-layout``
            is set to ``first-odd-even``, no header will appear on the first page. If a value
            of ``simple`` or ``odd-even`` is chosen for the ``page-layout``, this parameter will
            have no effect, and the header will appear on all pages.
        </block>
    </xsl:template>

    <!--not used anymore-->
    <xsl:template match="xsl:param[@name='suppress-first-page-footer']" priority = "3"/>
    <xsl:template match="xsl:param[@name='suppress-first-page-footer_old']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>True, False, ''</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            If set to True, and the ``page-layout`` is set to ``first``, or ``page-layout``
            is set to ``first-odd-even``, no footer will appear on the first page. If a value
            of ``simple`` or ``odd-even`` is chosen for the ``page-layout``, this parameter will
            have no effect, and the footer will appear on all pages.
        </block>
    </xsl:template>


    <!--not used anymore-->
    <xsl:template match="xsl:param[@name='spacing-header']" priority = "3"/>
    <xsl:template match="xsl:param[@name='spacing-header_old']" priority = "3">
        <xsl:call-template name="make-name">
            <xsl:with-param name="name">spacing-header and spacing-footer</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="possible-values">
            <xsl:with-param name="text" select="'Any Measure'"/>
        </xsl:call-template>
        <xsl:call-template name="p-defaults"/>
        <block>
            The parameters ``spacing-header`` and ``spacing-footer`` create the space for the 
            header and footer. Although the default is set to an empty string, the XSL
            styelsheets will create a satisfactory space if a header or footer is found. Use
            either of these parameters to change that default.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='spacing-footer']" priority = "3"/>

    <xsl:template match="xsl:param[@name='title-pagination']" priority = "3">
        <xsl:call-template name="make-name">
            <xsl:with-param name="name">pagination for front matter</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="possible-values">
            <xsl:with-param name="text" select="'with-front, with-toc, with-body'"/>
        </xsl:call-template>
        <block>**Defaults:** See below</block>
        <block>
            The function is the same for the following parameters:
        </block>
        <block>* title-pagination</block>
        <block>* bibliographic-pagination</block>
        <block>* dedication-pagination</block>
        <block>* abstract-pagination</block>
        <block>* toc-pagination</block>
        <block>
            Each determines what region to place the textual matter. There are 
            three regions, the front matter, the toc matter, and the body matter. The front 
            matter has no footers and headers. The toc matter starts a new page run, in which 
            the numbers start with 1 (or any other value), and can take any formatting. The 
            body matter again starts a new run of pages with its own page numbering and formatting
            of these numbers.
        </block>
        <block>
            In practice, the abstract and title page often occurr before the other front matter 
            material, and they appear on pages with no footers and headers. The dedication and Table
            of Contents appear next, with the first numbering of the document, the numbers being
            formatted as lower-case Roman numberals. The bibliographic information could appear 
            in either the front matter or toc matter. In order to achieve this standard layout,
            the defaults choose a ``with-front`` for the ``title-pagination``, 
            ``abstract-pagination.``, and ``bibliographic-pagination; and a 
            ``with-toc`` for the ``toc-pagination`` and ``dedication-pagination``.
        </block>
        <block>
            In order to change these defaults, choose a different value. For example, 
            to place the dedication in the front matter, set ``dedication-pagination`` to
            ``with-front``. For a simple document, in which there is only one set of page runs, 
            simply set each of these parameters to ``with-body``.
            
        </block>
    </xsl:template>


    <xsl:template match="xsl:param[@name='bibliographic-pagination']|
            xsl:param[@name='dedication-pagination']| xsl:param[@name='abstract-pagination']|
                xsl:param[@name='toc-pagination'] " priority = "3"/>

    <xsl:template match="xsl:param[@name='front-order']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>title,bibliographic,dedication,abstract,toc</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            The param ``front-order`` is a string of each region, separated by a comma, 
            that determines the order of the title, the bibliographic
            information, the dedication, the abstract, and the Table of Contents. The 
            default puts them in order that docutils puts them in when the document is
            converted to XML. In order to change this, change the order in the string.
            For example, to place the abstract before the dedication, use
            ``'title,bibliographic,dedication,abstract,toc'`` as a value.
        </block>
        <block>
            If you have a region in your parameter value that does not actually exist 
            in your document, no error will occurr. For example, if you set your value
            to ``title,bibliographic,dedication,abstract,toc``, but have no ``title`` in 
            your document, the XSL stylesheet will still place the abstract before the dedication
            without raising any error.
        </block>
        <block>
            However, if you lack a region in your value that exists in the document, the 
            stylesheets will recognize this as an error, notifiy you, and quit. For eaxmple, 
            if your value is ``,bibliographic,dedication,abstract,toc``, and your document 
            contains a title, the processing will quit.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='author-text']" priority = "3">
        <xsl:call-template name="make-name">
            <xsl:with-param name="name">Bibliographic Field Names</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="possible-values">
            <xsl:with-param name="text" select="'Any Text'"/>
        </xsl:call-template>
        <block>**Defaults:** See below</block>
        <block>
            The function is the same for the following parameters:
        </block>
        <block>* author-text (default: Author: )</block>
        <block>* authors-text (default: Authors: )</block>
        <block>* organization-text (default: Organization: )</block>
        <block>* contact-text (default: Contact: )</block>
        <block>* status-text (default: Status: )</block>
        <block>* copyright-text (default: Copyright: )</block>
        <block>* address-text (default: Address: )</block>
        <block>* version-text (default: Version: )</block>
        <block>* revision-text (default: Revison: )</block>
        <block>* date-text (default: Date: )</block>
        <block>
            Each parameter sets the text in the list for that particular bibliographic item. 
            For example if you wanted to change the default for ``contact`` from 'contact' to email, 
            you would simply set this value to 'email'.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='authors-text']| 
        xsl:param[@name='organization-text']| xsl:param[@name='contact-text']|
        xsl:param[@name='status-text']| xsl:param[@name='copyright-text']|
        xsl:param[@name='address-text']| xsl:param[@name='version-text']|
        xsl:param[@name='revision-text']| xsl:param[@name='date-text'] " priority = "3"/>

    <xsl:template match="xsl:param[@name='attention-title']" priority = "3">
        <xsl:call-template name="make-name">
            <xsl:with-param name="name">Admonition Title Names</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="possible-values">
            <xsl:with-param name="text" select="'Any Text'"/>
        </xsl:call-template>
        <block>**Defaults:** See below</block>
        <block>
            The function is the same for the following parameters:
        </block>
        <block>* attention-title (default: Attention!)</block>
        <block>* caution-title (default: Caution!)</block>
        <block>* danger-title (default: !Danger!)</block>
        <block>* error-title (default: Error)</block>
        <block>* hint-title (default: Hint)</block>
        <block>* important-title (default: Important)</block>
        <block>* note-title (default: Note)</block>
        <block>* tip-title (default: Tip)</block>
        <block>* warning-title (default: Warning!)</block>
        <block>
            Each parameter sets the text for the title for that particular Admonition. 
            For example if you wanted to change the default for ``attention-title`` from 
            'Important' to 'Pay Attention!', you would simply set this value to 'Pay Attnetion!'.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='caution-title']| xsl:param[@name='caution-title']|
        xsl:param[@name='danger-title']| xsl:param[@name='error-title']|
        xsl:param[@name='hint-title']| xsl:param[@name='important-title']|
        xsl:param[@name='note-title']| xsl:param[@name='tip-title']|
        xsl:param[@name='warning-title']" priority = "3"/>

    <xsl:template match="xsl:param[@name='transition-text']" priority = "3">
        <xsl:call-template name="make-name">
            <xsl:with-param name="name">transition-text</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="possible-values">
            <xsl:with-param name="text" select="'Any Text'"/>
        </xsl:call-template>
        <block>**Defaults:** \*\*\*</block>
        <block>
            The text to use for a transtion element. Use any text (including an empty 
            string) to change that value.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='number-section1']" priority = "3">
        <xsl:call-template name="make-name">
            <xsl:with-param name="name">Formatting of Section Numbering</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="possible-values">
            <xsl:with-param name="text" select="'Valid Number Formatting String'"/>
        </xsl:call-template>
        <block>**Defaults:** See below</block>
        <block>
            The function is the same for the following parameters:
        </block>
        <block>* number-section1 (default: 1)</block>
        <block>* number-section2 (default: .1)</block>
        <block>* number-section3 (default: .1)</block>
        <block>* number-section4 (default: .1)</block>
        <block>* number-section5 (default: .1)</block>
        <block>* number-section6 (default: .1)</block>
        <block>* number-section7 (default: .1)</block>
        <block>* number-section8 (default: .1)</block>
        <block>* number-section9 (default: .1)</block>
        <block>
            Each parameter sets the formatting (not the actual number) for that particular level.
            The stylesheets allow for a great deal of flexibility here. For example, in 
            order to set a level 3 number format to '(II)3.b', you would set 
            ``number-section1`` to '(I)', ``number-section2`` to '.1' (the default, in this case,
            meaning you woud not need to make a change), and ``number-section3`` to
            '.a'. 
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='number-section2']| xsl:param[@name='number-section3']|
        xsl:param[@name='number-section4']| xsl:param[@name='number-section5']|
        xsl:param[@name='number-section6']| xsl:param[@name='number-section7']|
        xsl:param[@name='number-section8']| xsl:param[@name='number-section9'] " 
        priority = "3"/>

    <xsl:template match="xsl:param[@name='inherit-section-num']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>True, False</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            If set to 'True', each section inherits the section numbering from the sections
            above it. For example, section '1.1.2' will appear as '1.1.2'. If set to 'False', 
            the section number will appear as '2'.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='bullet-text']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>Any Text</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            Use to set the value for the bullets in a bullet list. If this string is left blank,
            then the stylesheets will use the value in the XML.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='bullet-text-level2']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>Any Text</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            Use to set the value for the bullets in a nested bullet list. If this string is left blank,
            then the stylesheets will use the value in the XML.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='options-separator']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>Any Text</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            Use to set the value for the text that separates the options in an option list.
            For example, if your RST file has ``-f  -file`` as the options, and you choose
            ';' as the ``options-separator``, the output becomes ``-f; -file``.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='option-list-format']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>list, definition</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            Use to determine the formatting of an options list. If ``list`` is choosen, then
            the options list is formatted as a traditional list, with the options to the left 
            and the description to the right. If ``definition`` is choosen, the options 
            list is formatted as a defintion list, with the options above the description, which
            is indented. Lists with long options are probably better formatted using 
            ``definition.``
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='number-verse']" priority = "3">
        <xsl:call-template name="make-name">
            <xsl:with-param name="name">number-verse</xsl:with-param>
        </xsl:call-template>
        <block>**Possible Values:** any positive integer, or ``''``</block>
        <block>**Default:** 5</block>
        <block>
            When set, this parameter numbers a line block ("verse") every ``value`` lines.   
            The value of ``'5'`` numbers every 5th line. If ``number-verse`` is left 
            empty, the line block will not be numbered.
        </block>
    </xsl:template>


    <xsl:template match="xsl:param[@name='text-before-block-quote-attribution']" priority = "3">
        <xsl:call-template name="make-name">
            <xsl:with-param name="name">Text Before Attributions</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="possible-values">
            <xsl:with-param name="text" select="'Any Text'"/>
        </xsl:call-template>
        <block>**Defaults:** &#x2014;</block>
        <block>
            The function is the same for the following parameters:
        </block>
        <block>* text-before-block-quote-attribution</block>
        <block>* text-before-epigraph-attribution</block>
        <block>* text-before-pull-quote-attribution</block>
        <block>
            Each parameter determines the text before the attribution. When the parameter
            is left empty, no text will appear before an attribution.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='text-before-epigraph-attribution']|
        xsl:param[@name='text-before-pull-quote-attribution']" priority = "3"/>

    <xsl:template match="xsl:param[@name='table-title-placement']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>top, bottom</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            Where to place the table title, or caption.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='footnote-placement']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>footnote, endnote</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            This parameter determines whether footnotes will function as footnotes, 
            or endnotes. When ``footnote`` is choosen, footnotes appear at the 
            bottom of the page. When ``endnote`` is choosen, the *numbered* footnotes appear
            as endnotes, in the same position where they are in the RST document.
            If ``endnote`` is choosen, symbolic footnotes still appear as footnotes, 
            thus giving a user the ability to use both footnotes and endnotes.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='footnote-style']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>list, traditional</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            This parameter determines the style of the footnote or endnote text. 
            When ``'list'``, is choosen, the text is formatted as a list, 
            with the number as the item. When ``'traditional'`` is choosen,
            the footnote appears in the more traditional manner, as a paragraph
            with the first line indented.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='space-between-footnotes']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>Any Measure</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            Use to set the space between footnotes. (I have not determined how to set 
            this property in the normal way, which is why this property appears as 
            a parameter, rather than in an attribute set, like the other similar
            properties.)
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='internal-link-type']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>link, page, page-link</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            When set to ``'page'``, the page number of the target appears. When 
            set to ``'link'``, the text of the link appears, and clicking on that
            link takes you to the target. When set to ``'page-link'``, the page 
            of the target appears, and clicking on that page number takes you to 
            the target.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='bibliographic-format']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>list, normal, ''</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            Determines the formatting of the bibliographic info. When set to ``'list'`` 
            (the default), the bibliograhic fields will be formatted as a list. When set
            to ``'normal'`` or ``''``, the each bibliographic field will be formatted 
            as a block.
        </block>
    </xsl:template>


    <xsl:template match="xsl:param[@name='custom-bib-info1-name']" priority = "3">
        <xsl:call-template name="make-name">
            <xsl:with-param name="name">Custom bibliographic field names</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="possible-values">
            <xsl:with-param name="text" select="'Any Text'"/>
        </xsl:call-template>
        <block>**Defaults:** ''</block>
        <block>
            The function is the same for the following parameters:
        </block>
        <block>* custom-bib-info1-name</block>
        <block>* custom-bib-info2-name</block>
        <block>* custom-bib-info3-name</block>
        <block>* custom-bib-info4-name</block>
        <block>* custom-bib-info5-name</block>
        <block>* custom-bib-info6-name</block>
        <block>* custom-bib-info7-name</block>
        <block>* custom-bib-info8-name</block>
        <block>* custom-bib-info9-name</block>
        <block>* custom-bib-info10-name</block>
        <block>
            Each parameter sets the value of the corresponding text for cutom bibliographic fields.  
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='custom-bib-info2-name']|
        xsl:param[@name='custom-bib-info3-name']|
        xsl:param[@name='custom-bib-info4-name']|
        xsl:param[@name='custom-bib-info5-name']|
        xsl:param[@name='custom-bib-info6-name']|
        xsl:param[@name='custom-bib-info7-name']|
        xsl:param[@name='custom-bib-info8-name']|
        xsl:param[@name='custom-bib-info9-name']|
        xsl:param[@name='custom-bib-info10-name'] " priority = "3"/>

    <xsl:template match="xsl:param[@name='table-cols']" priority = "3">
        <xsl:call-template name="make-name">
            <xsl:with-param name="name">Custom Table Columns</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="possible-values">
            <xsl:with-param name="text" select="'numbers separated by commas'"/>
        </xsl:call-template>
        <block>**Defaults:** ''</block>
        <block>
            The function is the same for the following parameters:
        </block>
        <block>* table-cols</block>
        <block>* table-borderless-cols</block>
        <block>* table-long-cols</block>
        <block>* table1-cols</block>
        <block>* table2-cols</block>
        <block>* ...</block>
        <block>* table30-cols</block>
        <block>
            Each parameter sets the columns for the table. ``'table-cols'`` sets the columns
            for the default table; ``'table-borderless-cols'`` sets the columns for the 
            borderless table, and ``'table-long-cols'`` sets the columns for the long 
            table. There are also 30 custom tables, and the parameter for the columns is 
            ``'table1-cols'``, ``'table2-cols'`` ... ``'table30-cols'``.
        </block>
        <block>
            Use these parameters to override the defualts created by rst2xml.py
        </block>

        <block>
            Use a value of numbers separated by commas. For example, a value of 
            ``'10,20,10'`` sets the first column to 10, the second to 20, and the
            third to 10. That means the first and third columns will have the 
            same width, and the second will be twice as large as those.
        </block>
    </xsl:template>

    <xsl:template match="xsl:param[@name='table1-cols']|
        xsl:param[@name='table2-cols']|
        xsl:param[@name='table3-cols']|
        xsl:param[@name='table4-cols']|
        xsl:param[@name='table5-cols']|
        xsl:param[@name='table6-cols']|
        xsl:param[@name='table7-cols']|
        xsl:param[@name='table8-cols']|
        xsl:param[@name='table9-cols']|
        xsl:param[@name='table10-cols']|
        xsl:param[@name='table11-cols']|
        xsl:param[@name='table12-cols']|
        xsl:param[@name='table13-cols']|
        xsl:param[@name='table14-cols']|
        xsl:param[@name='table15-cols']|
        xsl:param[@name='table16-cols']|
        xsl:param[@name='table17-cols']|
        xsl:param[@name='table18-cols']|
        xsl:param[@name='table19-cols']|
        xsl:param[@name='table20-cols']|
        xsl:param[@name='table21-cols']|
        xsl:param[@name='table22-cols']|
        xsl:param[@name='table23-cols']|
        xsl:param[@name='table24-cols']|
        xsl:param[@name='table25-cols']|
        xsl:param[@name='table26-cols']|
        xsl:param[@name='table27-cols']|
        xsl:param[@name='table28-cols']|
        xsl:param[@name='table29-cols']|
        xsl:param[@name='table30-cols']|
        xsl:param[@name='table-borderless-cols']|
        xsl:param[@name='table-long-cols'] " priority = "3"/>

    <xsl:template match="xsl:param[@name='long-rows-first-page']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>numbers separated by commas</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>

            Use this property to set the number of rows you want on the first
            page for a table with a class of ``'long'``. Only use if you desire
            a different caption from that which appears on the first page. 

        </block>
        <block>
            FO by itself cannot create different table headings or footings
            from on subsequent pages. The stylesheets get around this
            limitation by creating two tables, one which takes the first
            heading (or footing), and one which takes the second. The user
            must tell the stylesheets when to start the new table; the 
            stylesheets have no way of calcuating this on their own.

        </block>
        <block>
            Use numbers separated by commas for this parameter, where the first number
            inidcatetes the first long table, the second the second table, and so on. For
            example, a value of ``'8,10'`` tells the stylesheet to break the first *long* table
            at 8 rows, and the second at 10 rows.
        </block>
        <block>
            Leave this parameter empty, or set it to 0 in order to have the same caption on
            all pages.
        </block>

    </xsl:template>

    

    <xsl:template match="xsl:param[@name='custom-spacing-header-footer']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>boolean</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
            Tells the stylesheets to use attributes of body and region-before that will determine
            the spacing for the header. Normally, the stylesheets sets the area to .75in, if a
            relevant header or footer is found. When ``'custom-spacing-header-footer'`` 
            is set to true, the stylesheets 
            won't try to generate any spacing, but will require these attributes to be set in 
            the relevant attribute sets.
        </block>
    </xsl:template>


    <xsl:template match="xsl:param[@name='test']" priority = "3">
        <xsl:call-template name="before_p_text">
            <xsl:with-param name="possible-values">
                <xsl:text>True, False, ''</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
        <block>
           For testing purposes only. 
        </block>
    </xsl:template>



    <xsl:template match="xsl:param" priority="2">
        <xsl:message>
            <xsl:text>no match for "</xsl:text>
            <xsl:value-of select="@name"/>
            <xsl:text>"</xsl:text>
        </xsl:message>
    </xsl:template>


    <xsl:template match="@*|node()" />

    
</xsl:stylesheet>
