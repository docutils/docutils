<?xml version="1.0" encoding="utf-8"?>
<!--
	Generates a PDF that DOES NOT include highlighted comments, errors, and 
suchlike.
	Suitable for end-users.
-->
<!--
Set namespace extensions. These are used as [shortname]:[tag] throughout the 
XSL-FO files.
xsl: eXtensible Stylesheet Language (all are implemented by Saxon)
fo: XSL Formatting Objects (almost all are implemented by XEP and FOP)
svg: SVG (Scalable Vector Graphics - only a subset is implemented by XEP and 
FOP)
rx: XEP extensions (RenderX XEP PDF-creation product)
fox: FOP extensions (Apache FOP PDF-creation product, open-source)
saxon: SAXON extensions (Saxon XSL transformation product, open-source)
u: user extensions (indicates utility 'call-template' routines defined in 
these XSL files)
-->
<xsl:stylesheet version="1.0"
	 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 	 xmlns:fo="http://www.w3.org/1999/XSL/Format"
 	 xmlns:fox="http://xml.apache.org/fop/extensions"
 	 xmlns:saxon="http://icl.com/saxon"
 	 xmlns:svg="http://www.w3.org/2000/svg"
 	 xmlns:u="U" 	>
	 
<!--
indent: indent FO output for better human readability (may cause side 
effects).
publish-comments: show ReST comments ('..comment').
publish_filenotes: show ReST file notes ('..file notes').
publish_problematics: show DocUtils-generated problematic error messages.
system_messages: show DocUtils-generated system messages.
-->
<xsl:output indent="no" saxon:indent-spaces="0" />
<xsl:variable name="publish_comments">not-enabled</xsl:variable>
<xsl:variable name="publish_filenotes">not-enabled</xsl:variable>
<xsl:variable name="publish_problematics">not-enabled</xsl:variable>
<xsl:variable name="system_messages">not-enabled</xsl:variable><!--
All inline-level attribute are named "foo_bar"
Most block-level attributes are named "foo_bar_block" EXCEPT when the block 
contains only a text override
All region attributes are named "foo_region_bar"
-->
<!--
The size of a standard page.
TO DO: perhaps have a selection of standard sizes, and a single variable to 
select from them.
-->
<xsl:attribute-set name="paper_size">
	<xsl:attribute name="page-width">8.5in</xsl:attribute>
	<xsl:attribute name="page-height">11in</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="page_size" use-attribute-sets="paper_size">
	<xsl:attribute name="margin-top">0.33in</xsl:attribute>
	<xsl:attribute name="margin-bottom">0.33in</xsl:attribute>
</xsl:attribute-set>
<!--
Page margins for almost all pages, with exceptions as defined below.
-->
<xsl:attribute-set name="generic_region_body">
	<xsl:attribute name="margin-left">1.0in</xsl:attribute>
	<xsl:attribute name="margin-right">1.0in</xsl:attribute>
	<xsl:attribute name="margin-top">0.33in</xsl:attribute>
	<xsl:attribute name="margin-bottom">0.33in</xsl:attribute>
</xsl:attribute-set>
	
<!--
Page margins for some exceptional pages (Safeguards warning, Versions list, 
etc)
-->
<xsl:attribute-set name="large_region_body" use-attribute-sets="generic_region_body">
	<xsl:attribute name="margin-left">0.5in</xsl:attribute>
	<xsl:attribute name="margin-right">0.5in</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid blue</xsl:attribute> -->
</xsl:attribute-set>

<xsl:attribute-set name="maximum_region_body" use-attribute-sets="generic_region_body">
	<xsl:attribute name="margin-top">0in</xsl:attribute>
	<xsl:attribute name="margin-bottom">0in</xsl:attribute>
	<xsl:attribute name="margin-left">0in</xsl:attribute>
	<xsl:attribute name="margin-right">0in</xsl:attribute>
	<xsl:attribute name="padding">0in</xsl:attribute>
</xsl:attribute-set><xsl:attribute-set name="chapter_flow">
	<xsl:attribute name="start-indent">1.75in</xsl:attribute>
	<xsl:attribute name="end-indent">1.75in</xsl:attribute>
</xsl:attribute-set><xsl:attribute-set name="generic_region_before_first">
	<xsl:attribute name="region-name">xsl-region-before</xsl:attribute>
	<xsl:attribute name="extent">0.25in</xsl:attribute>
	<xsl:attribute name="display-align">before</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid green</xsl:attribute> -->
</xsl:attribute-set>
<xsl:attribute-set name="generic_region_after_first">
	<xsl:attribute name="region-name">xsl-region-after</xsl:attribute>
	<xsl:attribute name="extent">0.25in</xsl:attribute>
	<xsl:attribute name="display-align">after</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter_region_before_first" use-attribute-sets="generic_region_before_first" />

<xsl:attribute-set name="chapter_region_after_first" use-attribute-sets="generic_region_after_first">
	<xsl:attribute name="background-color">#F5F5F5</xsl:attribute>
	<xsl:attribute name="border-top">0.5pt solid #BEBEBE</xsl:attribute>
</xsl:attribute-set>


<xsl:attribute-set name="generic_region_start_first">
	<xsl:attribute name="region-name">xsl-region-start</xsl:attribute>
	<xsl:attribute name="extent">2in</xsl:attribute>
	<xsl:attribute name="precedence">true</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid green</xsl:attribute> -->
</xsl:attribute-set>
<xsl:attribute-set name="generic_region_end_first">
	<xsl:attribute name="region-name">xsl-region-end</xsl:attribute>
	<xsl:attribute name="extent">1in</xsl:attribute>
	<xsl:attribute name="precedence">true</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid green</xsl:attribute> -->
</xsl:attribute-set><xsl:attribute-set name="generic_region_before">
	<xsl:attribute name="region-name">xsl-region-before</xsl:attribute>
	<xsl:attribute name="extent">0.25in</xsl:attribute>
	<xsl:attribute name="display-align">before</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="generic_region_after">
	<xsl:attribute name="region-name">xsl-region-after</xsl:attribute>
	<xsl:attribute name="extent">0.25in</xsl:attribute>
	<xsl:attribute name="display-align">after</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter_region_before" use-attribute-sets="generic_region_before">
	<xsl:attribute name="background-color">#F5F5F5</xsl:attribute>
	<xsl:attribute name="border-bottom">0.5pt solid #BEBEBE</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="chapter_region_after" use-attribute-sets="generic_region_after">
	<xsl:attribute name="background-color">#F5F5F5</xsl:attribute>
	<xsl:attribute name="border-top">0.5pt solid #BEBEBE</xsl:attribute>
</xsl:attribute-set>


<xsl:attribute-set name="generic_region_start">
	<xsl:attribute name="region-name">xsl-region-start</xsl:attribute>
	<xsl:attribute name="extent">2in</xsl:attribute>
	<xsl:attribute name="precedence">true</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid green</xsl:attribute> -->
</xsl:attribute-set>
<xsl:attribute-set name="generic_region_end">
	<xsl:attribute name="region-name">xsl-region-end</xsl:attribute>
	<xsl:attribute name="extent">1in</xsl:attribute>
	<xsl:attribute name="precedence">true</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid green</xsl:attribute> -->
</xsl:attribute-set>

<xsl:attribute-set name="balanced_region_start">
	<xsl:attribute name="region-name">xsl-region-start</xsl:attribute>
	<xsl:attribute name="extent">1.5in</xsl:attribute>
	<xsl:attribute name="precedence">true</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid green</xsl:attribute> -->
</xsl:attribute-set>
<xsl:attribute-set name="balanced_region_end">
	<xsl:attribute name="region-name">xsl-region-end</xsl:attribute>
	<xsl:attribute name="extent">1.5in</xsl:attribute>
	<xsl:attribute name="precedence">true</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid green</xsl:attribute> -->
</xsl:attribute-set>

<xsl:attribute-set name="generic_region_start_odd">
	<xsl:attribute name="region-name">xsl-region-start</xsl:attribute>
	<xsl:attribute name="extent">2in</xsl:attribute>
	<xsl:attribute name="precedence">true</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid green</xsl:attribute> -->
</xsl:attribute-set>
<xsl:attribute-set name="generic_region_end_odd">
	<xsl:attribute name="region-name">xsl-region-end</xsl:attribute>
	<xsl:attribute name="extent">1in</xsl:attribute>
	<xsl:attribute name="precedence">true</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid green</xsl:attribute> -->
</xsl:attribute-set>

<xsl:attribute-set name="generic_region_start_even">
	<xsl:attribute name="region-name">xsl-region-start</xsl:attribute>
	<xsl:attribute name="extent">1in</xsl:attribute>
	<xsl:attribute name="precedence">true</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid green</xsl:attribute> -->
</xsl:attribute-set>
<xsl:attribute-set name="generic_region_end_even">
	<xsl:attribute name="region-name">xsl-region-end</xsl:attribute>
	<xsl:attribute name="extent">2in</xsl:attribute>
	<xsl:attribute name="precedence">true</xsl:attribute>
<!--	<xsl:attribute name="border">1px solid green</xsl:attribute> -->
</xsl:attribute-set>
<xsl:attribute-set name="header_text" use-attribute-sets="serif_face">
	<xsl:attribute name="font-size">10pt</xsl:attribute>
	<xsl:attribute name="line-height">12pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="footer_text" use-attribute-sets="serif_face">
	<xsl:attribute name="font-size">10pt</xsl:attribute>
	<xsl:attribute name="line-height">12pt</xsl:attribute>
	<xsl:attribute name="text-align-last">justify</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="header_text_firstpage" use-attribute-sets="header_text" />

<xsl:attribute-set name="footer_text_firstpage" use-attribute-sets="footer_text" />

<xsl:attribute-set name="titlepage_region_body" use-attribute-sets="maximum_region_body" /><xsl:attribute-set name="safeguards_region_body" use-attribute-sets="large_region_body">
	<xsl:attribute name="margin-left">1.5in</xsl:attribute>
	<xsl:attribute name="margin-right">1.5in</xsl:attribute>
	<xsl:attribute name="column-count">2</xsl:attribute>
	<xsl:attribute name="column-gap">.25in</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="simple_region_body" use-attribute-sets="generic_region_body">
	<xsl:attribute name="margin-left">1.5in</xsl:attribute>
	<xsl:attribute name="margin-right">1.5in</xsl:attribute>
</xsl:attribute-set><xsl:attribute-set name="toc_region_body" use-attribute-sets="generic_region_body">
	<xsl:attribute name="margin-left">2.0in</xsl:attribute>
	<xsl:attribute name="margin-right">1.0in</xsl:attribute>
</xsl:attribute-set><xsl:attribute-set name="chapter_firstpage_region_body" use-attribute-sets="generic_region_body">
	<xsl:attribute name="margin-left">0.25in</xsl:attribute>
	<xsl:attribute name="margin-right">-0.75in</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter_region_body_odd" use-attribute-sets="generic_region_body">
	<xsl:attribute name="margin-left">0.25in</xsl:attribute>
	<xsl:attribute name="margin-right">-0.75in</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter_region_body_even" use-attribute-sets="generic_region_body">
	<xsl:attribute name="margin-left">-0.75in</xsl:attribute>
	<xsl:attribute name="margin-right">0.25in</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="swirlie_position">
	<xsl:attribute name="absolute-position">absolute</xsl:attribute>
	<xsl:attribute name="left">0in</xsl:attribute>
	<xsl:attribute name="top">0in</xsl:attribute>
	<xsl:attribute name="overflow">visible</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="main_title_position">
	<xsl:attribute name="position">absolute</xsl:attribute>
	<xsl:attribute name="left">1.5in</xsl:attribute>
	<xsl:attribute name="top">2in</xsl:attribute>
	<xsl:attribute name="background-color">transparent</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="main_title_text" use-attribute-sets="serif_bold_face">
	<xsl:attribute name="font-size">32pt</xsl:attribute>
	<xsl:attribute name="background-color">transparent</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="sub_title_position">
	<xsl:attribute name="position">absolute</xsl:attribute>
	<xsl:attribute name="left">1.5in</xsl:attribute>
	<xsl:attribute name="top">2.5in</xsl:attribute>
	<xsl:attribute name="background-color">transparent</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="sub_title_text" use-attribute-sets="serif_bold_face">
	<xsl:attribute name="font-size">24pt</xsl:attribute>
	<xsl:attribute name="background-color">transparent</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="logo_position">
	<xsl:attribute name="absolute-position">absolute</xsl:attribute>
	<xsl:attribute name="left">6.5in</xsl:attribute>
	<xsl:attribute name="top">9.0in</xsl:attribute>
	<xsl:attribute name="overflow">visible</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="docinfo_block" use-attribute-sets="serif_face">
	<xsl:attribute name="overflow">visible</xsl:attribute>
	<xsl:attribute name="vertical-align">bottom</xsl:attribute>
	<xsl:attribute name="margin-left">1.5in</xsl:attribute>
	<xsl:attribute name="margin-bottom">1.0in</xsl:attribute>
</xsl:attribute-set>




<xsl:attribute-set name="safeguards_title" use-attribute-sets="chapter_title">
	<xsl:attribute name="span">all</xsl:attribute>
	<xsl:attribute name="margin-bottom">9pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="book_toc_chapterblock">
	<xsl:attribute name="space-before">3pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="book_toc_chapterentry">
	<xsl:attribute name="font-size">12pt</xsl:attribute>
	<xsl:attribute name="text-align-last">justify</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="book_toc_sectionentry" use-attribute-sets="sans_face">
	<xsl:attribute name="font-size">10pt</xsl:attribute>
	<xsl:attribute name="margin-left">.25in</xsl:attribute>
</xsl:attribute-set>

<!--
See Titles/Special Titles branch.  Applied to copyright, colophon, doc 
versioning, etc.
-->
<xsl:attribute-set name="chapter_title_block">
	<xsl:attribute name="color">#FFFFFF</xsl:attribute>
	<xsl:attribute name="background-color">#006400</xsl:attribute>
	<xsl:attribute name="margin-bottom">9pt</xsl:attribute>
	<xsl:attribute name="padding-left">3pt</xsl:attribute>
	<xsl:attribute name="padding-right">3pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter_title" use-attribute-sets="titles">
	<xsl:attribute name="font-size">24pt</xsl:attribute>
	<xsl:attribute name="line-height">32pt</xsl:attribute>
	<xsl:attribute name="color">#FFFFFF</xsl:attribute>
	<xsl:attribute name="background-color">#006400</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter_title_left" use-attribute-sets="chapter_title">
	<xsl:attribute name="background-color">transparent</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter_title_right" use-attribute-sets="chapter_title">
	<xsl:attribute name="background-color">transparent</xsl:attribute>
	<xsl:attribute name="text-align">right</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter-toc" use-attribute-sets="titles">
	<xsl:attribute name="space-before">24pt</xsl:attribute>
	<xsl:attribute name="text-align">right</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter_toc_entry" use-attribute-sets="titles">
	<xsl:attribute name="text-align">right</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter_toc_pagenum" use-attribute-sets="titles">
<xsl:attribute name="text-align">right</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter-rule">
	<xsl:attribute name="leader-length">100%</xsl:attribute>
	<xsl:attribute name="leader-pattern">rule</xsl:attribute>
	<xsl:attribute name="alignment-baseline">middle</xsl:attribute>
	<xsl:attribute name="space-before">18pt</xsl:attribute>
	<xsl:attribute name="rule-thickness">0.5pt</xsl:attribute>
	<xsl:attribute name="color">#006400</xsl:attribute>
</xsl:attribute-set><xsl:attribute-set name="doc_version_info" use-attribute-sets="sans_face">
	<xsl:attribute name="font-size">8pt</xsl:attribute>
	<xsl:attribute name="text-align-last">justify</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="generic_text" use-attribute-sets="sans_face">
	<xsl:attribute name="font-size">10pt</xsl:attribute>
	<xsl:attribute name="line-height">12pt</xsl:attribute>
	<xsl:attribute name="space-before">3pt</xsl:attribute>
	<xsl:attribute name="space-after">3pt</xsl:attribute>
	<xsl:attribute name="hyphenate">true</xsl:attribute>
</xsl:attribute-set>
<!--
See the /XEP/etc/fonts.xml file for font-family definitions.
-->
<xsl:attribute-set name="serif_face">
	<xsl:attribute name="font-family">OfficinaSerif</xsl:attribute>
	<xsl:attribute name="font-weight">normal</xsl:attribute>
	<xsl:attribute name="font-style">normal</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="serif_bold_face" use-attribute-sets="serif_face">
	<xsl:attribute name="font-weight">bold</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="serif_italic_face" use-attribute-sets="serif_face">
	<xsl:attribute name="font-style">italic</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="serif_bold_italic_face" use-attribute-sets="serif_face">
	<xsl:attribute name="font-weight">bold</xsl:attribute>
	<xsl:attribute name="font-style">italic</xsl:attribute>
</xsl:attribute-set>
<!--
See the /XEP/etc/fonts.xml file for font-family definitions.
-->
<xsl:attribute-set name="sans_face">
	<xsl:attribute name="font-family">OfficinaSans</xsl:attribute>
	<xsl:attribute name="font-weight">normal</xsl:attribute>
	<xsl:attribute name="font-style">normal</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="sans_bold_face" use-attribute-sets="sans_face">
	<xsl:attribute name="font-weight">bold</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="sans_italic_face" use-attribute-sets="sans_face">
	<xsl:attribute name="font-style">italic</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="sans_bold_italic_face" use-attribute-sets="sans_face">
	<xsl:attribute name="font-weight">bold</xsl:attribute>
	<xsl:attribute name="font-style">italic</xsl:attribute>
</xsl:attribute-set>
<!--
See the /XEP/etc/fonts.xml file for font-family definitions.
-->
<xsl:attribute-set name="mono_face">
	<xsl:attribute name="font-family">monospace</xsl:attribute>
	<xsl:attribute name="font-weight">normal</xsl:attribute>
	<xsl:attribute name="font-style">normal</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="mono_bold_face" use-attribute-sets="mono_face">
	<xsl:attribute name="font-weight">bold</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="mono_italic_face" use-attribute-sets="mono_face">
	<xsl:attribute name="font-style">italic</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="mono_bold_italic_face" use-attribute-sets="mono_face">
	<xsl:attribute name="font-weight">bold</xsl:attribute>
	<xsl:attribute name="font-style">italic</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="generic_block" use-attribute-sets="generic_text" />
<xsl:attribute-set name="admonition_block">
	<xsl:attribute name="space-before">6pt</xsl:attribute>
	<xsl:attribute name="space-after">6pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="admonition_block_with_icon" use-attribute-sets="admonition_block">
	<xsl:attribute name="keep-together">always</xsl:attribute>
	<xsl:attribute name="start-indent">inherited-property-value(start-indent) - 0.33in</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="admonition_icon_float">
	<xsl:attribute name="float">left</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="admonition_icon_block" />

<xsl:attribute-set name="admonition_with_icon" use-attribute-sets="admonition_block">
	<xsl:attribute name="intrusion-displace">none</xsl:attribute>
	<xsl:attribute name="start-indent">inherited-property-value(start-indent) + 0.33in</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="attention_block" use-attribute-sets="admonition_block" /><xsl:attribute-set name="danger_block" use-attribute-sets="admonition_block" /><xsl:attribute-set name="error_block" use-attribute-sets="admonition_block" /><xsl:attribute-set name="hint_block" use-attribute-sets="admonition_block" /><xsl:attribute-set name="note_block" use-attribute-sets="admonition_block" /><xsl:attribute-set name="important_block" use-attribute-sets="admonition_with_icon">
<!--	<xsl:attribute name="border-left">1pt solid blue</xsl:attribute>
	<xsl:attribute name="border-bottom">none</xsl:attribute>
	<xsl:attribute name="border-right">none</xsl:attribute>
	<xsl:attribute name="border-top">none</xsl:attribute> -->
</xsl:attribute-set><xsl:attribute-set name="tip_block" use-attribute-sets="admonition_with_icon">
<!--	<xsl:attribute name="border-left">1pt solid green</xsl:attribute>
	<xsl:attribute name="border-bottom">none</xsl:attribute>
	<xsl:attribute name="border-right">none</xsl:attribute>
	<xsl:attribute name="border-top">none</xsl:attribute> -->
</xsl:attribute-set><xsl:attribute-set name="caution_block" use-attribute-sets="admonition_with_icon">
<!--	<xsl:attribute name="border-left">1pt solid olive</xsl:attribute>
	<xsl:attribute name="border-bottom">none</xsl:attribute>
	<xsl:attribute name="border-right">none</xsl:attribute>
	<xsl:attribute name="border-top">none</xsl:attribute> -->
</xsl:attribute-set>
<xsl:attribute-set name="warning_block" use-attribute-sets="admonition_with_icon">
<!--	<xsl:attribute name="border-right">1pt solid red</xsl:attribute>
	<xsl:attribute name="border-bottom">none</xsl:attribute>
	<xsl:attribute name="border-left">none</xsl:attribute>
	<xsl:attribute name="border-top">none</xsl:attribute> -->
</xsl:attribute-set>
<xsl:attribute-set name="list_block" use-attribute-sets="generic_block">
	<xsl:attribute name="provisional-label-separation">0em</xsl:attribute>
	<xsl:attribute name="provisional-distance-between-starts">1em</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="list_item_label" use-attribute-sets="sans_face" />

<xsl:attribute-set name="list_item" /><xsl:attribute-set name="bulleted_list_item" use-attribute-sets="list_item">
	<xsl:attribute name="margin-left">1em</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="bulleted_list_item_1stparagraph" use-attribute-sets="bulleted_list_item">
	<xsl:attribute name="space-before">0pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="bulleted_list_item_label" use-attribute-sets="list_item_label">
	<xsl:attribute name="baseline-shift">6pt</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="enumerated_list_item" use-attribute-sets="list_item" />

<xsl:attribute-set name="enumerated_list_item_1stparagraph" use-attribute-sets="enumerated_list_item">
	<xsl:attribute name="space-before">0pt</xsl:attribute>
</xsl:attribute-set><xsl:attribute-set name="definition_block" use-attribute-sets="generic_block" />
<xsl:attribute-set name="definition_item_block" use-attribute-sets="definition_block">
	<xsl:attribute name="provisional-label-separation">0.5em</xsl:attribute>
	<xsl:attribute name="provisional-distance-between-starts">1em</xsl:attribute>
	<xsl:attribute name="keep-together">always</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="definition_term" use-attribute-sets="generic_text sans_bold_face">
	<xsl:attribute name="space-after">0pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="definition_classifier" use-attribute-sets="generic_text" />

<xsl:attribute-set name="definition_definition" use-attribute-sets="generic_text">
	<xsl:attribute name="start-indent">inherited-property-value(start-indent) + 0.25in</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="literal_block" use-attribute-sets="generic_block">
	<xsl:attribute name="white-space-collapse">false</xsl:attribute>
	<xsl:attribute name="linefeed-treatment">preserve</xsl:attribute>
	<xsl:attribute name="white-space-treatment">preserve</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="figure_block" use-attribute-sets="generic_block">
	<xsl:attribute name="border">0.5pt solid #BEBEBE</xsl:attribute>
	<xsl:attribute name="space-before">6pt</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="footnote_block" use-attribute-sets="generic_block" />

<xsl:attribute-set name="block_quote_block" use-attribute-sets="generic_block">
	<xsl:attribute name="padding">12pt</xsl:attribute>
	<xsl:attribute name="space-before">6pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="sidebar_block" use-attribute-sets="generic_block" >
	<xsl:attribute name="width">1.75in - 6pt - 0.5em</xsl:attribute>
	<xsl:attribute name="start-indent">inherited-property-value(start-indent) - 1.75in + 3pt</xsl:attribute>
	<xsl:attribute name="end-indent">inherited-property-value(end-indent) - 1.75in + 3pt</xsl:attribute>
	<xsl:attribute name="border">0.5pt solid  #BEBEBE</xsl:attribute>
	<xsl:attribute name="padding">3pt</xsl:attribute>
	<xsl:attribute name="background-color">#F5FFFA</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="sidebar_float">
	<xsl:attribute name="float">inside</xsl:attribute>
	<xsl:attribute name="clear">none</xsl:attribute>
</xsl:attribute-set>


<xsl:attribute-set name="topic_block" use-attribute-sets="generic_block" >
	<xsl:attribute name="border">0pt double #BEBEBE</xsl:attribute>
	<xsl:attribute name="background-color">#F5FFFA</xsl:attribute>
</xsl:attribute-set>


<xsl:attribute-set name="line_block" use-attribute-sets="generic_block">
	<xsl:attribute name="white-space-collapse">false</xsl:attribute>
	<xsl:attribute name="linefeed-treatment">preserve</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="image_block">
	<xsl:attribute name="space-before">6pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="external_graphic">
	<xsl:attribute name="content-width">scale-to-fit</xsl:attribute>
	<xsl:attribute name="content-height">scale-to-fit</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="emphasis">
	<xsl:attribute name="font-weight">bold</xsl:attribute>
</xsl:attribute-set>


<xsl:attribute-set name="footnote_ref" use-attribute-sets="sans_bold_italic_face">
	<xsl:attribute name="font-size">xx-small</xsl:attribute>
	<xsl:attribute name="baseline-shift">super</xsl:attribute>
	<xsl:attribute name="vertical-align">super</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="literal_text">
	<xsl:attribute name="white-space-collapse">false</xsl:attribute>
	<xsl:attribute name="white-space-treatment">preserve</xsl:attribute>
</xsl:attribute-set><xsl:attribute-set name="reference" use-attribute-sets="sans_italic_face" />


<xsl:attribute-set name="generic_matter_title" use-attribute-sets="sans_bold_face titles">
	<xsl:attribute name="font-size">12pt</xsl:attribute>
	<xsl:attribute name="line-height">14pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="titles" use-attribute-sets="serif_bold_face">
	<xsl:attribute name="font-size">10pt</xsl:attribute>
	<xsl:attribute name="line-height">12pt</xsl:attribute>
	<xsl:attribute name="keep-with-next">always</xsl:attribute>
	<xsl:attribute name="space-after">0pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="title-level1" use-attribute-sets="titles">
	<xsl:attribute name="font-size">18pt</xsl:attribute>
	<xsl:attribute name="line-height">24pt</xsl:attribute>
	<xsl:attribute name="border-top">3pt solid #006400</xsl:attribute>
	<xsl:attribute name="border-right">3pt solid #006400</xsl:attribute>
	<xsl:attribute name="padding-right">3pt</xsl:attribute>
	<xsl:attribute name="space-before">18pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="title-level2" use-attribute-sets="titles">
	<xsl:attribute name="font-size">14pt</xsl:attribute>
	<xsl:attribute name="line-height">14pt</xsl:attribute>
	<xsl:attribute name="space-before">12pt</xsl:attribute>
</xsl:attribute-set>

 <xsl:attribute-set name="title-level3" use-attribute-sets="titles">
	<xsl:attribute name="font-size">12pt</xsl:attribute>
	<xsl:attribute name="line-height">12pt</xsl:attribute>
	<xsl:attribute name="space-before">12pt</xsl:attribute>
 </xsl:attribute-set>

<xsl:attribute-set name="title-level4up" use-attribute-sets="titles">
</xsl:attribute-set>


<xsl:attribute-set name="table_block" use-attribute-sets="generic_block">
	<xsl:attribute name="border">0.5pt solid #BEBEBE</xsl:attribute>
</xsl:attribute-set><xsl:attribute-set name="table_head" use-attribute-sets="sans_bold_face">
	<xsl:attribute name="background-color">#DCDCDC</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="table_body" use-attribute-sets="sans_face" />

<xsl:attribute-set name="table_row" />
	<!-- no FOP support for margins, padding, etc? -->

<xsl:variable name="even_row-color">#F0FFF0</xsl:variable>
<xsl:variable name="odd_row-color">#FFFFFF</xsl:variable>
<xsl:attribute-set name="table_entry">
	<xsl:attribute name="start-indent">0pt</xsl:attribute>
	<xsl:attribute name="end-indent">0pt</xsl:attribute>
	<xsl:attribute name="padding">3pt 3pt 1.5pt 3pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="table_entry_1stparagraph" use-attribute-sets="generic_text">
	<xsl:attribute name="space-before">0pt</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="marker_text">
<!-- FOP bug requires overriding most of parent's attributes to
prevent that style being applied when retrieved -->
<!-- <xsl:attribute name="font-family">OfficinaSerifBook,serif</xsl:attribute>
<xsl:attribute name="font-weight">normal</xsl:attribute>
<xsl:attribute name="font-style">normal</xsl:attribute>
<xsl:attribute name="font-size">10pt</xsl:attribute>
<xsl:attribute name="line-height">0pt</xsl:attribute>
<xsl:attribute name="space-before">0pt</xsl:attribute>
<xsl:attribute name="space-before.precedence">3.0</xsl:attribute>
<xsl:attribute name="space-after">0pt</xsl:attribute>
<xsl:attribute name="space-before.precedence">3.0</xsl:attribute>
<xsl:attribute name="margin">0pt</xsl:attribute>
<xsl:attribute name="border">0pt</xsl:attribute>
<xsl:attribute name="padding">0pt</xsl:attribute>
<xsl:attribute name="color">#000000</xsl:attribute>
--> </xsl:attribute-set>

<xsl:attribute-set name="internal_link" use-attribute-sets="sans_bold_italic_face">
	<xsl:attribute name="color">#006400</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="external_link" use-attribute-sets="sans_bold_italic_face">
	<xsl:attribute name="color">#191970</xsl:attribute>
</xsl:attribute-set><xsl:attribute-set name="review_text">
	<xsl:attribute name="color">#00008B</xsl:attribute>
	<xsl:attribute name="margin-left">-12pt</xsl:attribute>
	<xsl:attribute name="margin-right">-12pt</xsl:attribute>
	<xsl:attribute name="border-left">3pt solid #0000FF</xsl:attribute>
	<xsl:attribute name="border-right">3pt solid #0000FF</xsl:attribute>
	<xsl:attribute name="padding-left">9pt</xsl:attribute>
	<xsl:attribute name="padding-right">9pt</xsl:attribute></xsl:attribute-set>

<xsl:attribute-set name="docutil_message" use-attribute-sets="generic_text">
	<xsl:attribute name="font-family">sans-serif</xsl:attribute>
	<xsl:attribute name="color">#00008B</xsl:attribute>
	<xsl:attribute name="margin-left">-12pt</xsl:attribute>
	<xsl:attribute name="margin-right">-12pt</xsl:attribute>
	<xsl:attribute name="border-left">3pt solid #00008B</xsl:attribute>
	<xsl:attribute name="border-right">3pt solid #00008B</xsl:attribute>
	<xsl:attribute name="padding-left">9pt</xsl:attribute>
	<xsl:attribute name="padding-right">9pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="comment" use-attribute-sets="docutil_message">
	<xsl:attribute name="color">#8B0000</xsl:attribute>
	<xsl:attribute name="border-left">3pt solid #8B0000</xsl:attribute>
	<xsl:attribute name="border-right">3pt solid #8B0000</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="problem" use-attribute-sets="docutil_message" />

<xsl:attribute-set name="system_message" use-attribute-sets="docutil_message">
	<xsl:attribute name="font-weight">bold</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="unknown_tag" use-attribute-sets="docutil_message">
	<xsl:attribute name="color">#FF0000</xsl:attribute>
	<xsl:attribute name="font-weight">bold</xsl:attribute>
	<xsl:attribute name="border-left">3pt solid red</xsl:attribute>
	<xsl:attribute name="border-right">3pt solid red</xsl:attribute>
</xsl:attribute-set>

<!--
This branch defines the structure of the book, both at the book level (the 
sequence of sections) and the page level (start, end, before, after, and body 
regions.)
-->
<xsl:template match="/">
	<fo:root>
		<!--
			Define the page architecture for all sections of the document.
		-->
		<fo:layout-master-set>
			<!--
				Title pages have no before/after//start/end areas so that the LCS 
			"Swirlie" logo can go to the edges.
				There is currently no proper end-page
			-->
			<fo:simple-page-master master-name="Title Page" xsl:use-attribute-sets="paper_size">
				<fo:region-body xsl:use-attribute-sets="titlepage_region_body" />				<!-- No region before/after::start/end areas -->
			</fo:simple-page-master>			<!--
				Safeguards uses a two-column format.
			-->
			<fo:simple-page-master master-name="Safeguards" xsl:use-attribute-sets="page_size">
				<fo:region-body xsl:use-attribute-sets="safeguards_region_body" />
				<fo:region-before xsl:use-attribute-sets="generic_region_before" />
				<fo:region-after xsl:use-attribute-sets="generic_region_after" />
				<fo:region-start xsl:use-attribute-sets="balanced_region_start" />
				<fo:region-end xsl:use-attribute-sets="balanced_region_end" />
			</fo:simple-page-master>
			
			<fo:simple-page-master master-name="Simple Frontmatter" xsl:use-attribute-sets="page_size">
				<fo:region-body xsl:use-attribute-sets="simple_region_body" />
				<fo:region-before xsl:use-attribute-sets="generic_region_before" />
				<fo:region-after xsl:use-attribute-sets="generic_region_after" />
				<fo:region-start xsl:use-attribute-sets="balanced_region_start" />
				<fo:region-end xsl:use-attribute-sets="balanced_region_end" />
			</fo:simple-page-master>
			
			<!--
			Preface and other frontmatter chapters are handled as a variation 
			of chapter.
			-->
			<fo:simple-page-master	master-name="ToC" xsl:use-attribute-sets="page_size">
				<fo:region-body xsl:use-attribute-sets="toc_region_body" />
				<fo:region-before xsl:use-attribute-sets="generic_region_before" />
				<fo:region-after xsl:use-attribute-sets="chapter_region_after" />
				<fo:region-start xsl:use-attribute-sets="generic_region_start_odd" />
				<fo:region-end xsl:use-attribute-sets="generic_region_end_odd" />
			</fo:simple-page-master>
			
			
			
			<!--
			For our purposes, first pages *always* start on an odd page.
			-->
			<fo:simple-page-master master-name="Chapter First Page" xsl:use-attribute-sets="page_size">
				<fo:region-body xsl:use-attribute-sets="chapter_firstpage_region_body" />
				<fo:region-before xsl:use-attribute-sets="generic_region_before_first" />
				<fo:region-after xsl:use-attribute-sets="chapter_region_after_first" />
				<fo:region-start xsl:use-attribute-sets="generic_region_start_odd" />
				<fo:region-end xsl:use-attribute-sets="generic_region_end_odd" />
				
				
				
			</fo:simple-page-master>
			
			<fo:simple-page-master master-name="Chapter Odd Pages" xsl:use-attribute-sets="page_size">
				<fo:region-body xsl:use-attribute-sets="chapter_region_body_odd" />				<fo:region-before xsl:use-attribute-sets="chapter_region_before" />
				<fo:region-after xsl:use-attribute-sets="chapter_region_after" />
				<fo:region-start xsl:use-attribute-sets="generic_region_start_odd" />
				<fo:region-end xsl:use-attribute-sets="generic_region_end_odd" />
			</fo:simple-page-master>
			
			<fo:simple-page-master master-name="Chapter Even Pages" xsl:use-attribute-sets="page_size">
				<fo:region-body xsl:use-attribute-sets="chapter_region_body_even" />				<fo:region-before xsl:use-attribute-sets="chapter_region_before" />
				<fo:region-after xsl:use-attribute-sets="chapter_region_after" />
				<fo:region-start xsl:use-attribute-sets="generic_region_start_even" />
				<fo:region-end xsl:use-attribute-sets="generic_region_end_even" />
			</fo:simple-page-master>
			
			<!--
				Used by core transformations when the section type can not be 
			identified.
			-->
			<fo:simple-page-master master-name="Unknown FM_EM" xsl:use-attribute-sets="page_size" >
				<fo:region-body xsl:use-attribute-sets="generic_region_body" />
			</fo:simple-page-master>
			<!--
			Sections can have varying page architecture (layout masters). Some 
			macro-style definitions account for the most common section parts, 
			as various combinations of first, last, in-between ("rest"), even, 
			and odd pages.
			-->
			<fo:page-sequence-master master-name="Chapter">
				<fo:repeatable-page-master-alternatives>
					<fo:conditional-page-master-reference master-reference="Chapter First Page" page-position="first" />
					<fo:conditional-page-master-reference master-reference="Chapter Odd Pages" page-position="rest" odd-or-even="odd" />
					<fo:conditional-page-master-reference master-reference="Chapter Even Pages" page-position="rest" odd-or-even="even" />
					<fo:conditional-page-master-reference master-reference="Chapter Even Pages" page-position="last" odd-or-even="even" />
				</fo:repeatable-page-master-alternatives>
			</fo:page-sequence-master>		</fo:layout-master-set>
		<!--
		Define the page sequence for all sections of the document.
		-->
		<fo:page-sequence master-reference="Title Page" initial-page-number="0">
			<fo:flow flow-name="xsl-region-body">
				<fo:block>
					<!-- images applied before text are placed behind the text -->
					<xsl:apply-templates select="//document/image" mode="titlepage" />
					<xsl:apply-templates select="//document/title | //document/subtitle | //document/docinfo" mode="titlepage" />
				</fo:block>
			</fo:flow>
		</fo:page-sequence>
		<xsl:for-each select="//document/section[@class = 'specialfrontmatter']">
			<xsl:choose>
				<xsl:when test="@id='important-safeguards'">
					<fo:page-sequence master-reference="Safeguards" format="i" initial-page-number="auto-odd">
						<fo:static-content flow-name="xsl-region-after">
							<fo:block xsl:use-attribute-sets="footer_text">
								<fo:retrieve-marker retrieve-class-name="chapter" />
								<fo:leader leader-pattern="space" />
								<fo:page-number />
							</fo:block>
						</fo:static-content>
						<fo:flow flow-name="xsl-region-body">
							<xsl:apply-templates select="title" mode="safeguards" />
							<fo:block>
								<xsl:apply-templates select="child::*[name()!='title']" mode="safeguards" />
										<!-- the admonitions template will strip off mode="safeguards" after it has handled the title specially -->
							</fo:block>
						</fo:flow>
					</fo:page-sequence>
				</xsl:when>				<xsl:when test="@id='copyright-and-trademark-information'">
					<fo:page-sequence master-reference="Simple Frontmatter" format="i" initial-page-number="auto-even">
						<fo:static-content flow-name="xsl-region-after">
							<fo:block xsl:use-attribute-sets="footer_text">
								<fo:retrieve-marker retrieve-class-name="chapter" />
								<fo:leader leader-pattern="space" />
								<fo:page-number />
							</fo:block>
						</fo:static-content>
						<fo:flow flow-name="xsl-region-body">
							<xsl:apply-templates select="title" mode="copyright" />
							<xsl:apply-templates select="child::*[name()!='title']" />
						</fo:flow>
					</fo:page-sequence>
				</xsl:when>
				<xsl:otherwise>
					<fo:page-sequence master-reference="Unknown" initial-page-number="auto-odd">
						<fo:flow flow-name="xsl-region-body">
							<fo:block xsl:use-attribute-sets="unknown_tag" border="1px solid red">
								<xsl:text> Special Frontmatter: Section </xsl:text>
								<xsl:value-of select="@id" />
								<xsl:text> (</xsl:text><xsl:value-of select="@name" /><xsl:text>) not implemented; see PDF.XSL page sequencing templates.</xsl:text>
							</fo:block>
						</fo:flow>
					</fo:page-sequence>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each>
		<!--
		The table of contents is generated on-the-fly; it is not available (or 
		is ignored) in the source XML file.
		-->
		<fo:page-sequence master-reference="ToC" format="i" initial-page-number="auto-odd">
			<fo:static-content flow-name="xsl-region-after">
				<fo:block xsl:use-attribute-sets="footer_text">
					<xsl:text>Contents</xsl:text>
					<fo:leader leader-pattern="space" />
					<fo:page-number />
				</fo:block>
			</fo:static-content>
			<fo:flow flow-name="xsl-region-body">
				<xsl:call-template name="u:book_toc_title" />
				<xsl:call-template name="u:book_toc" />
			</fo:flow >
		</fo:page-sequence>
		
		<xsl:for-each select="//document/section[@class = 'frontmatter']">
			<fo:page-sequence master-reference="Chapter" format="i" initial-page-number="auto-odd">
				<!--
				First page has the version number and page number in the 
				footer region.
				-->
				<fo:static-content flow-name="xsl-region-before-first">
					<fo:block xsl:use-attribute-sets="header_text_firstpage">
						<xsl:text> v</xsl:text>
						<xsl:call-template name="u:docinfo-revision" />
					</fo:block>
				</fo:static-content>
				
				<fo:static-content flow-name="xsl-region-after-first">
					<fo:block xsl:use-attribute-sets="footer_text_firstpage">
						<xsl:text>v</xsl:text>
						<xsl:call-template name="u:docinfo-revision" />
						<fo:leader leader-pattern="space" />
						<fo:page-number />
					</fo:block>
				</fo:static-content>				<!--
				Remaining pages place document version number in header, 
				chapter name and page number in footer
				-->
				<fo:static-content flow-name="xsl-region-before">
					<fo:block xsl:use-attribute-sets="header_text">
						<xsl:text> v</xsl:text>
						<xsl:call-template name="u:docinfo-revision" />
					</fo:block>
				</fo:static-content>
						
				<fo:static-content flow-name="xsl-region-after">
					<fo:block xsl:use-attribute-sets="footer_text">
						<fo:retrieve-marker retrieve-class-name="chapter" />
						<fo:leader leader-pattern="space" />
						<fo:page-number />
					</fo:block>
				</fo:static-content>
				<fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="chapter_flow">
					<xsl:call-template name="u:not_numbered_chapter_title" />
					<xsl:call-template name="u:chapter_toc" />
					<xsl:apply-templates select="child::*[name()!='title']" />
				</fo:flow>
			</fo:page-sequence>
		</xsl:for-each>
		
		
		
		<xsl:for-each select="//document/section[not(@class = 'frontmatter' or @class = 'specialfrontmatter' or @class = 'endmatter' or @class = 'specialendmatter')]">
			<xsl:choose>
				<!--
				The first chapter needs to get page 1.
				-->
				<xsl:when test="position() = 1">
					<fo:page-sequence master-reference="Chapter" format="1" initial-page-number="1">
						<!--
						First page has the version number and page number in 
						the footer region.
						-->
						<fo:static-content flow-name="xsl-region-before-first">
							<fo:block xsl:use-attribute-sets="header_text_firstpage">
								<xsl:text> v</xsl:text>
								<xsl:call-template name="u:docinfo-revision" />
							</fo:block>
						</fo:static-content>
						
						<fo:static-content flow-name="xsl-region-after-first">
							<fo:block xsl:use-attribute-sets="footer_text_firstpage">
								<xsl:text>v</xsl:text>
								<xsl:call-template name="u:docinfo-revision" />
								<fo:leader leader-pattern="space" />
								<fo:page-number />
							</fo:block>
						</fo:static-content>						<!--
						Remaining pages place document version number in 
						header, chapter name and page number in footer
						-->
						<fo:static-content flow-name="xsl-region-before">
							<fo:block xsl:use-attribute-sets="header_text">
								<xsl:text> v</xsl:text>
								<xsl:call-template name="u:docinfo-revision" />
							</fo:block>
						</fo:static-content>
								
						<fo:static-content flow-name="xsl-region-after">
							<fo:block xsl:use-attribute-sets="footer_text">
								<fo:retrieve-marker retrieve-class-name="chapter" />
								<fo:leader leader-pattern="space" />
								<fo:page-number />
							</fo:block>
						</fo:static-content>
						<fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="chapter_flow">
							<xsl:call-template name="u:numbered_chapter_title" />
							<xsl:call-template name="u:chapter_toc" />
							<xsl:apply-templates select="child::*[name()!='title']" />
						</fo:flow>
					</fo:page-sequence>
				</xsl:when>				<xsl:otherwise>
					<!-- not the first chapter, page numbering follows previous chapter -->
					<fo:page-sequence master-reference="Chapter" format="1" initial-page-number="auto-odd">
						<!--
						First page has the version number and page number in 
						the footer region.
						-->
						<fo:static-content flow-name="xsl-region-before-first">
							<fo:block xsl:use-attribute-sets="header_text_firstpage">
								<xsl:text> v</xsl:text>
								<xsl:call-template name="u:docinfo-revision" />
							</fo:block>
						</fo:static-content>
						
						<fo:static-content flow-name="xsl-region-after-first">
							<fo:block xsl:use-attribute-sets="footer_text_firstpage">
								<xsl:text>v</xsl:text>
								<xsl:call-template name="u:docinfo-revision" />
								<fo:leader leader-pattern="space" />
								<fo:page-number />
							</fo:block>
						</fo:static-content>						<!--
						Remaining pages place document version number in 
						header, chapter name and page number in footer
						-->
						<fo:static-content flow-name="xsl-region-before">
							<fo:block xsl:use-attribute-sets="header_text">
								<xsl:text> v</xsl:text>
								<xsl:call-template name="u:docinfo-revision" />
							</fo:block>
						</fo:static-content>
								
						<fo:static-content flow-name="xsl-region-after">
							<fo:block xsl:use-attribute-sets="footer_text">
								<fo:retrieve-marker retrieve-class-name="chapter" />
								<fo:leader leader-pattern="space" />
								<fo:page-number />
							</fo:block>
						</fo:static-content>
						<fo:flow flow-name="xsl-region-body" xsl:use-attribute-sets="chapter_flow">
							<xsl:call-template name="u:numbered_chapter_title" />
							<xsl:call-template name="u:chapter_toc" />
							<xsl:apply-templates select="child::*[name()!='title']" />
						</fo:flow>
					</fo:page-sequence>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each>		<xsl:for-each select="//document/section[@class = 'specialendmatter']">
			<xsl:choose>
				<xsl:when test="@id='colophon'">
					<fo:page-sequence master-reference="Simple Frontmatter" format="1" initial-page-number="auto">
						<fo:static-content flow-name="xsl-region-after">
							<fo:block xsl:use-attribute-sets="footer_text">
								<fo:retrieve-marker retrieve-class-name="chapter" />
								<fo:leader leader-pattern="space" />
								<fo:page-number />
							</fo:block>
						</fo:static-content>
						<fo:flow flow-name="xsl-region-body">
							<xsl:apply-templates select="title" mode="colophon" />
							<xsl:apply-templates select="child::*[name()!='title']" />
						</fo:flow>
					</fo:page-sequence>
				</xsl:when>
				<xsl:when test="@id='documentation-versions-reference'">
					<fo:page-sequence master-reference="Simple Frontmatter" format="1" initial-page-number="auto-odd">
						<fo:static-content flow-name="xsl-region-after">
							<fo:block xsl:use-attribute-sets="footer_text">
								<fo:retrieve-marker retrieve-class-name="chapter" />
								<fo:leader leader-pattern="space" />
								<fo:page-number />
							</fo:block>
						</fo:static-content>
						<fo:flow flow-name="xsl-region-body">
							<xsl:apply-templates select="title" mode="versions" />
							<xsl:apply-templates select="child::*[name()!='title']" />
							<xsl:call-template name="u:list_versions" />
						</fo:flow>
					</fo:page-sequence>
				</xsl:when>
				<xsl:otherwise>
					<fo:page-sequence master-reference="Unknown" initial-page-number="auto-odd">
						<fo:flow flow-name="xsl-region-body">
							<fo:block xsl:use-attribute-sets="unknown_tag">
								<xsl:text> Special Endmatter: Section </xsl:text>
								<xsl:value-of select="@id" />
								<xsl:text> (</xsl:text><xsl:value-of select="@name" /><xsl:text>) not implemented; see PDF.XSL page sequencing templates.</xsl:text>
							</fo:block>
						</fo:flow>
					</fo:page-sequence>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each>
	</fo:root>	
</xsl:template><!--
This branch defines the layout of significant parts of the book.
-->
<xsl:template name="u:book_toc_title">
	<fo:block xsl:use-attribute-sets="chapter_title">
		<xsl:text>Table of Contents</xsl:text>
	</fo:block>
</xsl:template><xsl:template name="u:book_toc">
	<!-- don't include special front/end matter (safeguards, copyright, colophon, etc) -->
	<xsl:for-each select="//document/section[not(@class = 'specialfrontmatter' or @class = 'specialendmatter')]/title">
		<fo:block xsl:use-attribute-sets="book_toc_chapterblock">
			<fo:marker marker-class-name="chapter"><xsl:value-of select="./text()" /></fo:marker>
			<fo:block xsl:use-attribute-sets="book_toc_chapterentry">
				<fo:basic-link internal-destination="{../@id}" xsl:use-attribute-sets="internal_link">
					<xsl:value-of select="./text()" />
					<fo:leader leader-pattern="space" /><fo:page-number-citation ref-id="{../@id}"/>
				</fo:basic-link>
			</fo:block>
			<!-- include major sections within the chapter -->
			<xsl:for-each select="../section/title">
				<fo:block xsl:use-attribute-sets="book_toc_sectionentry">
					<xsl:value-of select="./text()" />
				</fo:block>
			</xsl:for-each>
		</fo:block>
	</xsl:for-each>
</xsl:template>
<xsl:template name="u:not_numbered_chapter_title">
	<fo:marker marker-class-name="chapter"><xsl:value-of select="./title/text()" /></fo:marker>
	<fo:list-block id="{@id}" xsl:use-attribute-sets="chapter_title_block">
		<fo:list-item>
			<fo:list-item-label end-indent="label-end()" />
			<fo:list-item-body start-indent="body-start()">
				<fo:block xsl:use-attribute-sets="chapter_title_right">
					<xsl:value-of select="./title/text()" />
				</fo:block>
			</fo:list-item-body>				
		</fo:list-item>
	</fo:list-block>
</xsl:template><xsl:template name="u:numbered_chapter_title">
	<!-- start of a chapter. fiddlefart around to get nice numbering -->
	<fo:marker marker-class-name="chapter"><xsl:value-of select="title/text()" /></fo:marker>
	<fo:list-block id="{@id}" xsl:use-attribute-sets="chapter_title_block">
		<fo:list-item>
			<fo:list-item-label end-indent="label-end()">
				<fo:block xsl:use-attribute-sets="chapter_title_left">
					<xsl:value-of select="count(preceding-sibling::*[name()='section' and not(@class = 'frontmatter' or @class = 'specialfrontmatter')]) + 1" />
				</fo:block>
			</fo:list-item-label>
			<fo:list-item-body start-indent="body-start()">
				<fo:block xsl:use-attribute-sets="chapter_title_right">
					<xsl:value-of select="title/text()" />
				</fo:block>
			</fo:list-item-body>				
		</fo:list-item>
	</fo:list-block>
</xsl:template><xsl:template name="u:chapter_toc">
	<xsl:for-each select="section/title">
		<fo:block>
			<fo:list-block>
				<fo:list-item>
					<fo:list-item-label end-indent="body-start()">
						<fo:block xsl:use-attribute-sets="chapter_toc_entry">
							<fo:basic-link internal-destination="{../@id}" xsl:use-attribute-sets="internal_link">
								<xsl:value-of select="./text()" />
							</fo:basic-link>
						</fo:block>
					</fo:list-item-label>
					<fo:list-item-body start-indent="body-start()">
						<fo:block xsl:use-attribute-sets="chapter_toc_pagenum">
							<fo:page-number-citation ref-id="{../@id}"/>
						</fo:block>
					</fo:list-item-body>
				</fo:list-item>
			</fo:list-block>
		</fo:block>
	</xsl:for-each>
	<fo:block>
		<fo:leader xsl:use-attribute-sets="chapter-rule" />
	</fo:block>
</xsl:template><xsl:template name="u:list_versions">
	<xsl:for-each select="//document">
		<fo:block>
			<xsl:call-template name="u:printfilename">
				<xsl:with-param name="FName">
					<xsl:call-template name="u:docinfo-filename" />
				</xsl:with-param>
			</xsl:call-template>
			<!-- process all sections/subsections/etc of document -->
			<xsl:for-each select="//section">
				<xsl:call-template name="u:printfilename">
					<xsl:with-param name="FName">
						<xsl:call-template name="u:docinfo-filename" />
					</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
		</fo:block>
	</xsl:for-each>
</xsl:template>

<xsl:template name="u:printfilename">
	<xsl:param name="FName">(filename not known)</xsl:param>

	<!-- discover filename of ancestor -->
	<xsl:variable name="ancestorFName">
		<xsl:call-template name="u:docinfo-filename" >
			<xsl:with-param name="thisNode" select="ancestor::*" />
		</xsl:call-template>
	</xsl:variable>

	<xsl:variable name="level">
		<xsl:value-of select="count(ancestor::section) - count(self::document) + 1" />
	</xsl:variable>

	<xsl:variable name="revision">
		<xsl:call-template name="u:docinfo-revision" />
	</xsl:variable>

	<!-- show top-level sections (chapters) AND ALSO those that are in a separate file -->
	<xsl:if test="($level &lt; 2) or ($FName != $ancestorFName)">
		<fo:block xsl:use-attribute-sets="doc_version_info">

			<!-- indent according to section level -->
			<xsl:attribute name="text-indent"><xsl:value-of select="$level*12" />pt</xsl:attribute>

			<!-- title & filename -->
			<xsl:value-of select="./title/text()" />
			<fo:leader leader-pattern="space" />
			<xsl:value-of select="$FName" />

			<!-- rev if different from ancestor's -->
			<xsl:if test="$FName != $ancestorFName">
				<xsl:if test="string-length($revision) != 0">
					<fo:leader leader-length="1em" />
					<xsl:text>Rev. </xsl:text>
					<xsl:value-of select="$revision" />
				</xsl:if>
			</xsl:if>
		</fo:block>
	</xsl:if>
</xsl:template>

<!--
This branch defines the transformations applied to data that is being flowed 
into a significant sectional structure.
-->
<!--
DocUtils top-level (root) element
-->
<xsl:template match="document">
	<xsl:apply-templates />
</xsl:template>
<!-- 
<xsl:template match="/document/decoration/header" />
<xsl:template match="/document/decoration/footer" />
--><!--
DocUtils generates a bushel of useful document information, applicable only to 
the book as a whole.
Some of this information is displayed on the title page.  The rest is ignored.
-->
<xsl:template match="docinfo" mode="titlepage">
	<!-- the footnote hack ensures that bottom of the docinfo block aligns to the "bottom" of the page -->
	<fo:footnote>
		<fo:inline />
		<fo:footnote-body>
			<fo:block xsl:use-attribute-sets="docinfo_block">
				<xsl:apply-templates mode="titlepage" />
			</fo:block>
		</fo:footnote-body>
	</fo:footnote>
</xsl:template><!-- Condensed XSL follows -->
<xsl:template match="docinfo/address" mode="titlepage">
	<fo:block><xsl:apply-templates /></fo:block></xsl:template>
<xsl:template match="docinfo/author" mode="titlepage">
	<fo:block><xsl:text>Author: </xsl:text><xsl:apply-templates /></fo:block></xsl:template>
<xsl:template match="docinfo/authors" mode="titlepage">
	<fo:block><xsl:text>Authors: </xsl:text><xsl:apply-templates /></fo:block></xsl:template>
<xsl:template match="docinfo/contact" mode="titlepage">
	<fo:block><xsl:text>Contact </xsl:text><xsl:apply-templates /></fo:block></xsl:template>
<xsl:template match="docinfo/copyright" mode="titlepage">
	<fo:block><xsl:text>Copyright </xsl:text><xsl:apply-templates /></fo:block></xsl:template>
<xsl:template match="docinfo/date" mode="titlepage">
	<fo:block><xsl:apply-templates /></fo:block></xsl:template>
<xsl:template match="docinfo/field" mode="titlepage">
	<fo:block><xsl:apply-templates /></fo:block></xsl:template>
<xsl:template match="docinfo/organization" mode="titlepage">
	<fo:block><xsl:apply-templates /></fo:block></xsl:template>
<xsl:template match="docinfo/revision" mode="titlepage">
	<fo:block><xsl:text>Revision </xsl:text><xsl:apply-templates /></fo:block></xsl:template>
<xsl:template match="docinfo/status" mode="titlepage">
	<fo:block><xsl:text>Status: </xsl:text><xsl:apply-templates /></fo:block></xsl:template>
<xsl:template match="docinfo/version" mode="titlepage">
	<fo:block><xsl:text>Version </xsl:text><xsl:apply-templates /></fo:block></xsl:template><xsl:template match="section">
		<xsl:apply-templates />
</xsl:template><xsl:template match="topic" >
	<fo:block xsl:use-attribute-sets="topic_block">
			<xsl:apply-templates select="title" mode="topic" />
			<xsl:apply-templates select="child::*[name()!='title']" />
	</fo:block>
</xsl:template>
<xsl:template match="sidebar">
	<fo:float xsl:use-attribute-sets="sidebar_float">
		<fo:block-container xsl:use-attribute-sets="sidebar_block">
			<fo:block>
				<xsl:apply-templates select="title" mode="sidebar" />
				<xsl:apply-templates select="subtitle" mode="sidebar" />
				<xsl:apply-templates select="child::*[name()!='title' and name()!='subtitle']" />
			</fo:block>
		</fo:block-container>
	</fo:float>
</xsl:template><!--
<xsl:template match="transition" />
--><xsl:template match="*[contains(@class,'review')]">
	<xsl:choose>
		<xsl:when test="($publish_comments='enabled')">
			<fo:block xsl:use-attribute-sets="review_text">
				<xsl:text>PLEASE REVIEW:</xsl:text>
				<xsl:apply-templates />
			</fo:block>
		</xsl:when>
		<xsl:otherwise>
			<xsl:apply-templates />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="admonition" mode="safeguards">
	<fo:inline xsl:use-attribute-sets="generic_matter_title"><xsl:value-of select="title" /></fo:inline>
	<fo:block>
		<xsl:apply-templates select="child::*[name()!='title']" />
	</fo:block>
</xsl:template><!--
Generic admonitions have (had?) (I think) a space between the "admonition" 
class label and the hyphenated-title of the admonition.
TO DO: Get David Goodger to revert it back to the old behaviour.
Admonitions should be able to take a class and a title?
-->
<xsl:template match="admonition">
	<fo:block xsl:use-attribute-sets="admonition_block">
		<fo:block xsl:use-attribute-sets="generic_matter_title">
			<xsl:value-of select="substring-after(@class, ' ')" />:
			<xsl:value-of select="title" />
		</fo:block>
		<xsl:apply-templates select="child::*[name()!='title']" />
	</fo:block>
</xsl:template>

<xsl:template match="attention">
	<fo:block xsl:use-attribute-sets="attention_block">
		<fo:inline xsl:use-attribute-sets="generic_matter_title"><xsl:text>Attention: </xsl:text></fo:inline>
		<xsl:apply-templates />
	</fo:block>
</xsl:template>

<xsl:template match="danger">
	<fo:block xsl:use-attribute-sets="danger_block">
		<fo:inline xsl:use-attribute-sets="generic_matter_title"><xsl:text>Danger: </xsl:text></fo:inline>
		<xsl:apply-templates />
	</fo:block>
</xsl:template>

<xsl:template match="error">
	<fo:block xsl:use-attribute-sets="error_block">
		<fo:inline xsl:use-attribute-sets="generic_matter_title"><xsl:text>Error: </xsl:text></fo:inline>
		<xsl:apply-templates />
	</fo:block>
</xsl:template>

<xsl:template match="hint">
	<fo:block xsl:use-attribute-sets="hint_block">
		<fo:inline xsl:use-attribute-sets="generic_matter_title"><xsl:text>Hint: </xsl:text></fo:inline>
		<xsl:apply-templates />
	</fo:block>
</xsl:template>

<xsl:template match="note">
	<fo:block xsl:use-attribute-sets="note_block">
		<fo:inline xsl:use-attribute-sets="generic_matter_title"><xsl:text>Note: </xsl:text></fo:inline>
		<xsl:apply-templates />
	</fo:block>
</xsl:template>

<xsl:template match="caution">
	<fo:block xsl:use-attribute-sets="admonition_block_with_icon">
		<fo:float xsl:use-attribute-sets="admonition_icon_float">
			<fo:block xsl:use-attribute-sets="admonition_icon_block">
					<fo:external-graphic src="url(file:../Common/caution.png)" />
			</fo:block>
		</fo:float>
		<fo:block xsl:use-attribute-sets="caution_block">
			<fo:inline xsl:use-attribute-sets="generic_matter_title"><xsl:text>Caution: </xsl:text></fo:inline>
			<xsl:apply-templates />
		</fo:block>
	</fo:block>
</xsl:template>
<xsl:template match="important">
	<fo:block xsl:use-attribute-sets="admonition_block_with_icon">
		<fo:float xsl:use-attribute-sets="admonition_icon_float">
			<fo:block xsl:use-attribute-sets="admonition_icon_block">
					<fo:external-graphic src="url(file:../Common/important.png)" />
			</fo:block>
		</fo:float>
		<fo:block xsl:use-attribute-sets="important_block">
			<fo:inline xsl:use-attribute-sets="generic_matter_title"><xsl:text>Important! </xsl:text></fo:inline>
			<xsl:apply-templates />
		</fo:block>
	</fo:block>
</xsl:template><xsl:template match="tip">
	<fo:block xsl:use-attribute-sets="admonition_block_with_icon">
		<fo:float xsl:use-attribute-sets="admonition_icon_float">
			<fo:block xsl:use-attribute-sets="admonition_icon_block">
					<fo:external-graphic src="url(file:../Common/tip.png)" />
			</fo:block>
		</fo:float>
		<fo:block xsl:use-attribute-sets="tip_block">
			<fo:inline xsl:use-attribute-sets="generic_matter_title"><xsl:text>Tip: </xsl:text></fo:inline>
			<xsl:apply-templates />
		</fo:block>
	</fo:block>
</xsl:template><xsl:template match="warning">
	<fo:block xsl:use-attribute-sets="admonition_block_with_icon">
		<fo:float xsl:use-attribute-sets="admonition_icon_float">
			<fo:block xsl:use-attribute-sets="admonition_icon_block">
					<fo:external-graphic src="url(file:../Common/warning.png)" />
			</fo:block>
		</fo:float>
		<fo:block xsl:use-attribute-sets="warning_block">
			<fo:inline xsl:use-attribute-sets="generic_matter_title"><xsl:text>Warning: </xsl:text></fo:inline>
			<xsl:apply-templates />
		</fo:block>
	</fo:block>
</xsl:template><xsl:template match="block_quote">
	<fo:block xsl:use-attribute-sets="block_quote_block">
		<xsl:apply-templates />
	</fo:block>
</xsl:template>
<!--
Captions are labeled as "ch#.fig#: Title" whenever possible
-->
<xsl:template match="caption" mode="figure-pdf">
	<fo:block xsl:use-attribute-sets="generic_text">
		<xsl:text>Figure </xsl:text>
		<xsl:choose>
			<!-- If the chapter is numbered, get the chapter number -->
			<xsl:when test="ancestor::*[name()='section' and not(@class = 'frontmatter' or @class = 'specialfrontmatter')]">
				<xsl:value-of select="count((ancestor::*[last()-1])/preceding-sibling::*[name()='section' and not(@class = 'frontmatter' or @class = 'specialfrontmatter')]) + 1" />
				<xsl:text>.</xsl:text>
				<xsl:number level="any" count="figure[contains(@class,'pdf')]" from="/document/section" />
			</xsl:when>
			<!-- Otherwise, leave off the chapter number -->
			<xsl:otherwise>
				<xsl:number level="any" count="figure[contains(@class,'pdf')]" from="/document/section" />
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text>: </xsl:text>
		<xsl:apply-templates />
	</fo:block>
</xsl:template>
<!--
As these templates are only for FO->PDF transformation, we completely ignore 
all html figures.
-->
<xsl:template match="figure">
	<xsl:choose>
		<xsl:when test="contains(@class,'pdf')">
			<fo:block xsl:use-attribute-sets="figure_block">
				<xsl:if test="contains(@class,'breakbefore')">
					<xsl:attribute name="break-before">page</xsl:attribute>
				</xsl:if>
				<xsl:apply-templates mode="figure-pdf" />
			</fo:block>
		</xsl:when>
		<xsl:when test="contains(@class,'html')">
			<!-- do nothing; it's not for PDF output -->
		</xsl:when>
	</xsl:choose>
</xsl:template>

<xsl:template match="footnote">
	<fo:block id="{@id}" xsl:use-attribute-sets="footnote_block">
		<fo:inline xsl:use-attribute-sets="footnote_ref"><xsl:value-of select="@name" /></fo:inline>
		<xsl:apply-templates />
	</fo:block>
</xsl:template>
<!-- footnote label -->
	<xsl:template match="footnote/label" />
<!-- footnote para -->
	<xsl:template match="footnote/paragraph">
		<xsl:apply-templates />
	</xsl:template>

 <xsl:template match="image" mode="figure-pdf">
 	<fo:block xsl:use-attribute-sets="image_block">
		 <xsl:if test="contains(@class,'fullpage')">
		 	<xsl:attribute name="width">8.5in - 1in</xsl:attribute>
			<xsl:attribute name="height">11in - 2in</xsl:attribute>
			<xsl:attribute name="start-indent">0.25in - inherited-property-value(margin-left)</xsl:attribute>
			<xsl:attribute name="end-indent">0.25in - inherited-property-value(margin-right)</xsl:attribute>
		</xsl:if>
		<fo:external-graphic src="url(file:{@uri})" xsl:use-attribute-sets="external_graphic">
			 <xsl:if test="@width and @height and @scale">
				 <xsl:attribute name="width">
					 <xsl:value-of select="@width div @scale" />
					 <xsl:text>in</xsl:text>
				 </xsl:attribute>
				 <xsl:attribute name="height">
					 <xsl:value-of select="@height div @scale" />
					 <xsl:text>in</xsl:text>
				 </xsl:attribute>
			 </xsl:if>
			 <xsl:if test="contains(@class,'fullpage')">
				<xsl:attribute name="width">100%</xsl:attribute>
				<xsl:attribute name="height">100%</xsl:attribute>
				<xsl:attribute name="display-align">center</xsl:attribute>
			 </xsl:if>
		 </fo:external-graphic>
	</fo:block>
</xsl:template>

 <!-- html image mode, classed as html -->
 <xsl:template match="image" mode="figure-html">
 <!-- do nothing; it's not for PDF output -->
 </xsl:template>

 <!-- ordinary image mode, not a classed image -->
 <xsl:template match="image">
 	<fo:block xsl:use-attribute-sets="image_block">
		<fo:external-graphic src="url(file:{@uri})" xsl:use-attribute-sets="external_graphic">
			<xsl:if test="@width and @height and @scale">
				<xsl:attribute name="width">
					<xsl:value-of select="@width div @scale" /><xsl:text>in</xsl:text>
				</xsl:attribute>
				<xsl:attribute name="height">
					<xsl:value-of select="@height div @scale" /><xsl:text>in</xsl:text>
				</xsl:attribute>
			</xsl:if>
		 </fo:external-graphic>
	</fo:block>
 </xsl:template><!-- The "Swirlie" -->
<xsl:template match="//document/image[@class='spirographic-decoration']" mode="titlepage">
	<fo:block-container xsl:use-attribute-sets="swirlie_position">
		<fo:block>
			<fo:external-graphic src="url(file:{@uri})" />
		</fo:block>
	</fo:block-container>
</xsl:template>

<!-- The Logo -->			
<xsl:template match="//document/image[@class='lcs-logo']" mode="titlepage">
	<fo:block-container xsl:use-attribute-sets="logo_position">
		<fo:block>
			<fo:external-graphic src="url(file:{//document/image[@class='lcs-logo']/@uri})" />
		</fo:block>
	</fo:block-container>
</xsl:template>
<xsl:template match="line_block">
	<fo:block xsl:use-attribute-sets="line_block">
		<xsl:apply-templates />
	</fo:block>
</xsl:template><xsl:template match="bullet_list[not(contains(name(..),'list'))]">
	<!-- is not a list within a list -->
	<fo:list-block xsl:use-attribute-sets="list_block">
		<xsl:apply-templates />
	</fo:list-block>
</xsl:template>

<xsl:template match="bullet_list[contains(name(..),'list')]">
	<!-- is a list within a list -->
	<fo:list-block>
		<xsl:apply-templates />
	</fo:list-block>
</xsl:template>
<xsl:template match="bullet_list/list_item">
	<xsl:variable name="bullet" select="../@bullet" />
	<fo:list-item xsl:use-attribute-sets="list_item">
		<fo:list-item-label end-indent="label-end()">
			<fo:block xsl:use-attribute-sets="bulleted_list_item_label">
				<xsl:choose>
					<xsl:when test="$bullet='-'">
						<xsl:text>&#x2022;</xsl:text>
					</xsl:when>
					<xsl:when test="$bullet='+'">
						<xsl:text>&#x2013;</xsl:text>
					</xsl:when>
					<xsl:when test="$bullet='*'">
						<xsl:text>&#x2219;</xsl:text>
					</xsl:when>
				</xsl:choose>
			</fo:block>
		</fo:list-item-label>
		<fo:list-item-body start-indent="body-start()">
			<fo:block>
				<xsl:apply-templates />
			</fo:block>
		</fo:list-item-body>
	</fo:list-item>
</xsl:template>


<xsl:template match="definition_list">
		<xsl:apply-templates />
</xsl:template>
<xsl:template match="definition_list_item">
	<fo:block xsl:use-attribute-sets="definition_item_block">
		<xsl:apply-templates />
	</fo:block>
</xsl:template><xsl:template match="term">
	<fo:block xsl:use-attribute-sets="definition_term">
		<xsl:apply-templates />
	</fo:block>
</xsl:template><xsl:template match="classifier">
	<fo:block xsl:use-attribute-sets="definition_classifier">
		<xsl:apply-templates />
	</fo:block>
</xsl:template>
<xsl:template match="definition">
	<fo:block xsl:use-attribute-sets="definition_definition">
		<xsl:apply-templates />
	</fo:block>
</xsl:template>
<xsl:template match="enumerated_list[not(contains(name(..),'list'))]">
	<!-- is not a list within a list -->
	<fo:list-block xsl:use-attribute-sets="list_block">
		<xsl:apply-templates />
	</fo:list-block>
</xsl:template>

<xsl:template match="enumerated_list[(contains(name(..),'list'))]">
	<!-- is a list within a list -->
	<fo:list-block xsl:use-attribute-sets="list_item">
		<xsl:apply-templates />
	</fo:list-block>
</xsl:template><xsl:template match="enumerated_list/list_item">
	<fo:list-item xsl:use-attribute-sets="list_item">
		<fo:list-item-label end-indent="label-end()">
			<fo:block>
				<xsl:choose>
					<xsl:when test="../@enumtype = 'arabic'">
						<xsl:number format="1" /><xsl:text>.</xsl:text>
					</xsl:when>
					<xsl:when test="../@enumtype = 'loweralpha'">
						<xsl:number format="a" /><xsl:text>.</xsl:text>
					</xsl:when>
					<xsl:when test="../@enumtype = 'upperalpha'">
						<xsl:number format="A" /><xsl:text>.</xsl:text>
					</xsl:when>
					<xsl:when test="../@enumtype = 'lowerroman'">
						<xsl:number format="i" /><xsl:text>.</xsl:text>
					</xsl:when>
					<xsl:when test="../@enumtype = 'upperroman'">
						<xsl:number format="I" /><xsl:text>.</xsl:text>
					</xsl:when>
					<xsl:otherwise>
						<xsl:text>unrecognized enumtype</xsl:text>
						<xsl:number /><xsl:text>.</xsl:text>
					</xsl:otherwise>
				</xsl:choose>
			</fo:block>
		</fo:list-item-label>
		<fo:list-item-body start-indent="body-start()">
			<fo:block>
				<xsl:apply-templates />
			</fo:block>
		</fo:list-item-body>
	</fo:list-item>
</xsl:template><!--
Field lists may be used by other templates for author-defined parameters; they 
are never output directly.
-->
<xsl:template match="field_list" /><xsl:template match="field" />
<xsl:template match="literal_block">
	<fo:block xsl:use-attribute-sets="literal_block">
		<xsl:apply-templates />
	</fo:block>
</xsl:template>

<xsl:template match="paragraph">
	<fo:block xsl:use-attribute-sets="generic_text">
		<xsl:apply-templates />
	</fo:block>
</xsl:template>

<xsl:template match="thead/row/entry/paragraph">
	<!-- table headers are special -->
	<fo:block xsl:use-attribute-sets="table_head">
		<xsl:apply-templates />
	</fo:block>
</xsl:template>

<xsl:template match="tbody/row/entry/paragraph[1]">
	<!-- table cells don't need the extra space above -->
	<fo:block xsl:use-attribute-sets="table_entry_1stparagraph">
		<xsl:apply-templates />
	</fo:block>
</xsl:template>

<!-- BODY: table -->
<!-- to be tweaked: base table width on sum of column widths
<xsl:variable name="twidth">
	<xsl:value-of select="sum(. group/colspec/@colwidth)*.66" />
</xsl:variable>
<fo:table xsl:use-attribute-sets="table_block" width="{$twidth}em"> -->
<xsl:template match="table">
	<fo:table xsl:use-attribute-sets="table_block">
		<xsl:apply-templates />
	</fo:table>
</xsl:template>

<xsl:template match="tgroup">
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="colspec">
	<fo:table-column column-width="proportional-column-width({@colwidth})">
		<xsl:apply-templates />
	</fo:table-column>
</xsl:template><xsl:template match="thead">
	<fo:table-header xsl:use-attribute-sets="table_head">
		<xsl:apply-templates />
	</fo:table-header>
</xsl:template>

<xsl:template match="thead/row">
	<fo:table-row xsl:use-attribute-sets="table_row">
		<xsl:apply-templates />
	</fo:table-row>
</xsl:template>

<xsl:template match="thead/row/entry">
	<fo:table-cell xsl:use-attribute-sets="table_entry">
		<xsl:apply-templates />
	</fo:table-cell>
</xsl:template><xsl:template match="tbody">
	<fo:table-body xsl:use-attribute-sets="table_body">
		<xsl:apply-templates />
	</fo:table-body>
</xsl:template>

<xsl:template match="tbody/row[position() mod 2 = 0]">
	<fo:table-row background-color="{$even_row-color}" xsl:use-attribute-sets="table_row">
		<xsl:apply-templates />
	</fo:table-row>
</xsl:template>
<xsl:template match="tbody/row[position() mod 2 != 0]">
	<fo:table-row background-color="{$odd_row-color}" xsl:use-attribute-sets="table_row">
		<xsl:apply-templates />
	</fo:table-row>
</xsl:template>

<xsl:template match="entry">
	<fo:table-cell xsl:use-attribute-sets="table_entry">
		<xsl:apply-templates />
	</fo:table-cell>
</xsl:template><!--
TO DO: targets always target the next element; copy the target id to the 
target element
(change the references to use "contains" when this is done)
//target/following-sibling::*[1]
NOTE: Won't work!  Targets appearing before a section title are embedded in 
the previous section; no next sibling to copy to!
-->
<xsl:template match="target">
	<fo:block id="{@id}">
		<xsl:apply-templates />
	</fo:block>
</xsl:template>
<xsl:template match="comment">
	<xsl:variable name="is_filenotes">
		<xsl:value-of select="contains(text(),'file notes')" />
	</xsl:variable>
	<xsl:choose>
		<xsl:when test="($publish_filenotes = 'enabled') and ($is_filenotes = 'true')">
			<fo:block xsl:use-attribute-sets="comment">
				<xsl:text>File Notes: </xsl:text>
				<xsl:apply-templates />
			</fo:block>
		</xsl:when>
		<xsl:when test="($publish_comments = 'enabled') and ($is_filenotes = 'false')">
			<fo:block xsl:use-attribute-sets="comment">
				<xsl:text>Comment: </xsl:text>
				<xsl:apply-templates />
			</fo:block>
		</xsl:when>
	</xsl:choose>
</xsl:template>

<!--
DocUtils tells us the definitions for the substitution references even though 
it actually performs the substitution, too. Throw it away!
-->
<xsl:template match="substitution_definition" />

<!--
We make *no* use of DocUtils' self-generated title content
-->
<xsl:template match="title/generated" />
<!--
Shown only for Debug-Style PDFs when 'publish_problematics' is 'enabled'
-->
<xsl:template match="problematic">
	<xsl:if test="$publish_problematics = 'enabled'">
		<fo:block xsl:use-attribute-sets="problem">
			<xsl:text>DocUtils Problematic: </xsl:text>
			<xsl:apply-templates />
		</fo:block>
	</xsl:if>
</xsl:template>
<xsl:template match="system_message">
	<xsl:if test="$system_messages = 'enabled'">
		<fo:block xsl:use-attribute-sets="system_message">
			<xsl:text>DocUtils Warning:</xsl:text>
			<xsl:apply-templates />
		</fo:block>
	</xsl:if>
</xsl:template>

<xsl:template match="emphasis">
	<fo:inline xsl:use-attribute-sets="emphasis"><xsl:apply-templates /></fo:inline>
</xsl:template>

<!--
The (nominally superscripted) number or symbol placed in-line to indicate a 
footnote reference
-->
<xsl:template match="footnote_reference">
	<fo:basic-link internal-destination="{@refid}" xsl:use-attribute-sets="internal_link">
		<fo:inline xsl:use-attribute-sets="footnote_ref"><xsl:value-of select="@refid" /></fo:inline>
	</fo:basic-link>
</xsl:template>

<!--
Note there are also image blocks, which are another thing entirely.
-->
<xsl:template match="paragraph/image">
	<fo:external-graphic src="url(file:{@uri})" />
</xsl:template>

<!--
Note there are also literal and list blocks, which are another thing entirely.
-->
<xsl:template match="literal">
	<fo:inline xsl:use-attribute-sets="literal_text"><xsl:apply-templates /></fo:inline>
</xsl:template>


<xsl:key name="sectionkey" match="section" use="@id" />
<xsl:key name="targetkey" match="target" use="@id" />

<!--
Anonymous internal references do not have page numbers??
TO DO: figure out what this was all about originally; check if DocUtils 
correctly places title targets these days
NOTE: anonymous attribute is now missing?!?
-->
<xsl:template match="reference[@refid and @anonymous]">
	<xsl:choose>
		<!--
		When referencing a section (chapter or chapter section), use the true 
		section title itself.
		-->
		<xsl:when test="key('sectionkey', @refid)">
			<fo:inline xsl:use-attribute-sets="internal_link"><xsl:apply-templates /></fo:inline>
		</xsl:when>
		
		<!--
		When the target is at the end of a section, and immediately following 
		that is a new section, use that new section's title.
		This is necessary because DocUtils doesn't look ahead for a title
		-->
		<xsl:when test="((key('targetkey', @refid)/following::*)[1])[name()='section']">
			<fo:basic-link internal-destination="{(key('targetkey', @refid)/following::*[name()='section'])[1]/@id}" xsl:use-attribute-sets="internal_link">
				<fo:inline xsl:use-attribute-sets="internal_link"><xsl:apply-templates /></fo:inline>
			</fo:basic-link>
		</xsl:when>
		<!--
		When the target immediately follows a title, use that title's text.
		This was to allow authors to hack around the target-preceding problem 
		described above.
		-->
		<xsl:when test="key('targetkey', @refid)/preceding-sibling::*[name()='title']">
			<fo:basic-link internal-destination="{key('targetkey', @refid)/../@id}" xsl:use-attribute-sets="internal_link">
				<fo:inline xsl:use-attribute-sets="internal_link"><xsl:apply-templates /></fo:inline>
			</fo:basic-link>
		</xsl:when>
		<xsl:otherwise>
			<!-- use the text provided by the reference; may not actually match the target's text -->
			<fo:inline xsl:use-attribute-sets="internal_link"><xsl:apply-templates /></fo:inline>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>
<!-- not-anonymous internal reference -->
<xsl:template match="reference[@refid and not(@anonymous)]">
	<xsl:choose>
		<!--
		When referencing a section (chapter or chapter section), use the true 
		section title itself.
		-->
		<xsl:when test="key('sectionkey', @refid)">
			<fo:inline xsl:use-attribute-sets="internal_link"><xsl:apply-templates select="key('sectionkey', @refid)/title[1]" mode="xref" /></fo:inline>
		</xsl:when>
		
		<!--
		When the target is at the end of a section, and immediately following 
		that is a new section, use that new section's title.
		This is necessary because DocUtils doesn't look ahead for a title
		-->
		<xsl:when test="((key('targetkey', @refid)/following::*)[1])[name()='section']">
			<fo:basic-link internal-destination="{(key('targetkey', @refid)/following::*[name()='section'])[1]/@id}" xsl:use-attribute-sets="internal_link">
				<fo:inline xsl:use-attribute-sets="internal_link"><xsl:apply-templates select="(key('targetkey', @refid)/following::*[name()='section'])[1]/title[1]" mode="xref" /></fo:inline>
			</fo:basic-link>
		</xsl:when>
		
		<!--
		When the target immediately follows a title, use that title's text.
		This was to allow authors to hack around the target-preceding problem 
		described above.
		-->
		<xsl:when test="key('targetkey', @refid)/preceding-sibling::*[name()='title']">
			<fo:basic-link internal-destination="{key('targetkey', @refid)/../@id}" xsl:use-attribute-sets="internal_link">
				<fo:inline xsl:use-attribute-sets="internal_link"><xsl:apply-templates select="key('targetkey', @refid)/preceding-sibling::*[name()='title']" mode="xref" /></fo:inline>
			</fo:basic-link>
		</xsl:when>
		
		<xsl:otherwise>
			<!-- use the text provided by the reference; may not actually match the target's text -->
			<fo:inline xsl:use-attribute-sets="internal_link"><xsl:apply-templates /></fo:inline>
		</xsl:otherwise>
	</xsl:choose>	
	<xsl:text> (p. </xsl:text>
	<xsl:choose>
		<!-- use the target's title text content -->
		<xsl:when test="key('targetkey', @refid)/preceding-sibling::*[name()='title']">
			<fo:page-number-citation ref-id="{key('targetkey', @refid)/../@id}" />
		</xsl:when>
		<xsl:otherwise>
			<fo:page-number-citation ref-id="{@refid}" />
		</xsl:otherwise>
	</xsl:choose>
	<xsl:text>)</xsl:text>
</xsl:template>
<xsl:template match="reference[@refuri]">
	<fo:basic-link external-destination="url({@refuri})">
		<fo:inline xsl:use-attribute-sets="external_link">
			<xsl:apply-templates />
<!--			<xsl:call-template name="u:make-breakable">
				<xsl:with-param name="string" select="text()" />
			</xsl:call-template> -->
		</fo:inline>
	</fo:basic-link>
</xsl:template>
<!-- unknown reference type -->
<xsl:template match="reference">
	<fo:inline xsl:use-attribute-sets="reference"><xsl:apply-templates /></fo:inline>
</xsl:template><xsl:template match="strong">
	<fo:inline font-weight="bold"><xsl:apply-templates /></fo:inline>
</xsl:template><xsl:template match="subscript">
	<fo:inline baseline-shift="sub"><xsl:apply-templates /></fo:inline>
</xsl:template>
<xsl:template match="subtitle" mode="sidebar">
	<fo:block>
		<fo:inline font-weight="bold"><xsl:value-of select="text()" /></fo:inline>
	</fo:block>
</xsl:template>



<xsl:template match="subtitle" mode="titlepage">
	<fo:block-container xsl:use-attribute-sets="sub_title_position">
		<fo:block xsl:use-attribute-sets="sub_title_text">
			<xsl:apply-templates />
		</fo:block>
	</fo:block-container>
</xsl:template>
<xsl:template match="superscript">
	<fo:inline baseline-shift="super"><xsl:apply-templates /></fo:inline>
</xsl:template>

<!--
Never output to PDF; used only for reference links.
-->
<xsl:template match="section/title">
	<xsl:variable name="level" select="count(ancestor::section)" />
	<xsl:choose>
		<xsl:when test="$level = 1">
			<!-- special case, handled by the chapter first-page template -->
		</xsl:when>
		<xsl:when test="$level = 2">
			<fo:block id="{../@id}" xsl:use-attribute-sets="title-level1">
				<xsl:apply-templates />
			</fo:block>
		</xsl:when>
		<xsl:when test="$level = 3">
			<fo:block id="{../@id}" xsl:use-attribute-sets="title-level2">
				<xsl:apply-templates />
			</fo:block>
		</xsl:when>
		<xsl:when test="$level = 4">
			<fo:block id="{../@id}" xsl:use-attribute-sets="title-level3">
				<xsl:apply-templates />
			</fo:block>
		</xsl:when>
		<xsl:otherwise>
			<fo:block id="{../@id}" xsl:use-attribute-sets="title-level4up">
				<xsl:apply-templates />
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="title" mode="colophon">
	<fo:block id="{../@id}" xsl:use-attribute-sets="generic_matter_title">
		<fo:marker marker-class-name="chapter"><xsl:value-of select="text()" /></fo:marker>
		<xsl:value-of select="text()" />
	</fo:block>
</xsl:template>

<xsl:template match="title" mode="copyright">
	<fo:block id="{../@id}" xsl:use-attribute-sets="generic_matter_title">
		<fo:marker marker-class-name="chapter"><xsl:value-of select="text()" /></fo:marker>
		<xsl:value-of select="text()" />
	</fo:block>
</xsl:template>
<xsl:template match="title" mode="safeguards">
	<fo:block id="{../@id}" xsl:use-attribute-sets="safeguards_title">
		<fo:marker marker-class-name="chapter"><xsl:value-of select="text()" /></fo:marker>
		<xsl:value-of select="text()" />
	</fo:block>
</xsl:template><xsl:template match="title" mode="sidebar">
	<fo:block id="{../@id}" xsl:use-attribute-sets="generic_matter_title">
		<xsl:value-of select="text()" />
	</fo:block>
</xsl:template>


<xsl:template match="title" mode="titlepage">
	<fo:block-container xsl:use-attribute-sets="main_title_position">
		<fo:block xsl:use-attribute-sets="main_title_text">
			<xsl:apply-templates />
		</fo:block>
	</fo:block-container>
</xsl:template>
<xsl:template match="title" mode="topic">
	<fo:block id="{../@id}" xsl:use-attribute-sets="generic_matter_title">
		<xsl:value-of select="text()" />
	</fo:block>
</xsl:template>



<xsl:template match="title" mode="versions">
	<fo:block id="{../@id}" xsl:use-attribute-sets="generic_matter_title">
		<fo:marker marker-class-name="chapter"><xsl:value-of select="text()" /></fo:marker>
		<xsl:value-of select="text()" />
	</fo:block>
</xsl:template>
<!--
Used in the reference templates; a placeholder should one want to do anything 
interesting with the text used in the reference.
Note that the actual link face formatting is handled in the reference 
template.
-->
<xsl:template match="title" mode="xref">
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="title_reference">
	<fo:inline xsl:use-attribute-sets="sans_italic_face"><xsl:apply-templates /></fo:inline>
</xsl:template>

<xsl:template match="ui_shortcut">
	<fo:inline xsl:use-attribute-sets="mono_bold_face"><xsl:apply-templates /></fo:inline>
</xsl:template>
<xsl:template match="ui_guilabel">
	<fo:inline xsl:use-attribute-sets="sans_bold_face"><xsl:apply-templates /></fo:inline>
</xsl:template>
<xsl:template match="ui_command">
	<fo:inline xsl:use-attribute-sets="sans_bold_face"><xsl:apply-templates /></fo:inline>
</xsl:template><!-- show ancestors of current node -->
<xsl:template name="u:an">
	<xsl:variable name="full-path">
		<xsl:for-each select="ancestor-or-self::*">
			<xsl:value-of select="concat('/',name())" />
		</xsl:for-each>
	</xsl:variable>
	<xsl:message><xsl:value-of select="$full-path" /></xsl:message>
</xsl:template>

<!-- show all children of current node -->
<xsl:template name="u:ch">
	<xsl:for-each select="descendant::*">
		<xsl:call-template name="u:ch" />
	</xsl:for-each>
	<xsl:if test="count(child::*) = 0">
		<xsl:call-template name="u:an" />
	</xsl:if>
</xsl:template>

<!-- show immediate children of current node -->
<xsl:template name="u:ch1">
	<xsl:for-each select="*">
		<xsl:call-template name="u:an" />
	</xsl:for-each>
</xsl:template><!-- return filename for document containing the current node -->
<xsl:template name="u:docinfo-filename">
	<xsl:param name="thisNode" select="self::*"/>
	<xsl:variable name="fname">
		<xsl:value-of select="($thisNode/ancestor-or-self::*/field_list[@class='docinfo']/field/field_body/paragraph[contains(text(),'$RCSfile:')]/text())[last()]" />
	</xsl:variable>
	<xsl:value-of select="normalize-space(substring-before(substring-after($fname,'RCSfile: '), ',v $'))" />
</xsl:template>

<!-- return docinfo date -->
<xsl:template name="u:docinfo-date">
	<xsl:param name="whichNodes">self</xsl:param>
	<xsl:variable name="date">
		<xsl:choose>
			<xsl:when test="$whichNodes=self">
				<!-- just the current node's docinfo -->
				<xsl:value-of select="./field_list[@class='docinfo']/field/field_body/paragraph[contains(text(),'$Date:')]/text()" />
			</xsl:when>
			<xsl:otherwise>
				<!-- find the most-recent from all the children -->
				<!-- dammit, this DOES NOT WORK -->
				<xsl:for-each select="descendant-or-self::*/field_list[@class='docinfo']/field/field_body/paragraph[contains(text(),'$Date:')]/text()" >
					<xsl:sort select="." data-type="number" order="descending" />
					<xsl:if test="position()=1">
						<xsl:copy-of select="." />
					</xsl:if>
				</xsl:for-each>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:value-of select="normalize-space(substring-before(substring-after($date,'Date: '), ' $'))" />
</xsl:template>

<!-- return docinfo revision -->
<xsl:template name="u:docinfo-revision">
	<xsl:param name="whichNodes">self</xsl:param>
	<xsl:variable name="rev">
		<xsl:choose>
			<!-- just the current node's docinfo -->
			<xsl:when test="$whichNodes = 'self'">
				<xsl:value-of select="./field_list[@class='docinfo']/field/field_body/paragraph[contains(text(),'$Revision:')]/text()" />
			</xsl:when>
			<xsl:otherwise>
				<!-- find the most-recent from all the children -->
				<!-- dammit, this DOES NOT WORK -->
				<xsl:for-each select="descendant-or-self::*/field_list[@class='docinfo']/field/field_body/paragraph[contains(text(),'$Revision:')]/text()" >
					<xsl:sort select="." data-type="number" order="descending" />
					<xsl:if test="position()=1">
						<xsl:value-of select="." />
					</xsl:if>
				</xsl:for-each>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:value-of select="normalize-space(substring-before(substring-after($rev,'Revision: '), ' $'))" />
</xsl:template>

<!-- turn slashes into line-breakable slashes -->
<xsl:template name="u:make-breakable">
	<xsl:param name="string" />
	<xsl:variable name="breakables">
		&#x0021;&#x0022;&#x0023;&#x0024;&#x0025;&#x0026;&#x0027;
		&#x0028;&#x0029;&#x002A;&#x002B;&#x002C;&#x002D;&#x002E;
		&#x002F;&#x003A;&#x003B;&#x003C;&#x003D;&#x003E;&#x003F;
		&#x0040;&#x005B;&#x005C;&#x005D;&#x005E;&#x005F;&#x0060;
	</xsl:variable>

	<xsl:if test="string-length($string) &gt; 0">
		<xsl:variable name="c1" select="substring($string, 1, 1)" />
		<xsl:choose>
			<xsl:when test="contains($breakables, $c1)">
				<fo:character treat-as-word-space="true">
					<xsl:attribute name="character">
						<xsl:value-of select="$c1" />
					</xsl:attribute>
				</fo:character>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$c1" />
			</xsl:otherwise>
		</xsl:choose>

		<xsl:call-template name="u:make-breakable">
			<xsl:with-param name="string" select="substring($string, 2)"/>
		</xsl:call-template>
	</xsl:if>
</xsl:template>
<!-- A DISCARD FUNCTION -->
<xsl:template match="*" mode="discard">
	<xsl:message>
		<xsl:text>Discarded: </xsl:text>
		<xsl:value-of select="text()" />
	</xsl:message>
</xsl:template>

<!-- REPORT UNKNOWN TAGS -->
<xsl:template match="*">
	<xsl:message>
		<xsl:value-of select="name(.)" />
		<xsl:text> encountered</xsl:text>
		<xsl:if test="parent::*">
			<xsl:text> in </xsl:text>
			<xsl:value-of select="name(parent::*)" />
		</xsl:if>
		<xsl:text>, but no template matches.</xsl:text>
	</xsl:message>
	<!-- highlight in red in the output -->
	<fo:block >
		<xsl:text>&lt;</xsl:text>
		<xsl:value-of select="name(.)" />
		<xsl:text>&gt;</xsl:text>
		<xsl:apply-templates />
		<xsl:text>&lt;/</xsl:text>
		<xsl:value-of select="name(.)" />
		<xsl:text>&gt;</xsl:text>
	</fo:block>
</xsl:template>

</xsl:stylesheet>



