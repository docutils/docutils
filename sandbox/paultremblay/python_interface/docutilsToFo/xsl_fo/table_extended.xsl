<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id$ -->


     <xsl:attribute-set name="table1-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table1">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table1-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table1-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table1-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table1-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table1-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table1-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table1-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell1-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table2-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table2">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table2-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table2-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table2-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table2-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table2-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table2-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table2-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell2-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table3-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table3">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table3-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table3-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table3-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table3-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table3-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table3-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table3-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell3-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table4-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table4">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table4-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table4-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table4-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table4-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table4-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table4-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table4-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell4-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table5-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table5">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table5-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table5-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table5-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table5-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table5-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table5-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table5-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell5-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table6-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table6">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table6-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table6-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table6-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table6-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table6-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table6-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table6-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell6-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table7-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table7">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table7-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table7-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table7-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table7-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table7-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table7-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table7-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell7-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table8-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table8">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table8-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table8-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table8-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table8-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table8-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table8-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table8-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell8-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table9-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table9">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table9-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table9-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table9-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table9-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table9-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table9-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table9-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell9-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table10-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table10">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table10-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table10-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table10-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table10-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table10-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table10-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table10-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell10-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table11-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table11">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table11-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table11-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table11-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table11-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table11-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table11-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table11-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell11-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table12-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table12">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table12-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table12-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table12-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table12-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table12-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table12-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table12-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell12-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table13-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table13">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table13-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table13-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table13-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table13-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table13-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table13-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table13-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell13-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table14-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table14">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table14-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table14-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table14-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table14-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table14-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table14-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table14-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell14-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table15-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table15">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table15-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table15-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table15-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table15-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table15-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table15-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table15-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell15-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table16-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table16">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table16-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table16-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table16-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table16-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table16-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table16-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table16-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell16-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table17-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table17">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table17-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table17-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table17-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table17-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table17-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table17-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table17-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell17-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table18-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table18">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table18-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table18-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table18-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table18-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table18-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table18-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table18-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell18-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table19-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table19">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table19-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table19-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table19-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table19-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table19-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table19-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table19-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell19-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table20-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table20">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table20-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table20-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table20-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table20-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table20-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table20-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table20-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell20-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table21-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table21">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table21-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table21-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table21-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table21-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table21-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table21-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table21-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell21-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table22-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table22">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table22-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table22-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table22-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table22-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table22-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table22-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table22-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell22-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table23-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table23">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table23-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table23-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table23-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table23-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table23-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table23-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table23-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell23-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table24-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table24">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table24-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table24-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table24-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table24-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table24-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table24-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table24-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell24-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table25-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table25">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table25-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table25-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table25-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table25-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table25-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table25-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table25-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell25-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table26-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table26">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table26-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table26-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table26-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table26-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table26-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table26-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table26-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell26-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table27-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table27">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table27-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table27-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table27-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table27-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table27-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table27-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table27-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell27-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table28-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table28">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table28-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table28-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table28-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table28-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table28-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table28-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table28-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell28-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table29-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table29">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table29-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table29-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table29-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table29-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table29-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table29-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table29-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell29-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table30-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table30">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table30-header">
    </xsl:attribute-set>

     <xsl:attribute-set name="table30-header-cell">
    </xsl:attribute-set>

     <xsl:attribute-set name="table30-header-block">
    </xsl:attribute-set>

     <xsl:attribute-set name="table30-body">
    </xsl:attribute-set>

     <xsl:attribute-set name="table30-header-row">
    </xsl:attribute-set>

     <xsl:attribute-set name="table30-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>

     <xsl:attribute-set name="table30-cell" >
    </xsl:attribute-set>

     <xsl:attribute-set name="cell30-block">
    </xsl:attribute-set>

     <!--END OF ATTRIBUTE SETS-->

     <xsl:template name="make-fo-table-cols">
         <xsl:param name="cols-string"/>
         <xsl:param name="col-num">1</xsl:param>
         <xsl:variable name="before-comma" 
            select="normalize-space(substring-before($cols-string, ','))" />
         <xsl:variable name="col-width">
             <xsl:choose>
                 <xsl:when test="$before-comma">
                     <xsl:value-of select="$before-comma"/>
                 </xsl:when>
                 <xsl:otherwise>
                     <xsl:value-of select="$cols-string"/>
                 </xsl:otherwise>
             </xsl:choose>
         </xsl:variable>
         <xsl:choose>
             <xsl:when test="$col-width != ''">
                <fo:table-column column-number="{$col-num}" 
                    column-width="proportional-column-width({$col-width})"/>
                <xsl:call-template name="make-fo-table-cols">
                    <xsl:with-param name="cols-string" 
                        select= "normalize-space(substring-after($cols-string, ','))"/>
                    <xsl:with-param name="col-num" select="$col-num + 1"/>
                </xsl:call-template>
             </xsl:when>
         </xsl:choose>
    </xsl:template>

     <xsl:template name="get-column-widths">
        <xsl:param name="classes"/>
        <xsl:variable name="columns">
            <xsl:choose>
                 <xsl:when test="$classes = 'table1'">
                    <xsl:value-of select="$table1-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table2'">
                    <xsl:value-of select="$table2-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table3'">
                    <xsl:value-of select="$table3-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table4'">
                    <xsl:value-of select="$table4-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table5'">
                    <xsl:value-of select="$table5-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table6'">
                    <xsl:value-of select="$table6-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table7'">
                    <xsl:value-of select="$table7-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table8'">
                    <xsl:value-of select="$table8-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table9'">
                    <xsl:value-of select="$table9-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table10'">
                    <xsl:value-of select="$table10-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table11'">
                    <xsl:value-of select="$table11-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table12'">
                    <xsl:value-of select="$table12-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table13'">
                    <xsl:value-of select="$table13-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table14'">
                    <xsl:value-of select="$table14-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table15'">
                    <xsl:value-of select="$table15-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table16'">
                    <xsl:value-of select="$table16-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table17'">
                    <xsl:value-of select="$table17-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table18'">
                    <xsl:value-of select="$table18-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table19'">
                    <xsl:value-of select="$table19-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table20'">
                    <xsl:value-of select="$table20-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table21'">
                    <xsl:value-of select="$table21-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table22'">
                    <xsl:value-of select="$table22-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table23'">
                    <xsl:value-of select="$table23-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table24'">
                    <xsl:value-of select="$table24-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table25'">
                    <xsl:value-of select="$table25-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table26'">
                    <xsl:value-of select="$table26-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table27'">
                    <xsl:value-of select="$table27-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table28'">
                    <xsl:value-of select="$table28-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table29'">
                    <xsl:value-of select="$table29-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'table30'">
                    <xsl:value-of select="$table30-cols"/>
                </xsl:when>

                 <xsl:when test="$classes = 'borderless'">
                       <xsl:value-of select="$table-borderless-cols"/>
                  </xsl:when> 

                  <xsl:when test="$classes = 'long'">
                       <xsl:value-of select="$table-long-cols"/>
                  </xsl:when> 

                  <xsl:when test="$classes = ''">
                       <xsl:value-of select="$table-cols"/>
                  </xsl:when> 

             </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="$columns"/>
    </xsl:template>

     
    <xsl:template name="make-col-specs">
        <xsl:param name="classes"/>
        <xsl:variable name="columns">
            <xsl:call-template name = "get-column-widths">
                <xsl:with-param name="classes" select = "$classes"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$columns != ''">
                <xsl:call-template name="make-fo-table-cols">
                    <xsl:with-param name="cols-string" select="$columns"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates select="tgroup/colspec" mode="use">
                    <xsl:with-param name="classe" select="$classes"/>
                </xsl:apply-templates>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="cols-spanned">
        <xsl:choose>
            <xsl:when test="@morecols">
                <xsl:value-of select="@morecols + 1"/>
            </xsl:when>
            <xsl:otherwise>1</xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="rows-spanned">
        <xsl:choose>
            <xsl:when test="@morerows">
                <xsl:value-of select="@morerows + 1"/>
            </xsl:when>
            <xsl:otherwise>1</xsl:otherwise>
        </xsl:choose>
    </xsl:template>

     <xsl:template match="tgroup/colspec" mode="use">
        <xsl:variable name="col-num">
            <xsl:number/>
        </xsl:variable>
        <fo:table-column column-number="{$col-num}" 
            column-width="proportional-column-width({@colwidth})"/>
     </xsl:template>

     <xsl:template match="tgroup" >
         <xsl:param name="classes"/>
         <xsl:apply-templates >
             <xsl:with-param name="classes" select="$classes"/>
         </xsl:apply-templates>
    </xsl:template>

   
     <xsl:template match="table[@classes='table1']">
        <fo:block-container xsl:use-attribute-sets = "table1-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table1" xsl:use-attribute-sets="table1">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table1']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table1-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table1-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table1-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table1-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table1-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table1-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table1-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table1-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table1-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table1-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table1']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell1-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table2']">
        <fo:block-container xsl:use-attribute-sets = "table2-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table2" xsl:use-attribute-sets="table2">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table2']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table2-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table2']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table2-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table2']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table2-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table2']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table2-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table2']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table2-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table2']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table2-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table2']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table2-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table2-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table2-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table2-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table2']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell2-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table3']">
        <fo:block-container xsl:use-attribute-sets = "table3-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table3" xsl:use-attribute-sets="table3">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table3']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table3-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table3']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table3-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table3']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table3-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table3']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table3-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table3']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table3-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table3']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table3-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table3']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table3-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table3-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table3-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table3-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table3']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell3-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table4']">
        <fo:block-container xsl:use-attribute-sets = "table4-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table4" xsl:use-attribute-sets="table4">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table4']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table4-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table4']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table4-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table4']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table4-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table4']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table4-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table4']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table4-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table4']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table4-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table4']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table4-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table4-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table4-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table4-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table4']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell4-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table5']">
        <fo:block-container xsl:use-attribute-sets = "table5-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table5" xsl:use-attribute-sets="table5">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table5']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table5-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table5']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table5-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table5']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table5-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table5']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table5-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table5']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table5-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table5']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table5-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table5']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table5-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table5-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table5-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table5-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table5']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell5-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table6']">
        <fo:block-container xsl:use-attribute-sets = "table6-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table6" xsl:use-attribute-sets="table6">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table6']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table6-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table6']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table6-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table6']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table6-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table6']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table6-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table6']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table6-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table6']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table6-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table6']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table6-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table6-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table6-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table6-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table6']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell6-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table7']">
        <fo:block-container xsl:use-attribute-sets = "table7-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table7" xsl:use-attribute-sets="table7">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table7']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table7-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table7']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table7-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table7']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table7-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table7']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table7-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table7']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table7-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table7']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table7-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table7']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table7-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table7-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table7-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table7-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table7']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell7-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table8']">
        <fo:block-container xsl:use-attribute-sets = "table8-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table8" xsl:use-attribute-sets="table8">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table8']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table8-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table8']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table8-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table8']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table8-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table8']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table8-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table8']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table8-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table8']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table8-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table8']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table8-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table8-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table8-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table8-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table8']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell8-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table9']">
        <fo:block-container xsl:use-attribute-sets = "table9-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table9" xsl:use-attribute-sets="table9">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table9']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table9-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table9']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table9-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table9']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table9-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table9']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table9-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table9']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table9-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table9']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table9-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table9']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table9-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table9-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table9-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table9-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table9']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell9-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table10']">
        <fo:block-container xsl:use-attribute-sets = "table10-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table10" xsl:use-attribute-sets="table10">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table10']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table10-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table10']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table10-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table10']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table10-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table10']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table10-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table10']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table10-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table10']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table10-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table10']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table10-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table10-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table10-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table10-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table10']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell10-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table11']">
        <fo:block-container xsl:use-attribute-sets = "table11-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table11" xsl:use-attribute-sets="table11">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table11']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table11-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table11']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table11-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table11']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table11-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table11']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table11-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table11']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table11-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table11']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table11-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table11']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table11-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table11-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table11-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table11-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table11']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell11-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table12']">
        <fo:block-container xsl:use-attribute-sets = "table12-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table12" xsl:use-attribute-sets="table12">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table12']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table12-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table12']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table12-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table12']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table12-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table12']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table12-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table12']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table12-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table12']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table12-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table12']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table12-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table12-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table12-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table12-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table12']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell12-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table13']">
        <fo:block-container xsl:use-attribute-sets = "table13-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table13" xsl:use-attribute-sets="table13">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table13']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table13-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table13']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table13-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table13']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table13-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table13']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table13-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table13']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table13-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table13']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table13-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table13']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table13-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table13-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table13-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table13-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table13']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell13-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table14']">
        <fo:block-container xsl:use-attribute-sets = "table14-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table14" xsl:use-attribute-sets="table14">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table14']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table14-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table14']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table14-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table14']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table14-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table14']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table14-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table14']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table14-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table14']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table14-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table14']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table14-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table14-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table14-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table14-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table14']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell14-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table15']">
        <fo:block-container xsl:use-attribute-sets = "table15-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table15" xsl:use-attribute-sets="table15">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table15']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table15-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table15']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table15-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table15']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table15-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table15']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table15-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table15']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table15-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table15']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table15-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table15']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table15-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table15-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table15-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table15-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table15']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell15-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table16']">
        <fo:block-container xsl:use-attribute-sets = "table16-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table16" xsl:use-attribute-sets="table16">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table16']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table16-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table16']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table16-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table16']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table16-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table16']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table16-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table16']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table16-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table16']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table16-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table16']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table16-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table16-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table16-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table16-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table16']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell16-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table17']">
        <fo:block-container xsl:use-attribute-sets = "table17-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table17" xsl:use-attribute-sets="table17">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table17']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table17-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table17']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table17-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table17']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table17-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table17']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table17-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table17']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table17-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table17']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table17-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table17']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table17-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table17-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table17-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table17-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table17']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell17-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table18']">
        <fo:block-container xsl:use-attribute-sets = "table18-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table18" xsl:use-attribute-sets="table18">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table18']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table18-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table18']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table18-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table18']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table18-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table18']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table18-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table18']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table18-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table18']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table18-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table18']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table18-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table18-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table18-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table18-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table18']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell18-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table19']">
        <fo:block-container xsl:use-attribute-sets = "table19-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table19" xsl:use-attribute-sets="table19">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table19']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table19-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table19']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table19-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table19']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table19-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table19']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table19-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table19']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table19-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table19']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table19-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table19']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table19-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table19-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table19-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table19-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table19']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell19-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table20']">
        <fo:block-container xsl:use-attribute-sets = "table20-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table20" xsl:use-attribute-sets="table20">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table20']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table20-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table20']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table20-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table20']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table20-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table20']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table20-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table20']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table20-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table20']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table20-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table20']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table20-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table20-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table20-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table20-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table20']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell20-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table21']">
        <fo:block-container xsl:use-attribute-sets = "table21-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table21" xsl:use-attribute-sets="table21">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table21']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table21-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table21']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table21-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table21']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table21-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table21']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table21-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table21']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table21-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table21']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table21-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table21']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table21-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table21-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table21-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table21-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table21']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell21-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table22']">
        <fo:block-container xsl:use-attribute-sets = "table22-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table22" xsl:use-attribute-sets="table22">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table22']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table22-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table22']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table22-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table22']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table22-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table22']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table22-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table22']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table22-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table22']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table22-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table22']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table22-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table22-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table22-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table22-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table22']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell22-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table23']">
        <fo:block-container xsl:use-attribute-sets = "table23-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table23" xsl:use-attribute-sets="table23">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table23']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table23-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table23']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table23-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table23']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table23-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table23']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table23-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table23']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table23-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table23']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table23-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table23']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table23-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table23-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table23-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table23-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table23']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell23-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table24']">
        <fo:block-container xsl:use-attribute-sets = "table24-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table24" xsl:use-attribute-sets="table24">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table24']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table24-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table24']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table24-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table24']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table24-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table24']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table24-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table24']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table24-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table24']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table24-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table24']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table24-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table24-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table24-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table24-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table24']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell24-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table25']">
        <fo:block-container xsl:use-attribute-sets = "table25-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table25" xsl:use-attribute-sets="table25">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table25']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table25-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table25']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table25-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table25']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table25-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table25']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table25-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table25']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table25-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table25']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table25-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table25']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table25-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table25-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table25-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table25-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table25']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell25-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table26']">
        <fo:block-container xsl:use-attribute-sets = "table26-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table26" xsl:use-attribute-sets="table26">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table26']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table26-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table26']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table26-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table26']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table26-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table26']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table26-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table26']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table26-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table26']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table26-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table26']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table26-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table26-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table26-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table26-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table26']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell26-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table27']">
        <fo:block-container xsl:use-attribute-sets = "table27-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table27" xsl:use-attribute-sets="table27">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table27']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table27-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table27']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table27-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table27']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table27-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table27']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table27-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table27']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table27-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table27']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table27-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table27']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table27-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table27-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table27-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table27-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table27']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell27-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table28']">
        <fo:block-container xsl:use-attribute-sets = "table28-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table28" xsl:use-attribute-sets="table28">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table28']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table28-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table28']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table28-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table28']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table28-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table28']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table28-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table28']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table28-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table28']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table28-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table28']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table28-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table28-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table28-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table28-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table28']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell28-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table29']">
        <fo:block-container xsl:use-attribute-sets = "table29-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table29" xsl:use-attribute-sets="table29">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table29']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table29-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table29']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table29-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table29']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table29-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table29']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table29-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table29']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table29-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table29']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table29-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table29']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table29-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table29-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table29-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table29-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table29']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell29-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

   
     <xsl:template match="table[@classes='table30']">
        <fo:block-container xsl:use-attribute-sets = "table30-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table30" xsl:use-attribute-sets="table30">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table30']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table30-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table30']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table30-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table30']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table30-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table30']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table30-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table30']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table30-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table30']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table30-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table30']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table30-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table30-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table30-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table30-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table30']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell30-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

     <xsl:template match="title" mode="classes"/>

</xsl:stylesheet>