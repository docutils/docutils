.. $Id$

.. link to the main refs  /Library/svn/trunk/docutils/docs/ref/rst/restructuredtext.txt

doc structure:

bibliographic fields, toc, sections, transitions, body elements

document structure:

-- document: page setup, page dimensions, simple, first, odd-even,
first-odd-even

- sections: 

  - render: as chapters (start on a new page), number

  Should title be its own separate entity? 

  -- chapter title: number, any of the other block layout description, such as
  space before, etc.


TODO:

1. work on sections next, getting all the formatting down

2 paragrah elements (called Paragraphs)

3. work on lists, then tables (to get minimum functionality)

Open Office Names
==================

Page: 
   
   page-format

       width, height, orientation (portrait of landscape); 
   
   Margins:

     left right bottom top  

   page layout: 
   
        right and left, mirrored, only right, only left; 

   format [for numbering]:
   
      1, i, I, a, A none; 
         
    Background: 

       color; 

    Borders:

       [puts borders around page; not possible in FO]

   header [will put down below]

header: 

    header on, same content left and right, left margin, right margin;
    Spacing: use dynamic spacing, AutoFit height; 

    borders 
    
        [drop down of different types of borders; 
        
        spacing to content: 

             left, right, top bottom, syncronize; 
        
        Shadow style position: 
        
            distance, color; 
              
     backround [drop down of colors]

Can set font-size on  fo:page-sequence, fo:flow

.. svn propset svn:keywords "Date Author Id HeadURL Revision" the file

FO notes
========

Generally, set the extent to the same as top-margin or bottom-margin. Then, to 
move the header down, use space-before with
space-before.conitionality="retain".

<!--sets header 1/2 inch down from top margin-->
<fo:block space-before=".5in" space-before.conditionality = "retain">

    * em (ems, the height of the element's font)
    * ex (x-height, the height of the letter "x")
    * px (pixels, relative to the canvas resolution)
    * in (inches; 1in=2.54cm)
    * cm (centimeters; 1cm=10mm)
    * mm (millimeters)
    * pt (points; 1pt=1/72in)
    * pc (picas; 1pc=12pt)

