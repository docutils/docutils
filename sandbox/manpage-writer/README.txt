==============================
 manpage writer for Docutils_ 
==============================

:Contact: grubert@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

This tries to explore the posibilities to generate man-pages from
ReStructured text. Man pages are the way for unix systems to provide
help to the user. Gnu does this with info-pages.

Primary goal is to produce man pages to be viewed online, full nroff support
with tables, equations and images is not on the list, is it ?

Module information
''''''''''''''''''

man pages look like ::
       
   man(1)     Man Pager Utils     man(1)
       
   NAME
       man - an interface to the on-line reference manuals
       
   SYNOPSIS
       man [-c|-w|-tZT device] [-adhu7V] [-m system[,...]] [-L locale]
     
in roff formatting ::
     
     .TH man 1 "14 May 2001" "2.3.19" "Manual pager utils"
     .SH NAME
     man \- an interface to the on-line reference manuals
     .SH SYNOPSIS
     .\" The general command line
     .B man
     .RB [\| \-c \||\| \-w \||\| \-tZT
     .IR device \|]

This means we have 

* a title "man" 
* a subtitle "an interface to the on-line reference manuals"
* a section "1"
* a group "Manual pager utils"
* a date "14 May 2001"
* a version "2.3.19"

Problems
''''''''

* man pages have a special structure and organization. From the manpage 
  to *man* ::

    The table below shows the section numbers of the manual followed  by  the
    types of pages they contain.

    1   Executable programs or shell commands
    2   System calls (functions provided by the kernel)
    3   Library calls (functions within program libraries)
    4   Special files (usually found in /dev)
    5   File formats and conventions eg /etc/passwd
    6   Games
    7   Miscellaneous  (including  macro  packages and conven-
        tions), e.g. man(7), groff(7)
    8   System administration commands (usually only for root)
    9   Kernel routines [Non standard]

    A manual page consists of several parts.

    They  may  be  labelled  NAME,  SYNOPSIS,  DESCRIPTION,  OPTIONS,  FILES,
    SEE ALSO, BUGS, and AUTHOR.

    The  following  conventions apply to the SYNOPSIS section and can be used
    as a guide in other sections.

    bold text          type exactly as shown.
    italic text        replace with appropriate argument.
    [-abc]             any or all arguments within [ ] are optional.
    -a|-b              options delimited by | cannot be used together.
    argument ...       argument is repeatable.
    [expression] ...   entire expression within [ ] is repeatable.

    The command or function illustration is a pattern that should  match  all
    possible  invocations.   In some cases it is advisable to illustrate sev-
    eral exclusive invocations as is shown in the SYNOPSIS  section  of  this
    manual page.

* use title and subtitle to built ::

    NAME
       man - program to view ...

* line ends around email or web addresses in texts.
  How to distinguish something is inline or not ?
  
* Is one rst-document one manpage section or might there be more sections
  contained.
* Tables are possible in roff, via the external processor tbl. Although
  advised to avoid if possible.
* Images
* Equations
* nested list entries sometimes get bold. RST problem
* RST problem "-d #  option " breaks
* enumerated lists with more than 9 
* line blocks in tables have too much new lines.

Change log
''''''''''


* writers/manpage.py:

  - 2006-10-31: Add raw.
  - 2006-10-31: Distinguish section_level 0 (TH), 1 (SH) other (SS).
  - 2006-10-30: Add line block.
  - 2006-10-30: Add headerless tables.
  - 2006-10-23: Fix: listbullets and numbers were prefixed with ``0``.
  - 2006-10-22: less empty lines
  - 2006-10-21: from sandbox/grubert, better option list handling,
                definition list handling.

.. _Docutils: http://docutils.sourceforge.net/
