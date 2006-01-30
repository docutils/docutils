# $Id$
# Copyright (C) 2002-2005 Freescale Semiconductor, Inc.
# Distributed under terms of the GNU General Public License (GPL).

package RST;
# This package does parsing of RST files

=pod
=begin reST
=begin Usage
Defines for reStructuredText parser
-----------------------------------
-D align=<0|1>     Allow inferring right/center alignment in
                   single line simple table cells (default is 1).
-D entryattr=<text>
                   Specifies attributes to be passed to table entry
                   (default is '').  Note: this option can be
                   changed on the fly within a table by using a perl
                   directive to set $main::opt_D{entryattr}.
-D includeext=<text>
                   A colon-separated list of extensions to check for
                   included files.  Default is ":.rst:.txt".
-D includepath=<text>
                   A colon-separated list of directories for the
                   include directive to search.  The special token
                   "<.>" represents the directory of the file 
                   containing the directive (which may not be the
                   same as the directory in which trip is invoked, ".".
                   Default is "<.>".
-D nestinline[=<0|1>]
                   Specify whether to allow nesting of inline markup.
                   There are some limitations, like strong cannot be
                   nested within emphasis.
                   Default is 1 (1 if specified with no value).
-D perlpath=<text>
                   A colon-separated list of directories to search
                   for Perl modules.  The special token "<INC>" 
                   represents the default Perl include path.
                   Default is "<INC>".
-D report=<level>  Set verbosity threshold; report system messages
                   at or higher than <level> (by name or number:
                   "info" or "1", warning/2, error/3, severe/4; 
                   also, "none" or 5).  Default is 2 (warning).
-D rowattr=<text>  Specifies attributes to be passed to table rows
                   (default is '').  Note: this option can be
                   changed on the fly within a table by using a perl
                   directive to set $main::opt_D{rowattr}.
-D source=<text>   Overrides the file name as the source.
-D tableattr=<text>
                   Specifies attributes to be passed to tables (default
                   is '${RST::DEFAULTS{tableattr}}'). 
                   Note: this option can be changed on the fly to
                   have tables with different characteristics by
                   using a perl directive to set
                   $main::opt_D{tableattr}.
-D tabstops=<num>  Specifies that tab characters are assumed to tab
                   out to every <num> characters (default is 8).
-D xformoff=<regexp>
                   Turns off default transforms matching regexp.
                   (Used for internal testing.)
=end Usage
=end reST
=cut

# Global variables:
#   Many static (read-only) variables defined in BEGIN blocks (not documented).
#
#   ``$RST::NEXT_ID``:   The next id to be returned by RST::Id();
#   ``%RST::SEC_LEVEL``: Hash whose keys are header styles and whose
#                        value (if defined) indicates the level of
#                        sections encoded with that header style.
#   ``@RST::SEC_DOM``:   Array whose index is the section level and
#                        whose value is the section DOM object at that level.
#   ``@RST::SEC_STYLE``: Array whose index is the section level and
#                        whose value is the section style at that level.
#   ``@RST::ANONYMOUS_TARGETS``: Array of references to anonymous
#                        target DOMs in file order. 
#   ``%RST::REFERENCE_DOM``: Hash whose keys are tags and whose values
#                        are references to a hash with names or ids as
#                        keys and the associated target DOM object as
#                        value.
#   ``%RST::TARGET_NAME``: Hash whose keys are namespace ids and whose
#                        values are references to a hash whose keys
#                        are names and whose value is a reference to
#                        an array of all the DOM objects having that
#                        name in that name space.
#   ``%RST::ALL_TARGET_IDS``: Hash whose keys are ids and whose value
#                        is a reference to an array of all the DOM
#                        objects having that id. 
#   ``%RST::ALL_TARGET_NAMES``: Hash whose keys are names and whose
#                        value is a reference to an array of all the
#                        DOM objects having that name.
#   ``%RST::IMPLICIT_SCHEME``: Hash whose keys are the names of implicit
#                        URI schemes and whose values are 1.
#   ``%RST::MY_ROLES``:  Hash whose keys are the role names that are
#                        currently defined for the current document
#                        (it gets reset between documents) and whose
#                        values are Role definition hash references.
#   ``$RST::MY_DEFAULT_ROLE``:  The current name of the default role
#                        for the current document.  Initially
#                        ``title_reference``.

use strict;

# Initialized global variables
use vars qw($BULLETS $EMAIL $ENUM $ENUM_INDEX $FIELD_LIST $LINE_BLOCK
	    $MARK_END_TRAILER $MARK_START $MIN_SEC_LEN $OPTION
	    $OPTION_LIST $SECTION_HEADER $SEC_CHARS %DIRECTIVES
	    %ERROR_LEVELS %IMPLICIT_SCHEME %LEFT_BRACE %MARK_END
	    %MARK_TAG %MARK_TAG_START %MATCH_BRACE %ROLES
	    $DEFAULT_ROLE %XML_SPACE %DEFAULTS
	    %ALPHA_INDEX %ROMAN_VALS %CITSPACE %NAMESPACE @UNITS);

# Run-time global variables
use vars qw($NEXT_ID $TOPDOM %ALL_TARGET_IDS %ALL_TARGET_NAMES
	    %REFERENCE_DOM %SEC_LEVEL %TARGET_NAME @ANONYMOUS_TARGETS
	    @ENUM_STRINGS @SEC_DOM @SEC_STYLE %MY_ROLES $MY_DEFAULT_ROLE);

BEGIN {
    use URIre;
    $SEC_CHARS = '[^a-zA-Z0-9\s]';
    $SECTION_HEADER = "(((?!$SEC_CHARS+\\n(?:\\n|\\Z))(?!::\\n|(?:(?:\\.\\.|__)\\n(?:   |\\.\\.[ \\n]|__[ \\n]|\\n)))($SEC_CHARS)\\3+)\\n(.*\\n)?(($SEC_CHARS)\\6+\\n)?|^(?!(?:\\.\\.|__)(?: .*)?\\n(?:\.\.|__)[ \\n])(\\S.*\\n)(($SEC_CHARS)\\9+)\\n)";
    $BULLETS = '[-*+]';
    $LINE_BLOCK = '\|';
    $MIN_SEC_LEN = 4;
    my $rst_low_roman = 'm{0,4}(?:dc{0,3}|c[dm]|c{0,3})?(?:lx{0,3}|x[lc]|x{0,3})?(?:vi{0,3}|i[vx]|i{0,3})?';
    my $rst_upp_roman = $rst_low_roman;
    $rst_upp_roman =~ tr/a-z/A-Z/;
    $ENUM_INDEX = "\\d+|[a-zA-Z]|$rst_low_roman|$rst_upp_roman|#";
    $ENUM = "(\\()?($ENUM_INDEX)([\\).])";
    $FIELD_LIST = ':(?! )[^:\n]*[^:\n ]:(?!\`[^\`]*?\`)';
    $OPTION = '[+-][\w](?: [^ ,]+| ?<[^>]+>)?|(?:--?[\w][\w-]*|/[A-Z]+)(?:=[^ ,=]+| [^ ,]+|=<[^>]+>)?';
    $OPTION_LIST = "(?:$OPTION)(?:, (?:$OPTION))*(?:  |\\s*\\n )";
    %ERROR_LEVELS = (1=>"INFO", 2=>"WARNING", 3=>"ERROR", 4=>"SEVERE");
    $EMAIL = '[\w.-]+\@[\w.-]*[\w-]';
    $MARK_START = '\*\*?|\`\`?|\||_\`|\[';
    %MARK_END = ('*'=>'\*', '**'=>'\*\*', '`'=>'\`_?_?', '``'=>'\`\`',
		 '|'=>'\|_?_?', '_`'=>'\`', '['=>'\]__?',
		 ''=>"__?|$URIre::absoluteURI|$EMAIL");
    $MARK_END_TRAILER = '[-\'\"\)\]\}\\\\>/:.,;!? ]|\Z';
    %MARK_TAG = ('**'=>'emphasis', '****'=>'strong', '``'=>'interpreted',
		 '````'=>'literal', '||'=>'substitution_reference',
		 '||_'=>'substitution_reference',
		 '||__'=>'substitution_reference',
		 '_``'=>'target', '[]_'=>'footnote_reference',
		 '``_'=>'reference',
		 '_'=>'reference', '``__'=>'reference',
		 '__'=>'reference');
    %MARK_TAG_START = ('*'=>'emphasis', '**'=>'strong',
		       '`'=>'interpreted text or phrase reference', '``'=>'literal',
		       '|'=>'substitution_reference', '_`'=>'target',
		       '['=>'footnote', ''=>'reference');
    %MATCH_BRACE = ('"'=>'"', "'"=>"'", '('=>')', '['=>']', '{'=>'}',
		    '<'=>'>', ''=>'impossible', '_`'=>'`');
    %LEFT_BRACE = ('>'=>'<', ')'=>'(', ']'=>'[', '}'=>'{');
    my @implicit_schemes = qw(acap afs cid data dav fax file ftp go
			      gopher h323 http https im imap ipp ldap
			      mailserver mailto mid modem mupdate news
			      nfs nntp opaquelocktoken pop pres
			      prospero rtsp service sip sips soap.beep
			      soap.beeps tel telnet tftp tip tn3270
			      urn vemmi wais xmlrpc.beep xmlrpc.beeps
			      z39.50r z39.50s
			      );
    @IMPLICIT_SCHEME{@implicit_schemes} = (1) x @implicit_schemes;
    %DIRECTIVES = (admonition=> \&RST::Directive::admonition,
		   attention => \&RST::Directive::admonition,
		   caution   => \&RST::Directive::admonition,
		   danger    => \&RST::Directive::admonition,
		   error     => \&RST::Directive::admonition,
		   hint      => \&RST::Directive::admonition,
		   important => \&RST::Directive::admonition,
		   note      => \&RST::Directive::admonition,
		   tip       => \&RST::Directive::admonition,
		   warning   => \&RST::Directive::admonition,
		   footer    => \&RST::Directive::decoration,
		   header    => \&RST::Directive::decoration,
		   section_numbering
		             => \&RST::Directive::sectnum,
		   section_autonumbering
		             => \&RST::Directive::sectnum,
		   csv_table => \&RST::Directive::table,
		   list_table=> \&RST::Directive::table,
		   restructuredtext_test_directive
		             => \&RST::Directive::test_directive,
		   );
    %ROLES = (emphasis=>{tag=>'emphasis'},
	      strong=>{tag=>'strong'},
	      literal=>{tag=>'literal'},
	      subscript=>{tag=>'subscript'},
	      sub=>{alias=>'subscript'},
	      superscript=>{tag=>'superscript'},
	      sup=>{alias=>'superscript'},
	      ab=>{tag=>'abbreviation'},
	      ac=>{tag=>'acronym'},
	      inline=>{tag=>'inline'},
	      raw=>{tag=>'raw', attr=>{'xml:space'=>'preserve'},
		check=>\&RST::Role::raw},
	      
	      'raw-formatting'=>{tag=>'inline'},
	      'pep-reference'=>{alias=>'PEP'},
	      PEP=>{tag=>'reference',
		    attr=>{refuri=>"http://www.python.org/peps/pep-%04d.html"},
		    text=>"PEP %s",
		    check=>\&RST::Role::PEP,
		},
	      'rfc-reference'=>{alias=>'RFC'},
	      RFC=>{tag=>'reference',
		    attr=>{refuri=>"http://www.faqs.org/rfcs/rfc%04d.html"},
		    text=>"RFC %s",
		    check=>\&RST::Role::RFC,
		},
	      'title-reference'=>{tag=>'title_reference'},
	      title=>{alias=>'title-reference'},
	      t=>{alias=>'title-reference'},
	      );
    $DEFAULT_ROLE = 'title-reference';
    @UNITS = (qw(em ex px in cm mm pt pc), '');
    %XML_SPACE = ('xml:space'=>'preserve');
    %DEFAULTS = (align=>1, report=>2, includeext=>':.rst:.txt',
		 includepath=>'<.>', nestinline=>1,
		 perlpath=>'<INC>',
#		 tableattr=>'class="table" frame="border" rules="all"',
		 tableattr=>'border="1" class="docutils"',
		 tabstops=>8);
}

# Processes defaults for -D defines and resets global variables
# between documents.
# Arguments: document DOM object
# Returns: None
# Sets globals: $RST::NEXT_ID, %RST::SEC_LEVEL, @RST::SEC_DOM,
#               @RST::SEC_STYLE, @RST::ANONYMOUS_TARGETS,
#               %RST::REFERENCE_DOM, %RST::TARGET_NAME,
#               %RST::ALL_TARGET_IDS, %RST::ALL_TARGET_NAMES, $RST::TOPDOM
#               %RST::MY_ROLES
sub init {
    my ($doc) = @_;

    foreach (keys %DEFAULTS) {
	$main::opt_D{$_} = $DEFAULTS{$_} unless defined $main::opt_D{$_};
    }
    foreach (keys %main::opt_D) {
	# Force any defines with no values specified to be 1
	$main::opt_D{$_} = 1
	    if defined $main::opt_D{$_} && $main::opt_D{$_} eq '';
    }

    undef $NEXT_ID;
    undef %SEC_LEVEL;
    @SEC_DOM = ($doc);
    @SEC_STYLE = ("");
    undef @ANONYMOUS_TARGETS;
    undef %REFERENCE_DOM;
    undef %TARGET_NAME;
    undef %ALL_TARGET_IDS;
    undef %ALL_TARGET_NAMES;

    $TOPDOM = $doc;

    # Handle the Perl include path
    my $perl_inc = join(':', @INC);
    my $new_inc = $main::opt_D{perlpath};
    $new_inc =~ s/<inc>/$perl_inc/gi;
    @INC = split(/:/, $new_inc);
    delete $main::opt_D{perlpath};
    $MY_DEFAULT_ROLE = $DEFAULT_ROLE;
    %MY_ROLES = %ROLES;
}

# Returns a DOM object for a problematic with its ids.
# Arguments: message, reference id (optional), id (optional)
# Returns: DOM object, reference id, id
sub problematic {
    my ($text, $refid, $id) = @_;

    $refid = Id() unless defined $refid;
    $id = Id() unless defined $id;
    my $dom = new DOM('problematic', refid=>$refid, ids=>[ $id ]);
    $dom->append(newPCDATA DOM($text));
    return ($dom, $refid, $id);
}

# Returns a DOM object for a system message.
# Arguments: severity level, source, line number, message, literal text, 
#            key/value pairs for additional attributes
sub system_message {
    my ($level, $source, $lineno, $msg, $lit, %attr) = @_;
    my $dom = new DOM("system_message", level=>$level, line=>$lineno,
		      source=>$source,
		      type=>$ERROR_LEVELS{$level}, %attr);
    my $para = new DOM('paragraph');
    $para->append(newPCDATA DOM("$msg\n"));
    $dom->append($para);
    if (defined $lit && $lit ne '') {
	my $lb = new DOM('literal_block', %XML_SPACE);
	$lb->append(newPCDATA DOM($lit));
	$dom->append($lb);
    }
    my $line = $lineno ? ":$lineno" : '';
    print STDERR "$source$line ($ERROR_LEVELS{$level}/$level) $msg\n"
	if $level >= $main::opt_D{report} && $source ne 'test data';
    return $dom;
}

# Processes a bulleted list paragraph.
# Arguments: paragraph, source, line number
# Returns: processed paragraph, list of DOM objects and unprocessed paragraphs
sub BulletList {
    my($para, $source, $lineno) = @_;

    my @err;
    my $lines = 0;
    my ($processed, @unp);
    $para =~ /^($BULLETS)(?: |\n)/o;
    my $dom = new DOM('bullet_list', bullet=>$1);
    my $bullet = "\\$1";
    (undef, my @paras) = split /^($bullet(?: +|\n))/m, $para;
    while (my ($bull, $p) = splice @paras, 0, 2) {
	$p = '' unless defined $p;
	my $li = new DOM('list_item');
	$dom->append($li);
	my $para = "$bull$p";
	$para =~ s/^$bullet *//;
	$para =~ s/^  //mg;
	Paragraphs($li, $para, $source, $lineno+$lines);
	$lines += $para =~ tr/\n//;
	$processed .= $para;
    }
    return ($processed, @err, $dom, @unp);
}

# Coalesces a series of similar paragraphs and divides initial
# paragraph for unexpected indents.
# Argument: reference to array of paragraphs
# Returns: None (but modifies the paragraphs referenced by the argument)
sub Coalesce {
    my ($paras) = @_;
    # Note: consecutive paragraphs are two indices apart in the array, with
    # any blank lines between them in the intermediate index.  We also use
    # the intermediate index to store error sentinels, which begin with a
    # newline and have a non-blank character in the second line.
    my $p;
    my ($enumtype, $enumval, $enumprefix, $enumsuffix) = ('') x 4;
    for ($p=0; $p <= 2 && $p < @$paras; $p++) {
#print STDERR "[",join("][",@{$paras}[0..2]),"]\n";
	if (defined $paras->[$p]) {
	    # Pull out the part of the paragraph prior to a blank line
	    my @split = split /^(\s*\n)/, $paras->[$p], 2;
	    my ($pre_p, $post_p) = @split > 1 ? @split :
		($paras->[$p], '');
	    # May need to split the first paragraph
	    if ($pre_p =~ /^($BULLETS)(?: |\n)/so) {
		# Bulleted list
		if ((my @s = split /^(?![$1]|  )(.)/m, $pre_p, 2) > 1) {
		    # Bulleted list has unexpected unindent
		    splice(@$paras, $p, 1,($s[0],
					   # This is a sentinel that an error occurred
					   "\n" . q(system_message(2, $source, $lineno, "Bullet list ends without a blank line; unexpected unindent.")),
					   "$s[1]$s[-1]$post_p"));
		}
	    }
	    elsif ($pre_p =~ /^($LINE_BLOCK)(?: |\n)/so) {
		# Line block
		if ((my @s = split /^(?!$LINE_BLOCK(?:\s+\S|\n)|  )(.)/m,
		     $pre_p, 2) > 1) {
		    # Line block has unexpected unindent
		    splice(@$paras, $p, 1,($s[0],
					   # This is a sentinel that an error occurred
					   "\n" . q(system_message(2, $source, $lineno, "Line block ends without a blank line.")),
					   "$s[1]$s[-1]$post_p"));
		}
	    }
	    elsif ($pre_p =~ /^$SECTION_HEADER/om) {
	    }
	    elsif ($pre_p =~ /^((\.\.|__)( |\n))/) {
		# A comment or anonymous target
		$pre_p =~ s/^(.*\n?)//;
		my $first = $1;
		if ((my @s = split /^((?:\.\.|__)(?: |\n))/m, $pre_p, 2) > 1){
		    splice(@$paras, $p, 1, "$first$s[0]", "", "$s[1]$s[-1]");
		}
	    }
	    elsif ($pre_p =~ /^( |\n)/) {
		# These get dealt with elsewhere
	    }
	    elsif ($pre_p =~ /^$ENUM .*\n(?=\Z|\n| |$ENUM)/o) {
		# An enumerated list
		my ($prefix,$index,$suffix) = map(defined $_ ? $_ : '',
						  ($1,$2,$3));
		my $type = EnumType($index);
		$type = 'arabic' if $type eq '#';
		my $val = EnumVal($index, $type);
		$pre_p =~ s/^(.*\n)//;
		my $first = $1;
		my @enum_list;
		while ($pre_p ne '') {
		    if ((my @s = split /^($ENUM .*\n(?=\Z|\n| |$ENUM))/mo,
			 $pre_p, 2) > 1) {
			# Check for out-of-sequence enumerated list item
			my ($pf,$in,$sf) = map(defined $_ ? $_ : '',
					       @s[2..4]);
			my $v = EnumVal($in, $type);
			if ($pf ne $prefix || $sf ne $suffix ||
			    ($v ne '#' && $v != $val+1)) {
			    my $enum_list = join('',@enum_list);
			    splice(@$paras, $p, 1, "$enum_list$first$s[0]",
				   "\n" . q(system_message(2, $source, $lineno, "Enumerated list ends without a blank line; unexpected unindent.")),
				   "$s[1]$s[-1]$post_p");
			    last;
			}
			else {
			    push(@enum_list, "$first$s[0]");
			    $first = $s[1];
			    $pre_p = "$s[-1]";
			    $val++;
			}
		    }
		    else {
			push(@enum_list, "$first$pre_p");
			$first = "";
			$pre_p = "";
		    }
		}
		push (@enum_list, $first) if $first ne '';
#print "$p: {\n",map("[$_]\n", @enum_list),"}\n";
		# Check any enumerated lists for unexpected indent
		my $prev_paras = '';
		my $enum;
		while ($enum = shift @enum_list) {
		    my $para = $enum;
		    $para =~ /^($ENUM )/o;
		    my $spaces = " " x length($1);
		    $para =~ s/^(.*\n)//;
		    my $first = $1;
		    if ((my @s = split /^(?!$spaces)(.)/m, $para, 2) > 1) {
			my $rest = join('',@enum_list);
			my @items =
			    (
			     # This is a sentinel that an error occurred
			     "\n" . q(system_message(2, $source, $lineno, "Enumerated list ends without a blank line; unexpected unindent.")),
			     "$s[1]$s[-1]$rest$post_p");
			# Enumerated list has unexpected indent
			splice(@$paras, $p, 1, "$prev_paras$first$s[0]",
			       @items);
			last;
		    }
		    $prev_paras .= "$first$para";
		}
	    }
	    elsif ($pre_p =~ /^$FIELD_LIST/) {
		# A field list
		if ((my @s = split /^(?! |$FIELD_LIST)(.)/m, $pre_p, 2) > 1) {
		    # Field list has unexpected indent
		    splice(@$paras, $p, 1,($s[0],
					   # This is a sentinel that an error occurred
					   "\n" . q(system_message(2, $source, $lineno, "Field list ends without a blank line; unexpected unindent.")),
					   "$s[1]$s[-1]$post_p"));
		}
	    }
	    elsif ($pre_p =~ /^$OPTION_LIST/) {
		# An option list
		if ((my @s = split /^(?! |$OPTION_LIST)(.)/m, $pre_p, 2) > 1){
		    # Field list has unexpected indent
		    splice(@$paras, $p, 1,($s[0],
					   # This is a sentinel that an error occurred
					   "\n" . q(system_message(2, $source, $lineno, "Option list ends without a blank line; unexpected unindent.")),
					   "$s[1]$s[-1]$post_p"));
		}
	    }
	    elsif (IsTable($pre_p)) {
		# It's a table
		if ($pre_p =~ /^[+][+-]+[+] *\n/ &&
		    (my @s = split /^([^|+])/m, $pre_p, 2) > 1) {
		    my $after = "$s[1]$s[-1]$post_p";
		    # Table is missing blank line
		    splice(@$paras, $p, 1, ($s[0],
					    # This is a sentinel that an error occurred
					    "\n" . q(system_message(2, $source, $lineno, "Blank line required after table.")),
					    $after));
		    if ($after =~ /^ /) {
			splice(@$paras, $p+1, 0, 
			       # This is a sentinel that an error occurred
			       "\n" . q(system_message(3, $source, $lineno, "Unexpected indentation.")),
			       "");
			
		    }
		}
	    }
	    elsif ($pre_p =~ /^\S.*\n /) {
		# A definition list
		if (#$pre_p =~ /^\S.*\n\S/m ||
		    (my @s = split /^(\S.*\n)$/m, $pre_p, 2) > 1) {
		    # Definition list has unexpected indent
		    splice(@$paras, $p, 1,($s[0],
					   # This is a sentinel that an error occurred
					   "\n" . q(system_message(2, $source, $lineno, "Definition list ends without a blank line; unexpected unindent.")),
					   "$s[1]$post_p"));
		}
	    }
	    else {
		# A standard paragraph
		if ((my @s = split /^( )/m, $pre_p, 2) > 1
		    && $pre_p !~ /:: *$/) {
		    # This "paragraph" has indentation or other problems
		    splice(@$paras, $p, 1,($s[0],
					   # This is a sentinel that an error occurred
					   "\n" . q(system_message(3, $source, $lineno, "Unexpected indentation.")),
					   "$s[1]$s[-1]$post_p"));
		}
	    }
	}
	# Or may need to join consecutive paragraphs
	if ($p >= 2 &&
	    # Don't consolidate paragraphs with errors in the middle
	    (defined $paras->[$p-1] && $paras->[$p-1] !~ /^\n\S/s &&
	     (
	      # Consecutive block quotes
	      (substr($paras->[$p-2],0,1) eq ' ' &&
	       substr($paras->[$p],0,1) eq ' ')
	      ||
	      # Comments followed by indented text
	      ($paras->[$p-2] =~ /^((\.\. )|(__( |\n)))/ &&
	       $paras->[$p]=~ /^ /)
	      ||
	      # Consecutive bulleted lists
	      ($paras->[$p-2] =~ /^($BULLETS)(?: |\n)/o &&
	       $paras->[$p] =~ /^(?:[$1]| )/)
	      ||
	      # Consecutive enumerated lists
	      ($paras->[$p-2] =~ /^$ENUM .*\n(?=\Z|\n| |$ENUM)/o &&
#do {print "$p: $paras->[$p-2]~~~~~~$paras->[$p]========"; 1; } &&
	       do {
		   my ($prefix, $index, $suffix) =
		       map defined $_ ? $_ : '', ($1, $2, $3);
		   my $type = EnumType($index);
		   if (($type ne $enumtype && $type ne '#') ||
		       $prefix ne $enumprefix ||
		       $suffix ne $enumsuffix) {
		       $enumtype = $type;
		       $enumtype = 'arabic' if $enumtype eq '#';
		       $enumprefix = $prefix;
		       $enumsuffix = $suffix;
		       $enumval = EnumVal($index, $type);
		       $enumval = 1 if $enumval eq '#';
		   }
		   ($paras->[$p] =~ /^   / ||
		    $paras->[$p] =~ /^$ENUM .*\n(?=\Z|\n| |$ENUM)/o &&
#do { print "$prefix-$enumtype-$enumval-$suffix vs $1-$enumtype-$2-$3\n"; 1; } &&
		    do {
			my $val = EnumVal($2, $enumtype);
			$val = $enumval + 1 if $val eq '#';
			my $oldval = $enumval;
			$enumval = $val;
			$val == $oldval+1;
		    }
		    && ($1 || '') eq $enumprefix && ($3 || '') eq $enumsuffix)
		   })
	      ||
	      # Incomplete simple table
	      ($paras->[$p-2] =~ /^=+( +=+)+ *\n/ &&
	       $paras->[$p] =~ /^\001/)
	      ||
	      # Consecutive field lists
	      ($paras->[$p-2] =~ /^$FIELD_LIST/o &&
	       $paras->[$p]=~ /^($FIELD_LIST| )/o)
	      ||
	      # Consecutive definition lists
	      ($paras->[$p-2] =~ /^(?!\.\.|__( |\n)|$OPTION_LIST)\S.*\n /o &&
	       $paras->[$p]=~ /^(?!\.\.|__( |\n)|$OPTION_LIST|$FIELD_LIST|$BULLETS( |\n)|$ENUM )(\S.*\n)? /o)
	      ||
	      # Consecutive option lists
	      ($paras->[$p-2] =~ /^$OPTION_LIST/o &&
	       $paras->[$p]=~ /^(($OPTION_LIST)| )/o)
	      ))) {
#print STDERR "Coalescing: [$paras->[$p-2]]\n[$paras->[$p-1]]\n[$paras->[$p]]\n";
	    splice(@$paras, $p-2, 3, "$paras->[$p-2]$paras->[$p-1]$paras->[$p]");
	    $p--;
	}
    }
}

# Defines a new role, optionally based upon an existing role
# Arguments: new role name, optional old role name, optional option key/values
# Returns: possible error message
sub DefineRole {
    my ($role, $tag, %options) = @_;

    $tag = 'inline' unless defined $tag;
    return qq(cannot make "$role" into a class name.)
	unless $role =~ /[a-z][-\w\.]*/i;
    my $class = defined $options{class} ? $options{class} : $role;
    return qq(invalid option value: (option: "class"; value: '$class')\ncannot make "$class" into a class name.)
	unless $class =~ /[a-z][-\w\.]*/i;
    # Default all options, etc. from the base tag
    $RST::MY_ROLES{$role} = Util::DeepCopy($RST::MY_ROLES{$tag});
    $RST::MY_ROLES{$role}{tag} = $RST::MY_ROLES{$tag}{tag};
    $RST::MY_ROLES{$role}{attr}{classes} = [ $class ];
    # Process format, prefix and suffix options
    if (defined $options{format}) {
	$RST::MY_ROLES{$role}{attr}{format} = $options{format};
	delete $options{format};
    }
    $options{prefix} = HashifyFieldList($options{prefix}) if $options{prefix};
    $options{suffix} = HashifyFieldList($options{suffix}) if $options{suffix};
    # Merge any local options with the options of the underlying class
    @{$RST::MY_ROLES{$role}{options}}{keys %options} =
	values %options if %options;
    return;
}

# Processes a definition list paragraph.
# Arguments: paragraph, source, line number
# Returns: processed paragraph, list of DOM objects and unprocessed paragraphs
sub DefinitionList {
    my($para, $source, $lineno) = @_;

    my $dom = new DOM('definition_list');
    my ($processed, @unp);

    (undef, my @paras) = split /^((?:\S.*)\n(?: +))/m, $para;
    while (@paras > 1 && (my ($item, $def) = splice @paras, 0, 2)) {
	my $para = "$item$def";
	$para =~ /^(\S.*)\n( +)/;
	my ($term, $spaces) = ($1, $2);
	my $dli = new DOM('definition_list_item');
	$dom->append($dli);
	my $class = '';
	my @errs;
	if ($term =~ /:: *$/) {
	    push (@errs, system_message(1, $source, $lineno+1,
					qq(Blank line missing before literal block (after the "::")? Interpreted as a definition list item.)));
	}
	# We have to handle the case where the ' : ' is
	# within a literal quote.
	# Get rid of all literal quotes
	my %strings;
	$term =~ s/(\A| )(``((?!``).)*``)(?=[ \n\]\):;])/
	    my $v = $2; my $s = \$v; bless $s,"STR"; $strings{$s}=$2; "$1$s"/ge;
	if ($term !~ /^``((?!``).)*``/ && $term =~ /(.*?) : (.*)/) {
	    $term = $1;
	    $class = $2;
	}
	# Put the literal quotes back
	$term =~ s/(STR=SCALAR\(0x[0-9a-f]+\))/$strings{$1} || $1/ge;
	$class =~ s/(STR=SCALAR\(0x[0-9a-f]+\))/$strings{$1} || $1/ge;
	my $def = new DOM('definition');
	my $t = new DOM('term');
	push(@errs, Inline($t, $term, $source, $lineno));
	$dli->append($t);
	if ($class ne '') {
	    my @classifiers = split / +: +/, $class;
	    foreach (@classifiers) {
		my $classifier = new DOM('classifier');
		push(@errs, Inline($classifier, $_, $source, $lineno));
		$dli->append($classifier);
	    }
	}
	$def->append(@errs);
	$dli->append($def);
	$para =~ s/^(.*\n)//;
	my $first = $1;
 	if ((my @s = split /\n(\S)/, $para, 2) > 1) {
	    # Check for unexpected unindents
	    $para = (shift @s) . "\n";
	    @paras = join '', @s, @paras;
 	}
	$para =~ s/^$spaces//mg;
	Paragraphs($def, $para, $source, $lineno+1);
	$para = "$first$para";
	$lineno += $para =~ tr/\n//;
	$processed .= $para;
    }
    my $unp = join('',@paras);
    if ($unp !~ /^$/) {
	push @unp, system_message(2, $source, $lineno,
				  "Definition list ends without a blank line; unexpected unindent.");
	push @unp, $unp;
    }
    return ($processed, $dom, @unp);
}

# Parses a directive and attaches it to a DOM if successful.
# Arguments: DOM object, source, line number, error message id, 
#            directive text, paragraph literal
# Returns: error flag,
#          reference to array of DOM objects (possibly including input DOM),
#          reference to array of unparsed paragraphs.
sub Directive {
    my ($parent, $source, $lineno, $errmsgid, $dtext, $lit) = @_;
#print STDERR "Directive(",join(',',@_),")\n";

    my @dom;
    my @unprocessed;
    my $error = 1;
    $dtext =~ /(\s*)([\w.-]+)\s*:: *(.*)/s;
    my ($pre, $directive, $body) = map defined $_ ? $_ : '',($1, $2, $3);
    my $dname = $directive;
    $directive =~ tr/[A-Z].-/[a-z]__/;
#print STDERR "[$pre][$directive][$body]\n";
    my $subst = $parent->{tag} eq 'substitution_definition' ?
	$parent->{attr}{names}[0] : '';
    
    if ($dtext eq "\n") {
	push(@dom, system_message(2, $source, $lineno,
				  qq($errmsgid "$subst" missing contents.),
				  $lit));
    }
    elsif ($directive eq '') {
	push(@dom, system_message(2, $source, $lineno,
				  qq($errmsgid "$subst" empty or invalid.),
				  $lit))
	    if $subst ne '';
    }
    else {
	if (! defined $DIRECTIVES{$directive}) {
	    # First see if there's a routine defined for it
	    my $d = "RST::Directive::$directive";
	    $DIRECTIVES{$directive} = \&$d if defined &$d;
	}
	if (! defined $DIRECTIVES{$directive}) {
	    push(@dom,system_message(1, $source, $lineno,
				     qq(No directive entry for "$dname" in module "RST::Directive".\nTrying "$dname" as canonical directive name.)));
	    eval("use Directive::$directive");
	    die "Error compiling $directive: $@" if $@ && ! $@ =~ /in \@INC/;
	    return 1, \@dom, [$lit] if defined $DIRECTIVES{$directive};
	}
	if ( defined $DIRECTIVES{$directive}) {
	    my $mylit = $parent->{tag} eq 'substitution_definition' ? $dtext :
		$lit;
	    my @dir = eval {
		&{$DIRECTIVES{$directive}}
		($dname, $parent, $source, $lineno, $dtext, $mylit); };
	    push(@dom, system_message(4, $source, $lineno,
				      qq(Error processing directive "$dname": $@),
				      $lit))
		 if $@;
	    my @doms = grep(/^DOM/, @dir);
	    push(@unprocessed,
		 map(split(/^(\s*\n)+/m, $_),grep(!/^DOM/, @dir)));
	    if (@doms >= 1 && $doms[0]{tag} eq 'system_message' || @dir == 0)
	    {
		push(@dom, @doms);
		push(@dom, system_message(2, $source, $lineno,
					  qq($errmsgid "$subst" empty or invalid.),
					  $lit))
		    if $subst ne '';
	    }
	    else {
		$parent->append(@doms);
		if ($parent->{tag} eq 'substitution_definition') {
		    my $err = RegisterName($parent, $source, $lineno);
		    push (@dom, $err) if $err;
		}
		$error = 0;
	    }
	}
	else {
	    push(@dom, system_message(3, $source, $lineno,
				      qq(Unknown directive type "$dname".),
				      $subst eq '' ? $lit : $dtext));
	    push(@dom, system_message(2, $source, $lineno,
				      qq($errmsgid "$subst" empty or invalid.),
				      $lit))
		if $subst ne '';
	}
    }
#print STDERR "Directive -> [",join(',',@dom),"][",join(',',@unprocessed),"]\n";
    return ($error, \@dom, \@unprocessed);
}

# Processes a enumerated list paragraph.
# Arguments: paragraph, source, line number
# Returns: processed paragraph, list of DOM objects and unprocessed paragraphs
sub EnumList {
    my($para, $source, $lineno) = @_;

    my $lines = 0;
    my ($processed, @unp, @err);
    $para =~ /^$ENUM .*\n(?=\Z|\n| |$ENUM)/o;
    my ($prefix, $index, $suffix) = map defined $_ ? $_ : '', ($1, $2, $3);
    my $type = EnumType($index);
    $type = 'arabic' if $type eq '#';
    my $dom = new DOM('enumerated_list', enumtype=>$type,
		      prefix=>"$prefix", suffix=>$suffix);
    my $val = EnumVal($index, $type);
    $val = 1 if $val eq '#';
    if ($val != 1) {
	$dom->{attr}{start} = $val;
	push(@err,
	     system_message(1, $source, $lineno,
			    qq(Enumerated list start value not ordinal-1: "$index" (ordinal $val))));
    }

    while ($para =~ /^$ENUM .*\n(?=\Z|\n| |$ENUM)/o) {
	my $next = '';
	my $li = new DOM('list_item');
	$dom->append($li);
	$para =~ s/^($ENUM )\s*//o;
	my $marker = $1;
	my $spaces = " " x length($marker);
	# See if there are any subsequent enumerated lists
	if ((my @s = split /^((?!\A)$ENUM .*\n(?=\Z|\n| |$ENUM))/om, $para, 2)
	    > 1) {
	    $para = $s[0];
	    $next = "$s[1]$s[-1]";
	}

	$para =~ s/^$spaces//mg;
	Paragraphs($li, $para, $source, $lineno+$lines);
	$lines += $para =~ tr/\n//;
	$processed .= $para;
	$para = $next;
    }

    return ($processed, $dom, @err, @unp);
}

# Given the initial index of an enumerated list, returns the enumeration type.
# Arguments: Initial index
# Returns: one of "arabic", "loweralpha", "upperalpha", "lowerroman", 
#          "upperroman" or "#" (for auto-enumerated)
sub EnumType {
    my ($index) = @_;
    BEGIN { @ENUM_STRINGS = ('arabic', 'loweralpha', 'upperalpha',
			     'lowerroman', 'upperroman', '#'); }
    my @matches = 
	$index=~/^(?:([0-9]+)|([a-hj-z])|([A-HJ-Z])|([ivxlcdm]+)|([IVXLCDM]+))|(\#)$/;
    my @defs = grep(defined $matches[$_], 0 .. 5);
    my $type = defined $defs[0] ? $ENUM_STRINGS[$defs[0]] : 'error';
    return $type;
}

# Given an index of an enumerated and the enumeration type, returns the
# index value.
# Arguments: Index, enumerated type
# Returns: number or -1 (for badly formatted Roman numerals/arabic)
sub EnumVal {
    my ($index, $enumtype) = @_;
    BEGIN {
	@ALPHA_INDEX{'a' .. 'z'} = (1 .. 26);
	%ROMAN_VALS = (i=>1, v=>5, x=>10, l=>50, c=>100, d=>500, m=>1000);
    }
    # Handle autonumber
    return $index if $index eq '#';
    # First handle arabic
    return $index =~ /^\d+$/ ? $index : -1 if $enumtype eq 'arabic';
    # Deal with alpha types
    $index =~ tr/A-Z/a-z/;
    return defined $ALPHA_INDEX{$index} ? $ALPHA_INDEX{$index} : -1
	if $enumtype =~ /alpha/;
    # Now left with roman numerals
    return -1 if $index !~ /^m{0,4}(?:dc{0,3}|c[dm]|c{0,3})?(?:lx{0,3}|x[lc]|x{0,3})?(?:vi{0,3}|i[vx]|i{0,3})?$/;
    my $val = 0;
    my @chars = split(//, $index);
    while (@chars) {
	my $charval = $ROMAN_VALS{shift @chars};
	if (@chars == 0 || $charval >= $ROMAN_VALS{$chars[0]}) {
	    $val += $charval;
	}
	else {
	    $val += $ROMAN_VALS{shift @chars} - $charval;
	}
    }
    return $val;
}

# Processes an explicit markup paragraph.
# Arguments: parent, paragraph, source, line number
# Returns: processed paragraph, new parent, 
#          list of DOM objects and unprocessed paragraphs
sub Explicit {
    my($parent, $para, $source, $lineno) = @_;
#print "Explicit(",join(',',@_),")\n";

    my $new_parent = $parent;
    my $lines = 0;
    my ($processed, @unp, @err, @dom);

    # Check for the end of the explicit markup block
    my $badindent = 0;
    if ((my @s = split /^(?!\A|\n|\Z| )/m, $para, 2) > 1) {
	push(@unp, $s[1]);
	$para = $s[0];
	$badindent = 1;
    }
    $processed = $para;

    $para =~ /^(?:\.\.|(__))(?: (?:\[((?:[\#*])?[\w.-]*)\] *|\|(?! )([^\|]*\S)\| *|(_.*:.*)|([\w\.-]+\s*::.*))?)?(.*)/s;
    my ($anon, $footnote, $subst, $target, $dir, $next) =
	($1, $2, $3, $4, $5, $6);
    my $btext = $next
	if defined $footnote || defined $subst || defined $dir;
    if (substr($para,0,3) eq "..\n") {
	my $undef;
	($anon, $footnote, $target) = ($undef) x 3;
    }
    if ($anon) {
	$target = "$anon:$next";
    }
#print "[$anon][$footnote][$target]\n";
    if (defined $footnote) {
	# It's a footnote or citation
	my %attr;
	my $tag = 'footnote';
	if ($footnote =~ /^([\#*])(.*)/) {
	    my ($auto, $name) = ($1, $2);
	    $attr{auto} = $auto eq '#' ? 1 : $auto;
	    if ($name ne '') {
		$attr{names} = [ NormalizeName($name) ];
		$attr{ids} = [ NormalizeId($name) ];
	    }
	}
	elsif ($footnote !~ /^\d+$/) {
	    $tag = 'citation';
	    $attr{names} = [ NormalizeName($footnote) ];
	    $attr{ids} = [ NormalizeId($footnote) ];
	}
	else {
	    $attr{names} = [ $footnote ];
	}
	$attr{ids} = [ Id() ] unless defined $attr{ids} ;
	my $dom = new DOM($tag, %attr);
	if ($footnote !~ /^[\#*]/) {
	    my $label = new DOM('label');
	    $label->append(newPCDATA DOM($footnote));
	    $dom->append($label);
	}
	my $err = RegisterName($dom, $source, $lineno);
	$dom->append($err) if $err;
	# Get rid of indentation spaces
	$btext =~ /^(?!\A)( +)/m;
	my $spaces = $1 || '';
	$btext =~ s/^$spaces//mg;
	my @redo;
	Paragraphs($dom, $btext, $source, $lineno);
	push(@dom, $dom);
    }
    elsif (defined $subst) {
	# It's a substitution definition
	my $dom = new DOM('substitution_definition',
			  names=>[ NormalizeName($subst, 'keepcase') ]);
	my ($err, $doms, $unp) =
	    Directive($dom, $source, $lineno, 'Substitution definition',
		      $btext, $para);
	push(@dom, @$doms);
	push(@dom, $dom) unless $err;
	$processed = '' if @$unp && $unp->[0] eq $para;
	unshift(@unp, @$unp);
    }
    elsif (defined $target) {
	# It's a hyperlink target
	my %attr;
	my $dom;
	my %char_class = ('`'=>'.', ''=>"[^:]");
	$target =~ /^(_((?:\\:|[^:])+): *)(.*)/s
	    unless $target =~ /^(_\`((?:.|\n)+)\`: *)(.*)/s;
	my ($id, $uri) = ($2 || '', $3);
	if ($id eq '_') {
	    $attr{anonymous} = 1;
	    $id = Id();
	}
	my $t = $1;
	my $indent = $anon ? 3 :
	    $uri =~ /^./ ? length($t)+($anon ? 0 : 3) :
	    do { $uri =~ /\n( +)/; length($1 || '') };
	my $spaces = ' ' x $indent;
	if ($uri =~ /^(?:\`((?:.|\n)*)\`|([\w.-]+))_$/) {
	    my $name = main::FirstDefined($1, $2);
	    # Get rid of newline-indents
	    $name =~ s/\n$spaces/ /g;
	    $attr{refname} = NormalizeName($name);
	}
	else {
	    # Get rid of newline-indents
	    $uri =~ s/\n$spaces//g;
	    $uri =~ s/\n   //g;
	    chomp $uri;
	    $uri =~ s/ *//g;
	    $uri =~ s/\\(.)/$1/g;
	    if ($uri ne '') {
		$uri = "mailto:$uri"
		    if $uri !~ /^$URIre::scheme:/ &&
		    $uri =~ /\@/ && $uri !~ /^\`.*\`$/;
		$uri = $1 if $uri =~ /^\`(.*)\`$/;
		$attr{refuri} = $uri;
	    }
	}
	$attr{names} = [ NormalizeName($id) ]
	    unless $attr{anonymous};
	$dom = new DOM('target', ids=>[ NormalizeId($id) ], %attr);
	my $err = RegisterName($dom, $source, $lineno);
	push (@dom, $err) if $err;
	push (@dom, $dom);
    }
    elsif (defined $dir) {
	# It's a directive
	my ($err, $doms, $unp) =
	    Directive($parent, $source, $lineno, 'Directive', "$dir$btext",
		      $para);
	push(@dom, @$doms);
	unshift(@unp, @$unp);
	$processed = '' if @$unp && $unp->[0] eq $para;
	$new_parent = $SEC_DOM[-1]
	    if $parent->{tag} =~ /^(document|section)$/;
    }
    else {
	# It's a comment
	$para =~ s/^(\.\.\s*)//;
	my $first = $1;
	if ($para =~ /^( +)/m) {
	    my $spaces = $1;
	    $para =~ s/^$spaces//mg;
	}
	my $dom = new DOM('comment', %XML_SPACE);
	$dom->append(newPCDATA DOM($para))
	    if $para ne '';
	$para = "$first$para";
	push(@dom, $dom);
    }
    
    if ($badindent) {
	push(@dom,
	     system_message(2, $source, $lineno + ($para =~ tr/\n//),
			    "Explicit markup ends without a blank line; unexpected unindent."))
	    unless substr($unp[-1], 0, 2) eq "..";
    }
    # Annote the dom object with source, lineno, and lit
    foreach (@dom) {
	if ($_->{tag} ne 'system_message') {
	    $_->{source} = $source;
	    $_->{lineno} = $lineno;
	    $_->{lit} = $processed;
	    chomp $_->{lit};
	}
    }
    return ($processed, $new_parent, @err, @dom, @unp);
}

# Processes a field list paragraph.
# Arguments: paragraph, source, line number
# Returns: processed paragraph, list of DOM objects and unprocessed paragraphs
sub FieldList {
    my($para, $source, $lineno) = @_;

    my $dom = new DOM('field_list');
    my $lines = 0;
    my ($processed, @unp);
    (undef, my @paras) = split /^($FIELD_LIST)/om, $para;
    while (my ($fl, $b) = splice @paras, 0, 2) {
	my ($name, $para) = "$fl$b" =~ /^:([^:\n]+): *(.*)/s;
	my $field = new DOM('field');
	$field->{source} = $source;
	$field->{lineno} = $lineno+$lines;
	$dom->append($field);
	my $n = new DOM('field_name');
	my $body = new DOM('field_body');
	$field->append($n, $body);
	$body->append(Inline($n, $name, $source, $lineno+$lines));
	# Remove initial spaces
	my @spaces = $para =~ /^(?!\A)( +)/mg;
	my $spaces = defined $spaces[0] ? $spaces[0] : '';
	foreach (@spaces) {
	    $spaces = $_ if length($_) < length($spaces);
	}
	$para =~ s/^$spaces//mg;
	Paragraphs($body, $para, $source, $lineno+$lines);
	$lines += $para =~ tr/\n//;
	$processed .= $para;
    }

    return ($processed, $dom, @unp);
}

# Takes a field list and turns it into a hash
# Arguments: text of field list
# Returns: hash reference
sub HashifyFieldList {
    my ($text) = @_;

    my %hash;
    if ($text =~ /^ *($RST::FIELD_LIST)/mo) {
	my @fields = split /^(?=:)/m, $text;
	foreach my $field (@fields) {
	    next unless $field =~ /^:([^:\n]*): *(.*)/s;
	    my ($fname,$val) = ($1, $2);
	    chomp $val;
	    $hash{$fname} = $val;
	}
    }

    return \%hash;
}

# Returns the next identifier.
# Arguments: None
# Uses global: $RST::NEXT_ID
sub Id {
    return "id" . ++$NEXT_ID;
}

# Parses inline markup.
# Arguments: DOM object of parent, text to parse, source, line number
# Returns: list of system_message DOMs if errors
sub Inline {
    my ($parent, $text, $source, $lineno) = @_;
#print STDERR "Inline($parent,$text)\n";
    
    my @problems;

    my ($is_start, $pre, $start, $next, $pending, $processed);

    ($is_start, $pre, $start, $next) = InlineStart($text);
    while ($is_start) {
	$pending .= $pre;
	$text = $next;

	# Is there an end for this start?
	my ($is_end, $mid, $end, $next1) = InlineEnd($text, $start);
	if (! $is_end) {
	    # We don't have an end
	    if ($start =~ /^(\[|)$/) {
		$pending .= "$start$mid";
		$text = $next1;
	    }
	    else {
		$lineno += $pending =~ tr/\n//;
		$pending = RemoveBackslashes($pending);
		$parent->append(newPCDATA DOM($pending))
		    if $pending ne '';
		$pending = "";
		# We have something problematic here
		my ($dom,$refid,$id) = problematic($start);
		$parent->append($dom);
		my $err = system_message(2, $source, $lineno,
					 "Inline $MARK_TAG_START{$start} start-string without end-string.",
					 "", backrefs=>[ $id ],
					 ids=>[ $refid ]);
		push (@problems, $err);
	    }
	}
	else {
	    my $lit = "$start$mid$end";
	    my %attr;
	    if ($MARK_TAG_START{$start} =~ /interpreted/ &&
		$pending =~ s/:([-\w\.]+):$//) {
		$attr{role} = $1;
		$attr{position} = 'prefix'; 
	    }

	    $lineno += $pending =~ tr/\n//;
	    $pending = RemoveBackslashes($pending);
	    $parent->append(newPCDATA DOM($pending))
		if $pending ne '';
	    $pending = '';
	    $text = $next1;
	    my @content;
	    my @errs;
	    my $tag = $MARK_TAG{"$start$end"};
	    my $implicit;
	    if (! defined $tag && $start eq '') {
		# This must be an implicit markup
		$mid = "$mid$end";
		$end = "_";
		$tag = 'reference';
		$implicit = 1;
	    }
	    if ($tag eq 'interpreted' && $text =~ s/^:([-\w\.]+)://) {
		my $role = $1;
		if (defined $attr{role}) {
		    # We have something problematic here
		    my ($dom,$refid,$id) = problematic(":$attr{role}:`$mid`:$role:");
		    $parent->append($dom);
		    my $err = system_message(2, $source, $lineno,
					     "Multiple roles in interpreted text (both prefix and suffix present; only one allowed).",
					     "", backrefs=>[ $id ],
					     ids=>[ $refid ]);
		    push (@problems, $err);
		    last;
		}
		elsif ($text =~ s/^(__?)//) {
		    # We have something problematic here
		    my ($dom,$refid,$id) = problematic("`$mid`:$role:$1");
		    $parent->append($dom);
		    my $err = system_message(2, $source, $lineno,
					     "Mismatch: both interpreted text role suffix and reference suffix.",
					     "", backrefs=>[ $id ],
					     ids=>[ $refid ]);
		    push (@problems, $err);
		    last;
		}
		else {
		    $attr{role} = $role;
		    $attr{position} = 'suffix';
		}
	    }
	    elsif ($tag =~ /reference/) {
		if (defined $attr{role}) {
		    # We have something problematic here
		    my ($dom,$refid,$id) = problematic(":$attr{role}:`$mid$end");
		    $parent->append($dom);
		    my $err = system_message(2, $source, $lineno,
					     "Mismatch: both interpreted text role prefix and reference suffix.",
					     "", backrefs=>[ $id ],
					     ids=>[ $refid ]);
		    push (@problems, $err);
		    last;
		}

		my $name = $mid;
		$name =~ s/\n/ /g;
		my $uri;

		if ($tag eq 'substitution_reference' && $end =~ /_$/) {
		    # Need to create a new level of reference
		    my $dom = new
			DOM ($tag, refname=>NormalizeName($name, 'keepcase'));
		    $dom->append(newPCDATA DOM($mid));
		    $dom->{source} = $source;
		    $dom->{lineno} = $lineno;
		    $dom->{lit} = $lit;
		    push (@content, $dom);
		    $tag = 'reference';
		    $mid = "";
		}
		$mid =~ /((?:\s|\A)<([^ <][^<]*[^ ])>)$/;
		my $embeduri = $2;
		if ((defined $embeduri || $implicit) &&
		    do {$uri = $implicit ? $mid : $embeduri;
			$uri =~ s/\s//g;
			$uri =~ /^($URIre::URI_reference|$EMAIL)$/o}) {
		    # Implicit references may pick up extra punctuation at
		    # the end.
		    if ($pre ne '<' && substr($next1,0,1) ne '>' &&
			$implicit && $uri =~ /(.*)([\)\]\};:\'\",.>\?])$/) {
			my ($newuri, $lastchar) = ($1, $2);
			if (defined $LEFT_BRACE{$lastchar}) {
			    # It's a close brace/paren/bracket/angle bracket
			    # It's part of the URI if the URI would otherwise
			    # be unmatched.
			    my $rb = "\\$lastchar";
			    my $lb = "\\$LEFT_BRACE{$lastchar}";
			    my $nleft = $newuri =~ s/($lb)/$1/g;
			    my $nright = $newuri =~ s/($rb)/$1/g;
			    if ($nleft <= $nright) {
				$mid = $uri = $newuri;
				$text = "$lastchar$text";
			    }
			}
			else {
			    $mid = $uri = $newuri;
			    $text = "$lastchar$text";
			}
		    }
		    $uri =~ s/\\(.)/$1/g; # Remove backslash quotes
		    $uri = "mailto:$uri"
			if $uri !~ /^$URIre::scheme:/o && $uri =~ /^$EMAIL$/o;
		    $attr{refuri} = $uri;
		    $mid =~ s/^<(.*)>$/$1/ if $embeduri;
		    $mid =~ s/(\s<[^<]+>)$//;
		    my $miduri = defined $1 ? $1 : '';
		    $attr{name} = $mid unless $implicit;
		    $lineno += $miduri =~ tr/\n//;
		}
		elsif ($end =~ /__/) {
		    $attr{anonymous} = 1;
		    $attr{name} = NormalizeName($mid, 'keepcase')
			if $mid ne '';
		}
		else {
		    $tag = 'citation_reference'
			if $start eq '[' && $name !~ /^([\#*].*|\d+)$/;
		    if ($tag =~ /footnote|citation/ && $name =~ /^[\#*](.*)/) {
			$attr{auto} = substr($name,0,1) eq '*' ? '*' : 1;
			$mid = "";
			$name = $1;
		    }
		    if ($name ne '') {
			$attr{refname} = $tag eq 'substitution_reference' ?
			    NormalizeName($name, 'keepcase') :
			    NormalizeName($name);
			$attr{name} = NormalizeName($name, 'keepcase')
			    if $tag eq 'reference' && $start ne '|';
		    }
		}
		if ($tag =~ /footnote|citation/) {
		    $attr{ids} = [ Id() ];
		}
	    }
	    elsif ($tag eq 'target') {
		$attr{ids} = [ NormalizeId($mid) ];
		$attr{names} = [ NormalizeName($mid) ];
	    }
	    # Do parse-time interpretations of interpreted text
	    my $was_interpreted;
	    my $suffix;
	    if ($tag eq 'interpreted') {
		$lit = $attr{position} eq 'prefix' ?
		    ":$attr{role}:$lit" : "$lit:$attr{role}:"
		    if defined $attr{role};
		$attr{role} = $MY_DEFAULT_ROLE if ! defined $attr{role};
		if (defined $attr{role} && defined $MY_ROLES{$attr{role}}) {
		    my $role = $MY_ROLES{$attr{role}};
		    $role = $MY_ROLES{$role->{alias}}
  			while defined $role->{alias};
		    my @errs = &{$role->{check}}($mid, $lit, $parent,
						 $source, $lineno, $attr{role})
			if defined $role->{check};
#		    delete $attr{role};
		    delete $attr{position};
		    if (@errs) {
			push @problems, @errs;
			last;
		    }
		    $tag = $role->{tag};
		    if (defined $role->{attr}) {
			foreach my $attr (keys %{$role->{attr}}) {
			    $attr{$attr} =
				ref($role->{attr}{$attr}) eq 'ARRAY' ?
				$role->{attr}{$attr} :
				sprintf $role->{attr}{$attr}, $mid;
			}
		    }
		    $mid = sprintf $role->{text}, $mid
			if defined $role->{text};
		    my $options = $role->{options};
		    if ($options && $options->{prefix} &&
			defined (my $pfx = ($options->{prefix}{$main::opt_w} ||
					    $options->{prefix}{default}))) {
			my $raw = new DOM('raw', format=>$main::opt_w);
			$raw->append(newPCDATA DOM($pfx));
			$parent->append($raw);
		    }
		    if ($options && $options->{suffix} &&
			defined (my $sfx = ($options->{suffix}{$main::opt_w} ||
					    $options->{suffix}{default}))) {
			$suffix = new DOM('raw', format=>$main::opt_w);
			$suffix->append(newPCDATA DOM($sfx));
		    }
		    $was_interpreted = 1;
		}
		elsif (defined $attr{role}) {
		    # We have something problematic here
		    my ($dom,$refid,$id) = problematic($lit);
		    $parent->append($dom);
		    push @problems, UnknownRole($attr{role}, $source, $lineno, '',
						backrefs=>[ $id ],
						ids=>[ $refid ]);
		    last;
		}
	    }
	    my $dom = new DOM($tag, %attr);
	    $dom->{source} = $source;
	    $dom->{lineno} = $lineno;
	    $dom->{lit} = $lit;
	    if ($attr{role}) {
		$dom->{role} = $attr{role};
		delete $dom->{attr}{role};
	    }
	    if ($tag eq 'target') {
		my $err = RegisterName($dom, $source, $lineno);
		push (@problems, $err) if $err;
	    }
	    $parent->append($dom);
	    $parent->append($suffix) if $suffix;
	    if ($tag =~ /^(literal)$/ && ! $was_interpreted || $implicit ||
		$tag eq 'raw' || ! $main::opt_D{nestinline}) {
		$mid = RemoveBackslashes($mid)
		    if $tag !~ /^(literal|raw)$/;
		$dom->append(newPCDATA DOM($mid))
		    if $mid ne '';
	    }
	    else {
		@errs = Inline($dom, $mid, $source, $lineno)
		    if $mid ne '';
		push @problems, @errs;
	    }
	    $dom->append(@content);
	    if ($tag eq 'reference' && defined $attr{refuri} &&
		$end !~ /__/ && ! $implicit && ! $was_interpreted) {
		my $dom = new DOM('target',
				  refuri=>$attr{refuri},
				  ids=>[ NormalizeId($mid) ],
				  names=>[ NormalizeName($mid) ]);
		my $err = RegisterName($dom, $source, $lineno);
		push (@problems, $err) if $err;
		$parent->append($dom);
	    }
	    $lineno += $mid =~ tr/\n//;
	}

    } continue { 
	($is_start, $pre, $start, $next) =
	    InlineStart($text, substr($pending, -1));
    }

    $pending .= $text;
    $pending = RemoveBackslashes($pending);
    $parent->append(newPCDATA DOM($pending))
	if $pending !~ /^(\n|)$/;

    return @problems;
}

# Finds the matching end mark for an inline start mark.  Works even if there
# is intervening nested markup.
# Arguments: text, start mark, start mark of outer nesting (may be null)
# Returns: boolean indicating match found, text between marks, end mark,
#          text after end mark
sub InlineEnd {
#print STDERR ++$dbg::ienest," InlineEnd(",join(',',@_),")\n";
    my ($text, $start, $outer_start) = @_;
    my ($is_end, $orig_mid, $end, $orig_next) =
	InlineFindEnd($text, $start, $outer_start);
    my ($full_mid, $next) = ($orig_mid, $orig_next);
    goto do_return
	unless $is_end && $main::opt_D{nestinline} && $start ne '``';

    # We only need to recurse to get it right if the start symbol is
    # "*" or "`" (emphasis or interpreted/target)
    my ($is_start, $pre, $nest_start, $next2) = InlineStart($text)
	if $start =~ /(\*|\`)$/;
    my $full_pre = $pre;
    while ($is_start && length($full_pre) < length($full_mid)) {
#print STDERR "[$is_start][$full_pre][$full_mid]\n";
	my ($nest_is_end, $nest_mid, $nest_end, $nest_next) =
	    InlineEnd($next2, $nest_start, $start);
	if (! $nest_is_end && $nest_start eq '') {
	    # Check for a later start
	    $text = "$nest_start$next2";
	    $full_pre .= substr($text,0,1);
	    $text = substr($text,1);
	    ($is_start, $pre, $nest_start, $next2) =
		InlineStart($text, substr($full_pre, -1));
	    $full_pre .= $pre if $is_start;
	}
	else {
	    # Skip over the nested start
	    $full_pre .= "$nest_start$nest_mid$nest_end";
	    $full_mid = $full_pre;
	    $text = $nest_next;

	    # Look for the next start/end pair
	    my ($new_end, $mid);
	    # What we thought was our end may have been swallowed up by
	    # the nested start; find the new end.
	    ($nest_is_end, $mid, $new_end, $next) =
		InlineFindEnd($text, $start);
	    if (! $nest_is_end) {
		($full_mid, $next) = ($orig_mid, $orig_next);
		last;
	    }
	    $full_mid .= $mid;
	    ($is_start, $pre, $nest_start, $next2) =
		InlineStart($text, substr($full_pre, -1));
	    $full_pre .= $pre if $is_start;
	    ($orig_mid, $orig_next, $end) = ($full_mid, $next, $new_end);
	}
    }

 do_return:
#print STDERR $dbg::ienest--," InlineEnd->[$is_end][$full_mid][$end][$next]\n";
    return ($is_end, $full_mid, $end, $next);
}

# Finds the first possible matching end mark for an inline start mark.  Does
# not take into account intervening nested markup.
# Arguments: text, start mark, start mark of outer nesting (may be null)
# Returns: boolean indicating match found, text between marks, end mark,
#          text after end mark
sub InlineFindEnd {
#print STDERR "InlineFindEnd(",join(',',@_),")\n";
    my ($text, $start, $nest_start, $null_string_ok) = @_;
    my $match = 0;
    my ($mid, $end, $after, @problems) = ('', '', '');
    my $nest_trailer = defined $nest_start && defined $MARK_END{$nest_start} ?
	$MARK_END{$nest_start} : "\001";
    while (! $match &&
	   (my @s = split /((\S|\A)($MARK_END{$start})(?=$MARK_END_TRAILER|$nest_trailer|\n))/, $text, 2) > 1) {
	my ($next, $after);
	($next, $end,$after) = ("$s[0]$s[2]", $s[3], $s[-1]);
#print STDERR "$lineno: #$start#$next#$end#$s[-1]\n";
	if (("$start$end" =~ /^_/ && $start ne '_`' &&
	     ($next =~ /([\._-])\1/ || $next =~ /[\`\]]$/)) ||
	    ($start eq '[' && $next !~ /^(?=.)[\#\*]?[\w\.-]*$/)) {
	    $mid = "$next$end";
	    $text = $after;
	    last;
	}
	$match = 1;
	my @content;
	if ($MARK_TAG_START{$start} ne 'literal') {
	    if (substr($next,-1, 1) eq "\\") {
		# It's backslash quoted; not a real end mark
		substr($next,-1, 1) = "\\$end";
		$match = 0;
	    }
	}
	$mid = "$mid$next";
	$text = $after;
    }
#print STDERR "InlineFindEnd -> [$match][$mid][$end][$text]\n";
    return ($match, $mid, $end, $text);
}

# Finds the place where inline markup starts.
# Arguments: text, character preceding text
# Returns: Boolean indicating markup was found, characters preceding markup 
#          start, start markup string, characters after markup start
sub InlineStart {
#print STDERR "InlineStart(",join(',',@_),")\n";
    my ($text, $previous) = @_;
    my $arg = $text;
    my $pre = '';

    while (#do{print STDERR "[$text]\n";1} &&
	   $text =~ m(^(.*?)(^|[-\'\"\(\[\{</: ])(?:($MARK_START)(\S)|(?=(?=[^-_\'\"\(\[\{</: \\\n])(\S*[^\\\s])__?($MARK_END_TRAILER|\n))|(?=$URIre::absoluteURI|$EMAIL))(.*))mos) {
	my ($processed, $prechar,$start,$postchar,$after) =
	    map defined $_ ? $_ : '', ($1,$2,$3,$4,$7);
	my $pending = $start if $start eq '[';
	$previous = substr($pre, -1) if $pre ne '';
	my $pchar = $prechar eq '' ? $previous : $prechar;
#print STDERR "<$pchar><$start><$postchar><$after>\n";
	my $validstart = 1;
	if (defined $pchar && defined $MATCH_BRACE{$pchar} &&
	    $postchar eq $MATCH_BRACE{$pchar} ||
	    $postchar eq $start && $start ne '') {
	    # It's within quotes
	    $processed .= "$prechar$start$postchar";
	    $text = $after;
	    $validstart = 0;
	}
	elsif ($start  eq '' && defined $pchar &&
	       $pchar !~ m!(\A|[-\'\"\(\[\{</: \\])$!) {
	    # It seems to be a reference, but prechar isn't allowed
	    my @s = split m!(([-\'\"\(\[\{</: \\])|(__?)(?=$MARK_END_TRAILER|\n))!, $text,2 ;
	    $pchar = $s[2] if defined $s[2];
	    my $anon = defined $s[3] ? $s[3] : '';
	    ($processed,$after) = ("$processed$s[0]$pchar$anon","$s[-1]");
	    $validstart = 0;
	    $text = $after;
	}
	elsif ($start eq '' &&
	       (my @s = split /(($URIre::scheme):(?:$URIre::hier_part|$URIre::opaque_part))/o, 
		$text, 2) > 1) {
	    # It seems to be implicit markup, but it's not a recognized scheme
	    my $scheme = $s[2];
	    if (! $IMPLICIT_SCHEME{$scheme}) {
		($processed, $after) = ("$s[0]$s[1]", "$s[-1]");
		$validstart = 0;
		$text = $after;
	    }
	    else {
		$processed = "$processed$prechar";
	    }
	}
	else {
	    $processed = "$processed$prechar";
	    $after = "$postchar$after";
	}
	$pre .= $processed;
#print STDERR "InlineStart -> [$validstart][$pre][$start][$after]\n" if $validstart; 
	return ($validstart, $pre, $start, $after) if $validstart;
    }

#print STDERR "InlineStart -> 0\n";
    return 0;
}

# Returns whether a reStructuredText string represents a valid table object.
# Arguments: string
# Returns: true or false
sub IsTable {
    my ($text) = @_;

    chomp $text;
    my @lines = split(/\n/, $text);
    my $first = $lines[0];
    return 0 unless defined $first;
    # Check for a validly constructed grid table
    if ($first =~ /^[+]([-=]+[+])+ *$/) {
	my $l;
	for ($l=1; $l < @lines; $l++) {
	    $_ = $lines[$l];
	    return 0 unless /^[|+].*[|+] *$/;
	    return 1 if $l > 1 && /^[+][+-]+[+] *$/;
	}
	return 0;
    }
    # Check for a validly constructed simple table
    elsif ($first =~ /^=+( +=+)+ *$/) {
	return 1;
    }
    return 0;
}

# Processes a line block paragraph.
# Arguments: paragraph, source, line number
# Returns: processed paragraph, list of DOM objects and unprocessed paragraphs
sub LineBlock {
    my($para, $source, $lineno) = @_;

    my @err;
    my $lines = 0;
    my ($processed, @unp);
    my $dom = new DOM('line_block');
    # Calculate our minimum indentation
    my @indents = map length $_, $para =~ /^(?:$LINE_BLOCK)( +)\S/gm;
    my $indent = $indents[0];
    grep do { $indent = $_ if $_ < $indent }, @indents;
    my $spaces = ' ' x $indent;
    my @paras = split /^$LINE_BLOCK(?:$spaces(\S))/m, $para;
    my $prev = shift @paras || '';
    if ($prev) {
	if ($prev =~ /^$LINE_BLOCK *$/) {
	    my $li = new DOM('line');
	    $dom->append($li);
	}
	else {
	    # Start with another line block
	    $prev =~ s/^$LINE_BLOCK$spaces/|/gm;
	    my ($prevproc, @prevdom) =
		LineBlock($prev, $source, $lineno+$lines);
	    $dom->append(grep ref $_ eq 'DOM', @prevdom);
	}	    
	$lines += $prev =~ tr/\n//;
	$processed .= $prev;
    }
    while (my ($pfx, $para) = splice @paras, 0, 2) {
	my $li = new DOM('line');
	$dom->append($li);
	$para = "$pfx$para";
	# Check for nested line blocks
	my $nest;
	if ((my @s = split /^($LINE_BLOCK(?: +\S))/mo, $para, 2) > 1) {
	    ($para, $nest) = ($s[0], "$s[1]$s[-1]");
	}	    
	# Check for immediately following blank lines
	($para, my @s) = split /^$LINE_BLOCK *$/m, $para;
	$para =~ s/^  +//mg;
	push @err, Inline($li, $para, $source, $lineno+$lines);
	$lines += $para =~ tr/\n//;
	$processed .= $para;
 	foreach (@s) {
 	    my $li = new DOM('line');
 	    $dom->append($li);
 	    $lines++;
 	    $processed .= "$LINE_BLOCK\n";
 	}
	if ($nest) {
	    # Process nested line block
	    $nest =~ s/^$LINE_BLOCK$spaces/|/gm;
	    my ($nestproc, @nestdom) =
		LineBlock($nest, $source, $lineno+$lines);
	    $dom->append(grep(ref $_ eq 'DOM', @nestdom));
	    $lines += $nest =~ tr/\n//;
	    $processed .= $nest;
	}
    }
    return ($processed, $dom, @err, @unp);
}

# Normalizes an attribute by putting it in lower case and replacing sequences
# of special characters with hyphens.
# Arguments: string, implicit
# Returns: normalized string
sub NormalizeId {
    my ($s, $implicit) = @_;
    $s = '' unless defined $s;
    chomp $s;
    # Get rid of any initial numbering of implicit targets
    $s =~ s/^(\d+\.)+\s+// if $implicit;
    $s =~ s/\n/ /g;
    $s = NormalizeName($s);
    # Get rid of special characters
    $s =~ s/[^\w\s\'\.-]//g;
    # Translate sequences of spaces to a single hyphen
    $s =~ s/[\s\'\._]+/-/g;
    $s =~ s/^-|-$//g;
    $s = Id() if $s eq '';
    return $s;
}

# Normalizes an attribute by putting it in lower case and replacing sequences
# of spaces with a single space.
# Arguments: string, flag to keep case
# Returns: normalized string
sub NormalizeName {
    my ($s, $keepcase) = @_;
    return unless defined $s;
    chomp $s;
    # Remove backslash-space combos
    $s =~ s/\\ //g;
    # Remove initial spaces
    $s =~ s/^\s+//;
    # Remove trailing spaces
    $s =~ s/\s+$//;
    # Translate to lower case
    $s = lc $s unless $keepcase;
    # Convert strings of spaces to a single space
    $s =~ s/[\s]+/ /g;
    # Remove inline markup characters
    $s =~ s/(^|(?!\\).)([*\`|])/$1 eq "\\" ? "$1$2" : $1/ge;
    $s =~ s/(^|(?!\\).)([*\`|])/$1 eq "\\" ? "$1$2" : $1/ge;
    # Handle backslashes
    $s =~ s/\\(.)/$1/g;
    return $s;
}

# Processes an option list paragraph.
# Arguments: paragraph, source, line number
# Returns: processed paragraph, list of DOM objects and unprocessed paragraphs
sub OptionList {
    my($para, $source, $lineno) = @_;

    my $dom = new DOM('option_list');
    my ($processed, @unp);
    (undef, my @paras) =
	split /^((?:$OPTION)(?:, (?:$OPTION))*(?:  |\s*\n ))/om, $para;
    while (my($pfx, $para) = splice @paras, 0, 2) {
	$pfx =~ /^((?:$OPTION)(?:, (?:$OPTION))*)(  |\s*\n )/o;
	my ($options, $sep) = ($1, $2);
	$para = "$sep$para";
	my @options = split(/, /, $options);
	my $oli = new DOM('option_list_item');
	$dom->append($oli);
	my $og = new DOM('option_group');
	$oli->append($og);
	my $option;
	foreach $option (@options) {
	    $option =~ /^(-.)()(<[^>]+>)/ ||
	    $option =~ /^([^ =]+)(?:([= ])(.*))?/;
	    my ($string, $del, $argument) = ($1, $2, $3);
	    my $opt = new DOM('option');
	    $og->append($opt);
	    my $os = new DOM('option_string');
	    $os->append(newPCDATA DOM($string));
	    $opt->append($os);
	    if (defined $argument) {
		my $oa = new DOM('option_argument', delimiter=>$del);
		$oa->append(newPCDATA DOM($argument));
		$opt->append($oa);
	    }
	}
	my $desc = new DOM('description');
	$oli->append($desc);
	my $proc = "$options$para";
	# Remove initial spaces
	$para =~ s/^ +//;
	my @spaces = $para =~ /^(?!\A)( +)/mg;
	my $spaces = defined $spaces[0] ? $spaces[0] : '';
	foreach (@spaces) {
	    $spaces = $_ if length($_) < length($spaces);
	}
	$para =~ s/^$spaces//mg;
	Paragraphs($desc, $para, $source, $lineno);
	$lineno += $proc =~ tr/\n//;
	$processed .= $proc;
    }

    return ($processed, $dom, @unp);
}

# Recursively parses a reStructuredText string that does not contain sections.
# Arguments: DOM object of parent, text to parse, source, start line number
# Returns: None, but appends parsed objects to the parent
sub Paragraphs {
    my ($parent, $text, $source, $lineno) = @_;
#$INDENT .= " ";

    return unless defined $text;
    # Convert any tabs to spaces
    while ($text =~ s/^([^\t\n]*)\t/
	   my $l = length($1);
	   my $ts = $main::opt_D{tabstops};
	   my $s = " " x ($ts - ($l % $ts));
	   "$1$s"/gem) {
    }
    # Convert form feeds and vertical tabs to spaces
    $text =~ s/[\013\014]/ /g;
    # To aid keeping simple tables together, we do an initial pass
    # over the file quoting all the lines in simple tables to begin
    # with a control-A (octal 001) character.
    $text = QuoteSimpleTables($text);

#print "${INDENT}Paragraphs(",join(',',@_),")\n";
    # Split into paragraphs
    my @para = map(do{s/^ +$//;$_},split(/^(\s*\n)+/m, $text));
    
    my $processed;
    my $exp_literal = 0; # Are we expecting a literal block
    my $doc_sec = $parent->{tag} eq 'section' ? "Section" :
	"Document or section";
    my $para;
    my $new_literal = 0; # Will we expect a literal block next time
    my @unprocessed;
    while (@para) {
	my @dom;
#print STDERR "[",join("][",@para),"]";
	Coalesce(\@para);
#print STDERR "->[",join("][",@para),"]\n";

	$para = shift(@para);
#print STDERR "[$para]\n";

	my $dom;
	my $got_literal; # Did we get a literal block
	@unprocessed = ();

 	if ((my @s = split /^$SECTION_HEADER/om, $para, 2) > 1) {
 	    if ($s[0] ne '') {
 		$para = $s[0];
 		unshift(@para, "$s[1]$s[-1]");
 	    }
	    else {
		if ($exp_literal) {
		    $parent->append
			(system_message(2, $source, $lineno,
					"Literal block expected; none found."));
		}

		my ($new_parent, $unp, @result);

		($para, $unp, $new_parent, @result) =
		    SectionBreaks($parent, $para, $source, $lineno, $para[0]);
		$parent->append(@result);
		if ($para eq '') {
		    $para = $unp;
		}
		else {
		    $parent = $new_parent;
		    $doc_sec = "Section";
		    push(@unprocessed, $unp) if $unp ne '';
		    next;
		}
	    }
	}

	if ($para =~ /^(\s*\n)*$/s) {
	    $new_literal = $got_literal = $exp_literal;
	}
	# Check for error sentinels
	elsif ($para =~ s/^\n//) {
	    push (@dom, eval($para));
	    $para = '';
	    $new_literal = $got_literal = $exp_literal;
	}
	# Check for explicit markup blocks
	elsif ($para =~ /^(?:\.\.|(__))( |\n)/s) {
	    my @result;
	    ($para, $parent, @result) = Explicit($parent, $para, $source,
						 $lineno);
	    push(@dom, grep(ref($_) eq 'DOM', @result));
	    unshift(@para, grep(ref($_) ne 'DOM', @result));
	}
	# Check for bulleted lists
	elsif ($para =~ /^($BULLETS)(?: |\n)/o) {
	    my @result;
	    ($para, @result) = BulletList($para, $source, $lineno);
	    push(@dom, grep(ref($_) eq 'DOM', @result));
	    unshift(@para, grep(ref($_) ne 'DOM', @result));
	}
	# Check for line blocks
	elsif ($para =~ /^($LINE_BLOCK)(?: |\n)/o) {
	    my @result;
	    ($para, @result) = LineBlock($para, $source, $lineno);
	    push(@dom, grep(ref($_) eq 'DOM', @result));
	    unshift(@para, grep(ref($_) ne 'DOM', @result));
	}
	# Check for enumerated lists
	elsif ($para =~ /^$ENUM .*\n(?=\Z|\n| |$ENUM)/o) {
	    my @result;
	    ($para, @result) = EnumList($para, $source, $lineno);
	    push(@dom, grep(ref($_) eq 'DOM', @result));
	    unshift(@para, grep(ref($_) ne 'DOM', @result));
	}
	# Check for doctest blocks
	elsif ($para =~ /^>>> /) {
	    my $dom = new DOM('doctest_block', %XML_SPACE);
	    $dom->append(newPCDATA DOM($para));
	    push(@dom, $dom);
	}
	# Check for field lists
	elsif ($para =~ /^$FIELD_LIST/o) {
	    my @result;
	    ($para, @result) = FieldList($para, $source, $lineno);
	    push(@dom, grep(ref($_) eq 'DOM', @result));
	    unshift(@para, grep(ref($_) ne 'DOM', @result));
	}
	# Check for option lists
	elsif ($para =~ /^$OPTION_LIST/o) {
	    my @result;
	    ($para, @result) = OptionList($para, $source, $lineno);
	    push(@dom, grep(ref($_) eq 'DOM', @result));
	    unshift(@para, grep(ref($_) ne 'DOM', @result));
	}
	# Check for tables
	elsif ($para =~ /^(=+( +=+)+|[+](-+[+])+) *\n/ && IsTable($para)) {
	    push(@dom, Table($para, $source, $lineno));
	}
	# Check for block quotes 
	elsif (substr($para, 0, 1) eq ' ') {
	    # It's indented: it must be a block quote or indented literal
	    my $dom;
	    if ($exp_literal) {
		$dom = new DOM('literal_block', %XML_SPACE);
	    }
	    else {
		$dom = new DOM('block_quote');
	    }
	    push @dom, $dom;
	    # Compute the minimum indent of my lines
	    my $min_indent = 0xffff;
	    my @spaces = $para =~ /^( *)\S/mg;
	    foreach (@spaces) {
		my $len = length($_);
		last if $len == 0;
		$min_indent = $len if $len < $min_indent;
	    }
	    # Make sure nothing is unindented
	    my $badindent = 0;
	    if ((my @s = split /^(\S)/m, $para, 2) > 1) {
		unshift(@para, "$s[1]$s[-1]");
		$para = $s[0];
		$badindent = 1;
	    }
	    my $spaces = ' ' x $min_indent;
	    $para =~ s/^$spaces//mg;
	    if ($exp_literal) {
		$dom->append(newPCDATA DOM($para));
	    }
	    else {
		# Check for an attribution
		my $attr = $1
		    if ($dom->{tag} eq 'block_quote' &&
			$para =~ s/\n(?:---?|\\u2014)(?!-) *(.*(\n( *).*(\n\2\S.*)*)?\Z)/\n/m);
		Paragraphs($dom, $para, $source, $lineno);
		if (defined $attr) {
		    my ($spaces) = $attr =~ /\n( *)/;
		    $attr =~ s/^$spaces//gm if defined $spaces;
		    my $attribution = new DOM('attribution');
		    $dom->append($attribution);
		    $attribution->append(newPCDATA DOM($attr));
		    $para .= $attr;
		}
	    }
	    my $block = $exp_literal ? "Literal block" : "Block quote";
	    unshift(@para,
		    "\n" .
		    qq(system_message(2, \$source, \$lineno,
				      "$block ends without a blank line; unexpected unindent.")))
		if $badindent;
	    $got_literal = 1;
	}
	# Check for quoted literals (must precede check for definition lists)
	elsif ($exp_literal && $para =~ /^(($SEC_CHARS)[^\n]*\n(?:\2[^\n]*\n)*)(.*)/so) {
	    my ($lit, $quote, $next) = ($1, $2, $3);
	    my $dom = new DOM('literal_block', %XML_SPACE);
	    push @dom, $dom;
	    $dom->append(newPCDATA DOM($lit));
	    if ($next ne '') {
		unshift @para, $next;
		if (substr($next, 0, 1) eq ' ') {
		    unshift(@para, "\n" .
			    qq(system_message(3, \$source, \$lineno,
					      "Unexpected indentation.")));
		}
		else {
		    unshift(@para, "\n" .
			    qq(system_message(3, \$source, \$lineno,
					      "Inconsistent literal block quoting.")));
		}
	    }
	    $para = $lit;
	    $got_literal = 1;
	}
	# Check for definition lists
	elsif ($para =~ /^(\S.*)\n( +)/) {
	    my @result;
	    ($para, @result) = DefinitionList($para, $source, $lineno);
	    push(@dom, grep(ref($_) eq 'DOM', @result));
	    unshift(@para, grep(ref($_) ne 'DOM', @result));
	}
	# Check for transitions 
	elsif ($para =~ /^(($SEC_CHARS)\2\2\2+)$/o) {
	    if (length($1) < 4) {
		push(@dom, system_message(1, $source, $lineno,
					  "Unexpected possible title overline or transition.\nTreating it as ordinary text because it's so short."));
		my $p = new DOM('paragraph');
		$p->append(newPCDATA DOM($para));
		push(@dom, $p);
	    }
	    elsif ($parent->{tag} !~ /^(document|section)$/) {
		push(@dom, system_message(4, $source, $lineno,
					  "Unexpected section title or transition.",
					  $para));
	    }
	    else {
		my $last_sibling = $parent->num_contents() ?
		    $parent->{content}[-1] : {};
		my $transition = new DOM('transition');
		push(@dom, $transition);
		$transition->{source} = $source;
		$transition->{lineno} = $lineno;
	    }
	}
	# It must just be a paragraph
	else {
	    my $p = new DOM('paragraph');
	    my $pre;
	    if ((($pre) = $para =~ /(.*):: *$/s) &&
		(! defined $pre || $pre !~ /(^|[^\\])(\\\\)*\\$/)) {
		# We've got a literal block tagged on to us
		$new_literal = 1;
		$para =~ s/(^|.)(\s*):: *\n$/!$1 ? '' : $2 ? "$1\n" : "$1:\n"/e;
	    }
	    if ($para ne "") {
		push(@dom,($p, Inline($p, $para, $source, $lineno)));
		# Clean up trailing whitespace
		$p->{content}[-1]{text} =~ s/ +$//
		    if defined $p->{content}[-1]{text};
	    }
	}
	if ($exp_literal && ! $got_literal) {
	    $parent->append
		(system_message(2, $source, $lineno,
				"Literal block expected; none found."));
	}
	$parent->append(@dom);
    }
    continue {
	$exp_literal = $new_literal;
	$lineno += $para =~ tr/\n//;
	$processed .= $para;
	$new_literal = 0;
	# Push unprocessed information back to front of list
	my @unp;
	foreach (@unprocessed) {
	    my @p = split(/^(\s*\n)+/m, $_);
	    push (@unp, @p);
	}
	unshift (@para, @unp);
    }

    if ($exp_literal) {
	$parent->append
	    (system_message(2, $source, $lineno,
			    "Literal block expected; none found."));
    }
}

# Parses a reStructuredText document.
# Arguments: First line of file, whether we're at end of file
# Returns: DOM object
# Uses globals: <> file handle
sub Parse {
    my ($first_line, $eof) = @_;
    my $next_first_line;
    my $source = defined $main::opt_D{source} ? $main::opt_D{source} :
	$ARGV;
    my @file;
    if (! $eof) {
	while (<>) {
	    push @file, $_;
	    if (eof) {
		close ARGV;
		$next_first_line = <>;
		$eof = eof;
		last;
	    }
	}
    }
    my $file = join('',@file);
    my $dom = new DOM ('document', source=>$source);
    $dom->{source} = $source;
    my $text = "$first_line$file";

    init($dom);
    Paragraphs($dom, $text, $source, 1);

    # Do transformations on the DOM
    use Transforms;
    my $transform;
    foreach $transform (@Transforms::TRANSFORMS) {
	next if (defined $main::opt_D{xformoff} &&
		 $transform =~ /$main::opt_D{xformoff}/o);
	my $t = $transform;
	$t =~ s/\./::/g;
	if (! defined &$t) {
	    $dom->append
		(system_message(4, $source, 0,
				qq(No transform code found for "$transform".)));
	}
	else {
	    no strict 'refs';
	    &$t($dom);
	}
    }
    
    return $dom, $next_first_line, $eof;
}

# Quotes lines involved in simple tables that could be confused with
# section headers by starting them with a control-A (octal 001)
# character.  Works only with properly formatted tables (ones followed
# by a blank line).
# Arguments: text string
# Returns: quoted text string
sub QuoteSimpleTables {
    my($text) = @_;
    return "" unless defined $text;
    my $processed = '';
    while ((my @s = split /^(=+( +=+)+ *\n)/m, $text, 2) > 1) {
	my $line = $s[1];
	my $len = length($line);
	$processed = "$processed$s[0]$s[1]";
	$text = "$s[-1]";
	if ((my @s = split /^((=[ =]+= *\n)(\n|\Z))/m, $text, 2) > 1) {
	    my $table = "$s[0]$s[2]";
	    $text = "$s[3]$s[-1]";
	    $table =~ s/^/\001/gm;
	    $processed = "$processed$table";
	}
    }
    return "$processed$text";
}

# Registers a target name and returns an error DOM if it is an illegal
# duplicate.
# Arguments: target DOM, source, line number
# Returns: optional error DOM
# Uses globals: %TARGET_NAME
sub RegisterName {
    my ($dom, $source, $lineno) = @_;
    my $error = '';
    my $casename = defined $dom->{attr}{names} ? $dom->{attr}{names}[0] : '';
    my $name = lc $casename;
    push(@ANONYMOUS_TARGETS, $dom) if $dom->{attr}{anonymous};

    my $tag = $dom->{tag};
    if ($tag =~ /^(footnote|substitution|citation)/) {
	$REFERENCE_DOM{$tag}{$casename} = 
	    $REFERENCE_DOM{"$tag.lc"}{$name} = $dom
	    if $casename ne '';
	$REFERENCE_DOM{$tag}{$dom->{attr}{ids}[0]} = $dom
	    if defined $dom->{attr}{ids};
    }
    return unless defined $name;
    my $uri = $dom->{attr}{refuri};
    my $level = 1;
    my $target;
    my %tags;
    BEGIN {%NAMESPACE = (section=>'target', substitution_definition=>'subst',
			 );
	   %CITSPACE= (); }# citation=>'footcit', footnote=>'footcit');}
    my $space = $NAMESPACE{$tag} || 'target';
#print "$dom->{tag}: $name [$space]\n";
    foreach $target (@{$TARGET_NAME{$space}{$name}}) {
	next if $name eq '' || defined $target->{attr}{names} &&
	    $target->{attr}{names}[0] ne $casename;
	my $ttag = $target->{tag};
	$tags{$ttag}++;
	if ($tag =~ /substitution/) {
	    $level = 3;
	}
	if (((defined $uri && ($target->{attr}{refuri} || '') ne $uri) ||
	     (! defined $uri &&
#	      ($tag !~ 'section' && $ttag ne 'section') &&
	      # Both targets are explicit 
	      ($tag =~ /^(target|footnote|citation)$/ &&
	       $ttag =~ /^(target|footnote|citation)$/) &&
	      ($CITSPACE{$tag} || '') eq ($CITSPACE{$ttag} || '')))
	    && $level < 2) {
	    $level = 2;
	}
	if ($ttag ne $tag || $tag ne 'target' || $level == 2) {
	    $target->{attr}{dupnames} = $dom->{attr}{names};
	    delete $target->{attr}{names};
	}
    }
    push (@{$TARGET_NAME{$space}{$name}}, $dom);
    push (@{$ALL_TARGET_IDS{$dom->{attr}{ids}[0]}}, $dom)
	if defined $dom->{attr}{ids};
    push (@{$ALL_TARGET_NAMES{$name}}, $dom);
    my @same_name_targets =
	grep ($_ ne $dom &&
	      (defined $_->{attr}{names} && $_->{attr}{names}[0] ||
	       defined $_->{attr}{dupnames} && $_->{attr}{dupnames}[0] || '')
	      eq $casename,
	      @{$TARGET_NAME{$space}{$name}}) if $name ne '';
    if (@same_name_targets > 0) {
	my %attr;
	if ($tag !~ /substitution/) {
	    if ($tags{$tag}) {
		$dom->{attr}{dupnames} = $dom->{attr}{names};
		delete $dom->{attr}{names};
	    }
	    $dom->{attr}{ids} = [ Id() ]
		unless $dom->{attr}{ids}[0] =~ /^id\d+$/;
	    my $id = $dom->{attr}{ids}[0];
	    $attr{backrefs} = [ $id ];
	}
	my $plicit = $tag =~ /substitution/ ? 'substitution definition' :
	    $tag =~  /target|footnote/ ? 'explicit target' :
	    'implicit target';
	$error = system_message($level, $source, $lineno,
				qq(Duplicate $plicit name: "$name".),
				"", %attr);
    }
    return $error;
}

# Takes a string and handles backslash-quoting of characters
# Arguments: string
# Returns: processed string
sub RemoveBackslashes {
    my ($str) = @_;
    $str =~ s/\\(?!u[\da-fA-F]{4}|x[\da-fA-F]{2})(.)/$1 eq ' ' || $1 eq "\n" ? '' : $1/seg;
    $str;
}

# Takes the name associated with one DOM and reassigns it to another
# Arguments: source DOM, target DOM
# Returns: None
# Uses globals: %TARGET_NAME
sub ReregisterName {
    my ($olddom, $newdom) = @_;

    foreach my $id (@{$olddom->{attr}{ids}}) {
	@{$ALL_TARGET_IDS{$id}} =
	    map $_ eq $olddom ? $newdom : $_, @{$ALL_TARGET_IDS{$id}};
    }

    if ($olddom->{attr}{names}) {
	my $tag = $olddom->{tag};
	my $space = $NAMESPACE{$tag} || 'target';
	foreach my $casename (@{$olddom->{attr}{names}}) {
	    my $name = lc $casename;
	    next unless defined $name;

	    @{$TARGET_NAME{$space}{$name}} =
		map $_ eq $olddom ? $newdom : $_, @{$TARGET_NAME{$space}{$name}};
	    @{$ALL_TARGET_NAMES{$name}} =
		map $_ eq $olddom ? $newdom : $_, @{$ALL_TARGET_NAMES{$name}};
	}
    }
    return;
}

# Parses any section breaks in the text.
# Arguments: DOM object of parent, text to parse, source, line number,
#            first paragraph in section
# Returns: processed text, unprocessed text, new parent DOM object, 
#          list of DOM objects
sub SectionBreaks {
    my ($parent, $text, $source, $lineno, $section) = @_;
    my @dom;
    my $new_parent = $parent;

    $text =~ /^$SECTION_HEADER/o;
    my @sect;
    my $line;
    my $char;
    if (defined $3) {
	$char = "\\$3";
	@sect = split /^((?!$SEC_CHARS+\n(?:\n|\Z))(?!(?:\.\.|::)\n(?:   |\n))($char$char+)\n(.*\n)?(?:(($SEC_CHARS)\5+)\n)?)/m, $text, 2;
	$line = 'over';
    }
    else {
	$char = "\\$9";
	@sect = split /^((\S.*\n)(($char)$char+)\n)/m, $text, 2;
	$line = 'under';
    }
    my $next = "$sect[-1]";

    shift @sect;

    # Now process the section header
    my ($lit, $over, $title, $under, $under_char) =
	map(defined $sect[$_] ? $sect[$_] : '',
	    $line eq 'over' ? (0..4) : (0, 5, 1..3));
    if ($under eq '' && $title =~ /^($SEC_CHARS)\1+$/) {
	$under = $title;
	chomp $under;
	$title = '';
    }
# print STDERR "[$lit][$over][$title][$under][$under_char][]\n";
    my $lit_title = $title;
    $title =~ s/^\s+//;

    # Default to saying we've processed the literal part
    my $processed = $lit;
    my $unprocessed = $next;
    my $err = 1;

    # Check for errors
    if ($parent->{tag} !~ /^(document|section)$/) {
	return ('', $text)
	    if $line eq 'under' && length($under) < $MIN_SEC_LEN;
	if ($line eq 'over' && length($over) < $MIN_SEC_LEN) {
	    push(@dom,
		 system_message(1, $source, $lineno,
				"Unexpected possible title overline or transition.\n" .
				"Treating it as ordinary text because it's so short."));
	    $processed = "";
	    $unprocessed = $text;
	}
	else {
	    # It's a bogus section header in a block quote
	    push(@dom,
		 system_message(4, $source,
				$lineno+(($line eq 'over') ? 2 : 1),
				"Unexpected section title.",
				$lit));
	}
    }
    elsif ($line eq 'over' &&
	   (length($title) == 0 ||
	    length($over) < length($title)-1) &&
	   length($over) < $MIN_SEC_LEN) {
	# We don't actually consider this to be a section header
	push(@dom,
	     system_message(1, $source, $lineno,
			    "Possible incomplete section title.\n" .
			    "Treating the overline as ordinary text because it's so short."));
	if (length($title) == 0) {
	    # It's a title that looks like a section header...
	    $line = 'under';
	    $title = $over;
	    $char = substr($under, 0, 1);
	    $err = 0;
	}
	else {
	    $processed = "";
	    $unprocessed = $text;
	}
    }
    elsif ($line eq 'under' &&
	   length($under) < length($title)-1 &&
	   length($under) < $MIN_SEC_LEN) {
	# We don't actually consider this to be a section header
	push(@dom,
	     system_message(1, $source, $lineno+1,
			    "Possible title underline, too short for the title.\n" .
			    "Treating it as ordinary text because it's so short."));
	$processed = "";
	$unprocessed = $text;
    }
    elsif ($line eq 'over' && $under eq '') {
	push(@dom,
	     system_message(4, $source, $lineno,
			    defined $section && $section ne '' ?
			    "Missing matching underline for section title overline." :
			    "Incomplete section title.",
			    "$over\n$lit_title"));
    }
    elsif ($line eq 'over' && $under ne $over) {
	push(@dom,
	     system_message(4, $source, $lineno,
			    "Title overline & underline mismatch.",
			    "$over\n$lit_title$under\n"));
    }
    elsif ($line eq 'over' && $title eq '') {
	push(@dom,
	     system_message(3, $source, $lineno,
			    "Invalid section title or transition marker.",
			    "$over\n$lit_title$under\n"));
    }
    else {
	$err = 0;
    }
    if (! $err) {
	# Make sure the section style is consistent
	my $secstyle = "$line$char";
	if (! defined $SEC_LEVEL{$secstyle} &&
	    @SEC_DOM < @SEC_STYLE) {
	    push(@dom,
		 system_message(4, $source,
				$lineno + (($line eq 'over') ? 1 : 0),
				"Title level inconsistent:",
				"$lit"));
	}
	else {
	    my $dom = new DOM('section');
	    if (! defined $SEC_LEVEL{$secstyle}) {
		push(@SEC_STYLE, $secstyle);
		$SEC_LEVEL{$secstyle} = $#SEC_STYLE;
	    }
	    else {
		splice(@SEC_DOM, $SEC_LEVEL{$secstyle});
	    }
	    if (@dom) {
		$SEC_DOM[-1]->append(@dom);
		@dom = ();
	    }
	    $SEC_DOM[-1]->append($dom);
	    push(@SEC_DOM, $dom);
	    $new_parent = $dom;
	    my $titledom = new DOM('title');
	    my @errs = Inline($titledom, $title, $source, $lineno);
	    $dom->append($titledom);
	    # Reconstruct title text less markup
	    my $ttext = '';
	    $titledom->Recurse(sub {
		my ($dom) = @_;
		$ttext .= $dom->{text} if $dom->{tag} eq '#PCDATA';
	    });
	    my $id = NormalizeId($ttext, 1);
	    my $name = NormalizeName($ttext);
	    @{$dom->{attr}}{qw(ids names)} = ([ $id ], [ $name ]);
	    my $err = RegisterName($dom, $source, $lineno+1);
	    $dom->append($err) if $err;
	    # Check for short underlines
	    if ($line eq 'under' && length($title) > length($under)+1) {
		$dom->append
		    (system_message(2, $source, $lineno+1,
				    "Title underline too short.",
				    "$lit_title$under\n"));
	    }
	    if ($line eq 'over' && length($title) > length($over)+1) {
		$dom->append
		    (system_message(2, $source, $lineno,
				    "Title overline too short.",
				    "$over\n$lit_title$under\n"));
	    }
	}
    }

    return $processed, $unprocessed, $new_parent, @dom;
}

# Returns a DOM object for an RST table object given a simple table
# text string.
# Arguments: text string, source, line number
# Returns: DOM object
sub SimpleTable {
    my($text, $source, $lineno) = @_;

    # Split the table into its constituent lines
    my $table = $text;
    chomp $table;
    $table =~ s/^\001//gm;
    my @lines = split(/ *\n/, $table);
    $lines[-1] =~ s/ +$//;

    # We can compute the column boundaries from the first line.
    # It is complicated by the fact that a column separator may be more
    # than one character.
    my @segments = split(/( +)/, $lines[0]);
    my (@colstart, @colwidth, @sep);
    my $col = 0;
    foreach (@segments) {
	my $len = length($_);
	if (substr($_, 0, 1) eq ' ') {
	    push(@sep, [$col,$len]);
	}
	else {
	    push(@colstart, $col);
	    push(@colwidth, $len);
	}

	$col += $len;
    }
    # Now look for a header row
    my $head = 1;  # The line on which the heading ends
    my $l;
    my $last_equal_line = 0;
    for ($l=1; $l < @lines; $l++) {
	$_ = $lines[$l];
	if (/^=+( +=+)* *$/) {
	    return system_message(3, $source, $lineno,
				  "Malformed table.\nBottom/header table border does not match top border.",
				  $table)
		if length($lines[$l]) != length($lines[0]);
	    $head = $last_equal_line+1;
	    $last_equal_line = $l;
	}
    }
    
    my $dom = new DOM('table');
    $dom->{tableattr} = $main::opt_D{tableattr};
    my $tgroup = new DOM('tgroup', cols=>@colwidth+0);
    $dom->append($tgroup);
    my $colspec;
    foreach (@colwidth) {
	$colspec = new DOM('colspec', colwidth=>$_);
	$tgroup->append($colspec);
    }
    my $tbody;
    if ($head > 1) {
	$tbody = new DOM('thead');
	$tgroup->append($tbody);
    }

    # Process all the rows of the table
    my $row_start = 0;
    for ($l=1; $l < @lines; $l++) {
	$_ = $lines[$l];
	next if $l == $row_start && !/^([=-])(?:\1| )+\1 *$/;
	my $col1 = substr($_, 0, $colwidth[0]);
	if ($col1 =~ /\S/ || ($lines[$l-1] =~ /^([=-])(?:\1| )+\1 *$/ &&
			      ! /^\s*$/)) {
	    # We've hit the beginning of the next row; process the previous one
	    my $next_row_start;
	    my ($row_colstart, $row_colwidth, $row_sep);
	    if (/^([=-])(?:\1| )+\1 *$/) {
		return system_message(3, $source, $lineno,
				      "Malformed table.\nColumn span incomplete at line offset $l.",
				      $table)
		    if length($_) < length($lines[0]);
		# It's a separator/column span line
		for ($next_row_start=$l+1; $next_row_start<@lines;
		     $next_row_start++) {
		    my $next_col1 = 
			substr($lines[$next_row_start],0,$colwidth[0]);
		    last if $next_col1 =~ /\S/;
		}
		@segments = split(/( +)/);
		my $col = 0;
		foreach (@segments) {
		    my $len = length($_);
		    if (substr($_, 0, 1) eq ' ') {
			push(@$row_sep, [$col, $len]);
		    }
		    else {
			push(@$row_colstart, $col);
			push(@$row_colwidth, $len);
		    }
		    $col += $len;
		}
		# Do sanity check on the separator starts
		my $s;
		foreach $s (@$row_sep) {
		    return system_message(3, $source, $lineno,
					  "Malformed table.\nColumn span alignment problem at line offset @{[$row_start+1]}.",
					  $table)
			unless grep($_->[0] == $s->[0] && $_->[1] == $s->[1],
				    @sep);
		}
	    }
	    else {
		$next_row_start = $l;
		($row_colstart, $row_colwidth, $row_sep) =
		    (\@colstart, \@colwidth, \@sep);
	    }
	    # Process the row
	    if ($row_start > 0 && $row_start <= $l) {
		if ($row_start == $head) {
		    $tbody = new DOM('tbody');
		    $tgroup->append($tbody);
		}
		my $row = new DOM('row');
		$tbody->append($row);

		my $row_end = $l-1;
		my $col;
		# Make sure we don't have text in the column separators
		for ($col=0; $col<@$row_sep; $col++) {
		    my ($start,$width) = @{$row_sep->[$col]};
		    my $septext =
			join('',map(do {local $^W = 0;
					substr($lines[$_],$start,$width)
					    . "\n"},
				    $row_start .. $row_end));
		    return system_message(3, $source, $lineno,
					  "Malformed table.\nText in column margin at line offset $row_start.",
					  $table)
			if $septext =~ /\S/;
		}
		# Produce the entries
		for ($col=0; $col<@$row_colstart; $col++) {
		    my $entry = new DOM('entry');
		    $row->append($entry);

		    my $start = $row_colstart->[$col];
		    my $width = $col < $#$row_colstart ? $row_colwidth->[$col]
			: 0xffff;
		    my $end = $start+$width;
		    # Compute the column spans
		    my @colspans = grep($_ > $start && $_ < $end, @colstart);
		    $entry->{attr}{morecols} = @colspans if @colspans > 0;
		    my $celltext = 
			join('',map(do {local $^W = 0;
					substr($lines[$_],$start,$width)
					    . "\n"}, $row_start .. $row_end));
		    # Handle right/center alignment for single-row entries
		    if (($celltext =~ /\A.*\n[ \n]*\Z/) &&
			$main::opt_D{align}) {
			$celltext =~ /(.*)/;
			my $ct = $1;
			$entry->{attr}{align} = $ct =~ /^\S/ ? 'left' :
			    $ct =~ / $/ ||
			    length($ct) < $row_colwidth->[$col] ? 'center' :
			    'right';
		    }
		    # May need to update the colspec for text that overflows
		    # the last column
		    if ($col == $#$row_colstart) {
			my $colwidth = 0;
			grep(do{
			    my $cell_line = do {
				local $^W=0;
				substr($lines[$_],$start,$width) . ''};
			    my $len = length($cell_line);
			    $colwidth = $len if $len > $colwidth;
			}, $row_start .. $row_end);
			$colwidth +=
			    $row_colstart->[$col] - $colstart[-1];
			$colspec->{attr}{colwidth} = $colwidth
			    if $colwidth > $colspec->{attr}{colwidth};
		    }
		    $celltext =~ s/ *$//gm;
		    # Delete common indent
		    $celltext =~ /^( *)/;
		    my $spaces = $1;
		    $celltext =~ s/^$spaces//gm;
		    Paragraphs($entry, $celltext, $source,
			       $lineno+$row_start);
		    $entry->{entryattr} = $main::opt_D{entryattr};
		}
		$row->{rowattr} = $main::opt_D{rowattr};
	    }
	    $row_start = $next_row_start;
	}
    }

    if ($table !~ /\n=+( +=+)* *$/) {
	if ($table =~ /((?:.)+\n=+( +=+)*\n)(.*)/s) {
	    my ($table,$rest) = ($1, $3);
	    $dom = 
		system_message(3, $source, $lineno,
			       "Malformed table.\nNo bottom table border found or no blank line after table bottom.",
			       $table)
		if $head == 1;
	    $lineno += ($table =~ tr/\n//);
	    my $err =
		system_message(2, $source, $lineno,
			       "Blank line required after table.");
	    my $fake = new DOM('fake');
	    Paragraphs($fake, $rest, $source, $lineno);
	    return ($dom, $err, $fake->contents());
	}
	else {
	    return system_message(3, $source, $lineno,
				  "Malformed table.\nNo bottom table border found.",
				  "$table\n");
	}
    }

    return $dom;
}

# Returns a DOM object for an RST table object.
# Arguments: text string, source, line number
# Returns: DOM object
sub Table {
    my($text, $source, $lineno) = @_;

    return SimpleTable($text, $source, $lineno)
	if $text =~ /^=[ =]+= *\n/;
    # Split the table into its constituent lines
    my $table = $text;
    chomp $table;
    my @lines = split(/ *\n/, $table);
    my $head = 0;  # The line on which the heading ends

    # Create a graph to figure out how the cells are connected
    use Graph;
    my $g = new Graph;
    # Look for plus signs that are connected to other plus signs
    my $v;
    for ($v=0; $v < @lines; $v++) {
	my @segments = split(/[+]/, $lines[$v]);
	push(@segments, "") if substr($lines[$v],-1,1) eq '+';
	my $s;
	# Note: we start at $s=1 because that's where the first plus sign was
	my $h = length($segments[0]);
	for ($s=1; $s < @segments; $s++) {
	    my $seg = $segments[$s];

	    # Check for a horizontal edge
	    $g->AddEdge([$v,$h],[$v,$h+length($seg)+1])
		if ($seg =~ /^([-=])\1* *$/);
	    $head = $v if $1 eq '=';
	    # Check for a vertical edge
	    if ($v < @lines-2 &&
		do { local $^W=0; substr($lines[$v+1],$h,1) eq '|'}) {
		my $v1;
		for ($v1=$v+2; $v1<@lines; $v1++) {
		    last unless substr($lines[$v1],$h,1) eq '|';
		}
		$g->AddEdge([$v,$h],[$v1,$h])
		    if substr($lines[$v1],$h,1) eq '+';
	    }

	    $h += length($seg) + 1;
	}
    }
    # Now we mark everything that is reachable from 0,0
    $g->DFS([0,0], sub {my ($g,$p) = @_; $g->SetVertexProp($p,'mark',1)});
    my @verts = grep($g->GetVertexProp($_,'mark'),$g->GetVertices());
    my $vmax = $#lines;
    my $hmax = length($lines[0])-1;
    my (%rows,%cols);
    foreach (@verts) {
	$rows{$_->[0]} = 1;
	$cols{$_->[1]} = 1;
    }
    my @rows = sort {$a <=> $b} keys %rows;
    my @cols = sort {$a <=> $b} keys %cols;
    
    # Check that all vertices except the corners have degree >= 3 and that
    # corners have degree 2
    foreach (@verts) {
	my @edges = $g->GetVertexEdges($_);
	my $iscorner = ($_->[0] == 0 || $_->[0] == $vmax) &&
	    ($_->[1] == 0 || $_->[1] == $hmax);
	return system_message(3, $source, $lineno, "Malformed table.", $text)
	    if $iscorner && @edges != 2;
	return system_message(3, $source, $lineno, "Malformed table.\nMalformed table; parse incomplete.",
			      $text)
	    if ! $iscorner && @edges < 3;
    }

    my $dom = new DOM('table');
    $dom->{tableattr} = $main::opt_D{tableattr};
    my $tgroup = new DOM('tgroup', cols=>$#cols);
    $dom->append($tgroup);
    my $c;
    for ($c=0; $c < $#cols; $c++) {
	$tgroup->append(new DOM('colspec',colwidth=>$cols[$c+1]-$cols[$c]-1));
    }
    my $tbody;
    if ($head > 0) {
	$tbody = new DOM('thead');
	$tgroup->append($tbody);
    }

    # Now we go through all the upper-left corners of cells adding them
    # to the table body.
    my $lastv = -1;
    my $row;
#print join(',',map("[$_->[0],$_->[1]]",@verts)),"\n";
#print join(',',@rows),"\n";
    foreach (@verts) {
	my ($v, $h) = @$_;
	next if $v == $vmax || $h == $hmax;
	if ($v > $lastv) {
	    if ($v == $head) {
		$tbody = new DOM('tbody');
		$tgroup->append($tbody);
	    }
	    $row->{rowattr} = $main::opt_D{rowattr} if defined $row;
	    $row = new DOM('row');
	    $tbody->append($row);
	}

	# This is only the top-left if it has edges right and down
	my ($down,$right,$p2);
	my @edges = $g->GetVertexEdges([$v,$h]);
	foreach $p2 (@edges) {
	    $right = $p2->[1] if $p2->[0] == $v && $p2->[1] > $h;
	    $down = $p2->[0] if $p2->[1] == $h && $p2->[0] > $v;
	}
	next unless defined $right && defined $down;

	my $entry = new DOM('entry');
	$row->append($entry);
	# Check for row and column spans
	# Track right to an edge that goes down and down to an edge
	# that goes right
	my $p1 = $right;
	while (defined $p1) {
	    my ($r, $d);
	    my @edges = $g->GetVertexEdges([$v,$p1]);
	    foreach $p2 (@edges) {
		$r = $p2->[1] if $p2->[0] == $v && $p2->[1] > $p1;
		$d = $p2->[0] if $p2->[1] == $p1 && $p2->[0] > $v;
	    }
	    if (defined $d) {
		$right = $p1;
		last;
	    }
	    $p1 = $r;
	}
	@edges = $g->GetVertexEdges([$v,$h]);
	$p1 = $down;
	while (defined $p1) {
	    my ($r, $d);
	    my @edges = $g->GetVertexEdges([$p1,$h]);
	    foreach $p2 (@edges) {
		$r = $p2->[1] if $p2->[0] == $p1 && $p2->[1] > $h;
		$d = $p2->[0] if $p2->[1] == $h && $p2->[0] > $p1;
	    }
	    if (defined $r) {
		$down = $p1;
		last;
	    }
	    $p1 = $d;
	}
#print "[$v,$h] [$down,$right]\n";
	my @cspans = grep($_ > $v && $_ <= $down, @rows);
	my @rspans = grep($_ > $h && $_ <= $right, @cols);
	$entry->{attr}{morecols} = @rspans-1 if @rspans > 1;
	$entry->{attr}{morerows} = @cspans-1 if @cspans > 1;
	my $chars = $right - $h - 1;
	my $celltext = join('', map(substr($lines[$_], $h+1, $chars) . "\n",
				    (($v+1) .. ($down-1))));
	# Delete trailing spaces
	$celltext =~ s/ *$//gm;
	# Delete common indent
	$celltext =~ /^( *)/;
	my $spaces = $1;
	$celltext =~ s/^$spaces//gm;
	Paragraphs($entry, $celltext, $source, $lineno+$v+1);
	$entry->{entryattr} = $main::opt_D{entryattr};
	$lastv = $v;
    }
    $row->{rowattr} = $main::opt_D{rowattr} if defined $row;
    return $dom;
}

# Returns the error messages for an unknown role name
# Arguments: role name, source, line number, optional list of attributes for
#            last system_message
# Returns: array of DOM objects
sub UnknownRole {
    my ($role, $source, $lineno, $lit, %attrs) = @_;

    return
	system_message(1, $source, $lineno,
		       qq(No role entry for "$role" in module "docutils.parsers.rst.languages.en".\nTrying "$role" as canonical role name.),
		       ""),
	system_message(3, $source, $lineno,
		       qq(Unknown interpreted text role "$role".),
		       $lit, %attrs);
}

package RST::Role;

# Checks for validity of a PEP reference
sub PEP {
    my ($pep, $lit, $parent, $source, $lineno) = @_;

    my @errs;
    if ($pep !~ /^\d+$/ || $pep < 0 || $pep > 9999) {
	my ($dom,$refid,$id) = RST::problematic($lit);
	$parent->append($dom);
	push @errs, RST::system_message(3, $source, $lineno,
					qq(PEP number must be a number from 0 to 9999; "$pep" is invalid.),
					"", backrefs=>[ $id ],
					ids=>[ $refid ]);
    }
    return @errs;
}

# Checks for validity of an RFC reference
sub RFC {
    my ($rfc, $lit, $parent, $source, $lineno) = @_;

    my @errs;
    if ($rfc !~ /^\d+$/ || $rfc < 1) {
	my ($dom,$refid,$id) = RST::problematic($lit);
	$parent->append($dom);
	push @errs, RST::system_message(3, $source, $lineno,
					qq(RFC number must be a number greater than or equal to 1; "$rfc" is invalid.),
					"", backrefs=>[ $id ],
					ids=>[ $refid ]);
    }
    return @errs;
}

# Checks for validity of an RAW reference
sub raw {
    my ($raw, $lit, $parent, $source, $lineno, $role) = @_;

    my @errs;
    if (! defined $RST::MY_ROLES{$role}{attr}{format}) {
	my ($dom,$refid,$id) = RST::problematic($lit);
	$parent->append($dom);
	push @errs, RST::system_message(3, $source, $lineno,
					qq(No format (Writer name) is associated with this role: "raw".\nThe "raw" role cannot be used directly.\nInstead, use the "role" directive to create a new role with an associated format.),
					"", backrefs=>[ $id ],
					ids=>[ $refid ]);
    }
    return @errs;
}

package RST::Directive;

# This package contains the code for the various RST built-in directives

# Data structures:
#   _`Directive arguments hash ref`: Returned by 
#     RST::Directive::parse_directive.  It has keys:
#       ``name``:           the directive name
#       ``args``:           parsed arguments from dtext
#       ``options``:        reference to hash of parsed option/value pairs
#       ``content``:        parsed content from dtext
#       ``content_lineno``: line number of the content block
#   _`Role definition hash reference`: Used for a role definition.  It
#     has keys (all are optional except tag):
#       ``tag``:            the tag used for a DOM object using the
#                           role (required)
#       ``attr``:           reference to hash whose keys are attribute
#                           names and whose values are sprintf strings
#                           that are called with interpreted text
#       ``text``:           sprintf string called with the interpreted
#                           text to produce the final text string
#       ``alias``:          name of another role for which this is a
#                           synonym
#       ``check``:          reference to subroutine to call to check
#                           validity of the interpreted text. The
#                           arguments to the routine are (interpreted
#                           text, literal of interpreted/role
#                           combination, DOM object for future parent,
#                           source file name, line number).

# Built-in handler for admonition directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub admonition {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
#use PrintVar;PrintVar::PrintVar(\@_);print "\n";
    my $subst = $parent->{attr}{name}
    if $parent->{tag} eq 'substitution_definition';

    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my $content = $dhash->{content} ne '' ? $dhash->{content} :
	$dhash->{args};
    return RST::system_message(3, $source, $lineno,
			       qq(The "$name" admonition is empty; content required.),
			       $lit)
	if $content =~ /^$/;
    my $adm = new DOM(lc $name);
    if ($name eq 'admonition') {
	# A generic admonition
	my $ttext = $dhash->{args};
	return no_args($name, $source, $lineno, $lit)
	    if $ttext =~ /^$/;
	$adm->{attr}{classes} = [ $dhash->{options}{class} ||
				  RST::NormalizeId("$name-$ttext") ];
	my $title = new DOM ('title');
	$title->append(newPCDATA DOM ($ttext));
	$adm->append($title);
    }
    RST::Paragraphs($adm, $content, $source, $dhash->{content_lineno});

    return $adm;
}

# Built-in handler for class directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub class {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';

    return RST::system_message(3, $source, $lineno,
			       qq(The "$name" directive may not have contents.),
			       $lit)
	if $dhash->{content} ne '';

    my($args, $options) = map($dhash->{$_}, qw(args options));
    return no_args($name, $source, $lineno, $lit) if $args eq '';
    return RST::system_message(3, $source, $lineno,
			       qq(Invalid class attribute value for "$name" directive: "$args".)
			       , $lit)
	unless $args =~ /^[a-z][-a-z0-9]*(?:\s+[a-z][-a-z0-9]*)*$/i;

    my $pending = new DOM('pending');
    $pending->{internal}{'.transform'} = "docutils.transforms.parts.Class";
    my $details = $pending->{internal}{'.details'} = { };
    $details->{class} = $args;
    @{$pending}{qw(source lineno lit)} = ($source, $lineno, $lit);
    my @optlist = ();
    my $err = check_option_names($name, $options, \@optlist, $source, $lineno,
				 $lit);
    return $err || $pending;
}

# Built-in handler for compound directive.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub compound {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
#use PrintVar;PrintVar::PrintVar(\@_);print "\n";
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($content, $args, $options) =
	map($dhash->{$_}, qw(content args options));
    return system_message($name, 3, $source, $lineno,
			  'no arguments permitted; blank line required before content block.',
			  $lit)
	if $content ne '' && $args ne '';

    $content = $args if $content eq '';
    return RST::system_message(3, $source, $lineno,
			       qq(The "$name" compound is empty; content required.),
			       $lit)
	if $content =~ /^$/;
    my @optlist = qw(class);
    my $err = check_option_names($name, $options, \@optlist, $source, $lineno,
 				 $lit);
    return $err if $err;

    my $comp = new DOM($name);
    RST::Paragraphs($comp, $content, $source, $dhash->{content_lineno});
    $comp->{attr}{classes} = [ $options->{class} ]
	if defined $options->{class};

    return $comp;
}

# Built-in handler for contents directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub contents {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';

    return RST::system_message(3, $source, $lineno,
			       qq(The "contents" directive may not be used within topics or body elements.),
			       $lit)
	if $parent->{tag} !~ /^(section|document|sidebar)$/;

    my($args, $options) =
	map($dhash->{$_}, qw(args options));

    # Create the topic
    my $topic = new DOM('topic', classes=>[ 'contents' ]);
    my $ttext = $args ne '' ? $args : 'Contents';
    push @{$topic->{attr}{classes}}, 'local' if defined $options->{local};
    $topic->{attr}{ids} = [ RST::NormalizeId($ttext) ];
    $topic->{attr}{names} = [ RST::NormalizeName($ttext) ];
    RST::RegisterName($topic, $source, $lineno);
    if ($args ne '' || ! defined $options->{local}) {
	my $title = new DOM('title');
	my $fake = new DOM('fake');
	RST::Paragraphs($fake, $ttext, $source, $lineno);
	my $last = $fake->last();
	if ($fake->num_contents() == 1 && $last->{tag} eq 'paragraph') {
	    $title->append($last->contents());
	}
	$topic->append($title);
    }
    
    my $pending = new DOM('pending');
    $topic->append($pending);
    $pending->{internal}{'.transform'} =
	"docutils.transforms.parts.Contents";
    $pending->{source} = $source;
    $pending->{lineno} = $lineno;
    my @optlist = qw(depth local backlinks);
    my $err = check_option_names($name, $options, \@optlist, $source, $lineno,
				 $lit);
    return $err if $err;

    # Save away the enclosing section for local toc
    $pending->{section} = $RST::SEC_DOM[-1] if defined $options->{local};
    my $opt;
    foreach $opt (sort keys %$options) {
	my $str = $options->{$opt};
	if ($opt eq 'local') {
	    return bad_option($name, $opt, $str, 3, $source, $lineno,
			      qq(no argument is allowed; "$str" supplied.),
			      $lit)
	    if $str ne '';
	}
	elsif ($opt eq 'depth') {
	    my $err = check_int_option($name, $opt, $str, $str, $source,
				       $lineno, $lit);
	    return $err if $err;
	}
	elsif ($opt eq 'backlinks') {
	    my @vallist = qw(top entry none);
	    my $err = check_enum_option($name, $opt, $str, \@vallist,
					$source, $lineno, $lit);
	    return $err if $err;
	    substr($str,0,1) =~ tr/a-z/A-Z/;
	}
	$pending->{internal}{'.details'}{$opt} = $str;
    }
    return $topic;
}

# Built-in handler for decoration directive.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub decoration {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
#use PrintVar;PrintVar::PrintVar(\@_);print "\n";
#     return system_message($name, 3, $source, $lineno,
# 			  "directive must be used before any body blocks.",
# 			  $lit)
# 	if $parent->{tag} ne 'document';
    my $topdom = $RST::TOPDOM;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    return system_message($name, 3, $source, $lineno,
			  'no arguments permitted; blank line required before content block.',
			  $lit)
	if $dhash->{content} ne '' && $dhash->{args} ne '';

    # See if there's already the right kind of block under <decoration>
    my $dec = $topdom->{content}[0];
    if (! defined $dec || $dec->{tag} ne 'decoration') {
	$dec = new DOM('decoration');
	$topdom->prepend($dec);
    }
    my ($block) = grep $_->{tag} eq $name, $dec->contents();
    if (! defined $block) {
	$block = new DOM($name);
	if ($name eq 'header') {
	    $dec->prepend($block);
	}
	else {
	    $dec->append($block);
	}
    }
    my $content = $dhash->{content} ne '' ? $dhash->{content} :
	$dhash->{args};
    if ($content =~ /^$/) {
	RST::Paragraphs($block, qq(Problem with the "$name" directive: no content supplied.),
			$source, $dhash->{content_lineno});
	return RST::system_message(2, $source, $lineno,
				   qq(Content block expected for the "$name" directive; none found.),
				   $lit);
    }

    RST::Paragraphs($block, $content, $source, $dhash->{content_lineno});

    return;
}

# Built-in handler for default-role directive.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub default_role {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
#use PrintVar;PrintVar::PrintVar(\@_);print "\n";
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    return system_message($name, 3, $source, $lineno,
			  'no content block permitted.',
			  $lit)
	if $dhash->{content} ne '';

    my $role = $dhash->{args};
    return RST::UnknownRole($role, $source, $lineno, $lit)
	unless $role eq '' || defined $RST::MY_ROLES{$role};
    $RST::MY_DEFAULT_ROLE = $role ? $role : $RST::DEFAULT_ROLE;

    return;
}

# Built-in handler for figure directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub figure {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($content, $content_lineno, $options) =
	map($dhash->{$_}, qw(content content_lineno options));

    if (defined $options->{align}) {
	my @vallist = qw(left center right);
	my $err = check_enum_option($name, 'align', $options->{align},
				    \@vallist, $source, $lineno, $lit);
	return $err if $err;
    }

    my $cline = $content_lineno - $lineno;
    my @dline = split(/\n/, $dtext);
    my $dline = join("\n", @dline[0 .. $cline-1]);
    my %myopts = (figwidth=>'width', figclass=>'@classes', align=>'align');
    my $image = image("image", $parent, $source, $lineno, $dline, $lit,
		      keys %myopts);
    return $image if $image->{tag} eq 'system_message';

    my @dom;
    my $figure = new DOM(lc $name);
    $figure->append($image);
    push(@dom, $figure);
    foreach (keys %myopts) {
	if (defined $options->{$_}) {
	    if ($myopts{$_} =~ /^@(.*)/) {
		$figure->{attr}{$1} = [ $options->{$_} ];
	    }
	    else {
		$figure->{attr}{$myopts{$_}} = $options->{$_};
	    }
	}
    }

    my $caption = '';
    my $legend = '';
    my $legend_lineno;
    if ((my @s = split /^(\n+)/m, $content, 2) > 1) {
	$caption = $s[0];
	$legend = $s[-1];
	if ($legend ne "") {
	    my $pre = "$s[0]$s[1]";
	    $legend_lineno = $content_lineno + ($pre =~ s/(\n)/\n/g);
	}
    }
    else { $caption = $content }
    if ($caption !~ /^(..)?$/) {
	my $capdom = new DOM('caption');
	my $fake = new DOM('fake');
	RST::Paragraphs($fake, $caption, $source, $content_lineno);
	my $last = $fake->last();
	if ($fake->num_contents() == 1 && $last->{tag} eq 'paragraph') {
	    $capdom->append($last->contents());
	    $figure->append($capdom);
	}
	else {
	    # This wasn't a simple paragraph
	    push(@dom, RST::system_message(3, $source, $lineno,
					   "Figure caption must be a paragraph or empty comment.",
					   $lit));
	}
    }
    if ($legend ne '') {
	my $legdom = new DOM('legend');
	$figure->append($legdom);
	RST::Paragraphs($legdom, $legend, $source, $legend_lineno);
    }
    return @dom;
}

# Built-in handler for image directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text, list of extra options allowed
# Returns: array of DOM objects
sub image {
    my($name, $parent, $source, $lineno, $dtext, $lit, @extra_opts) = @_;
#use PrintVar;PrintVar::PrintVar(\@_);print "\n";
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $content, $options) =
	map($dhash->{$_}, qw(args content options));

    return system_message($name, 3, $source, $lineno,
			  "no content permitted.", $dtext)
	if ($content ne '' && $name eq 'image');
    my ($err) = arg_check($name, $source, $lineno, $args, $lit, 1);
    return $err if $err;
    $args =~ s/[ \n]//g;

    # Process the options
    my @optlist = (qw(width height scale alt usemap target align class),
		   @extra_opts);
    $err = check_option_names($name, $options, \@optlist, $source, $lineno,
			      $lit);
    return $err if $err;
    if (defined $options->{scale}) {
	my $scale = $options->{scale};
 	my $err = check_int_option($name, 'scale', $scale, $scale, $source,
				   $lineno, $lit, 0);
 	return $err if $err;
    }
    if (defined (my $align = $options->{align})) {
	my $in_subst = $parent->{tag} eq 'substitution_definition';
	my @svals = qw(top middle bottom);
	my @ovals = qw(left center right);
	my @vals = $in_subst ? @svals : @ovals;
	my $err = check_enum_option($name, 'align', $align, [@svals, @ovals],
				    $source, $lineno, $lit);
	return $err if $err;
	if (! grep($_ eq $align, @vals)) {
	    my $subst = $in_subst ? ' within a substitution definition' : '';
	    my $vals = join(', ', map(qq("$_"), @vals));
	    return RST::system_message(3, $source, $lineno,
				       qq(Error in "$name" directive: "$align" is not a valid value for the "align" option$subst.  Valid values for "align" are: $vals.),
				       $lit);
	}
    }
    foreach my $opt (qw(height width)) {
	my $err = check_units_option($name, $opt, $options->{$opt},
				     $source, $lineno, $lit,
				     $opt eq 'width' ? ('%') : ())
	    if defined $options->{$opt};
	return $err if $err;
    }
    foreach my $opt (qw(target)) {
	my $err = check_required_option($name, $opt, $options->{$opt},
					$source, $lineno, $lit)
	    if defined $options->{$opt};
	return $err if $err;
    }
    if (defined $options->{class}) {
	$options->{classes} = [ $options->{class} ];
	delete $options->{class};
    }

    my %attr;
    my $alt = '';
    $alt = $parent->{attr}{names}[0]
    if $parent->{tag} eq 'substitution_definition';
    $attr{alt} = $alt if $alt ne '';
    delete $options->{$_} foreach (@extra_opts);

    my $dom = new DOM (lc $name, uri=>$args, %attr, %$options);
    if (my $target = $options->{target}) {
	delete $dom->{attr}{target};
	my $newdom = new DOM ('reference');
	if ($target =~ /\`(.*)\`_$/s || $target =~ /^(\S+)_$/) {
	    # Indirect target
	    (my $refname = $1) =~ s/\n/ /g;
	    $target =~ s/\n/ /g;
	    $newdom->{attr}{name} = RST::NormalizeName($target);
	    $newdom->{attr}{refname} = $refname;
	}
	else {
	    $target =~ s/\n//g;
	    $newdom->{attr}{refuri} = $target;
	}
	$newdom->append($dom);
	$dom = $newdom;
    }
    return $dom;
}

# Built-in handler for include directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub include {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $options) = map($dhash->{$_}, qw(args options));

    my @exts = split(/:/, $main::opt_D{includeext});
    my $mydir = $source =~ m|(.*)/| ? $1 : ".";
    my $path = $main::opt_D{includepath};
    $path =~ s/<\.>/$mydir/;
    my @dirs = map(m|^\./?$| ? "" : m|/$| ? $_ : "$_/",split(/:/, $path));
    $args =~ s/^<(.*)>$/$1/;
    my $file = $args;
    my $dir;
    foreach $dir (@dirs) {
	my @files = map("$dir$args$_", @exts);
	my @foundfiles = grep(-r $_, @files);
	if (@foundfiles) {
	    $file = $foundfiles[0];
	    last;
	}
    }
    my $text;
    print STDERR "Debug: $source, $lineno: Including $file\n" if $main::opt_d;
    if (open(FILE,$file)) {
	$text = join('',<FILE>);
# TODO:
#  	use Encode qw/encode decode/;
#  	$text = decode($options->{encoding}, $text)
#  	    if defined $options->{encoding};
	if (defined $options->{literal}) {
	    my $lb = new DOM('literal_block', %RST::XML_SPACE, source=>$file);
	    $lb->append(newPCDATA DOM($text));
	    return $lb;
	}
	else {
	    RST::Paragraphs($parent, $text, $file, 1) if defined $text;
	}
    }
    else {
	my $err = "IOError: " . system_error();
	return RST::system_message(4, $source, $lineno,
				   qq(Problems with "$name" directive path:\n$err: '$args'.),
				   $lit);
    }
    return;
}

# Built-in handler for line-block directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub line_block {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $content, $content_lineno, $options) =
	map($dhash->{$_}, qw(args content content_lineno options));
    my ($err) = arg_check($name, $source, $lineno, $args, $lit, 0);
    return $err if $err;
    my @optlist = qw(class);
    $err = check_option_names($name, $options, \@optlist, $source, $lineno,
			      $lit);
    return $err if $err;
    return RST::system_message(2, $source, $lineno,
			       qq(Content block expected for the "$name" directive; none found.),
				  $lit) unless $content;
    $content =~ s/^/| /gm;
    my ($proc, @doms) = RST::LineBlock($content, $source, $content_lineno);
    if ($options->{class}) {
	my ($lb) = grep($_->{tag} eq 'line_block', @doms);
	$lb->{attr}{classes} = [ $options->{class} ] if $lb;
    }
    return grep(ref $_ eq 'DOM', @doms);
}

# Built-in handler for meta directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub meta {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my @doms;
    my $para = $dtext;
    $para =~ s/^.*\n//;
    return RST::system_message(3, $source, $lineno,
			       "Empty meta directive.", $lit)
	unless $para ne '';
    $para =~ /^( *)/;
    my $spaces = $1;
    $para =~ s/^$spaces//gm;
    my $lines = 1;
    while ($para =~ /^:([^:\n]+): *(.*)/s) {
	my $optlit = $para;
	my ($next, $field) = ('');
	($field, $para) = ($1, $2);
	# See if there are any subsequent field list items
	if ((my @s = split /^(?!\A)($RST::FIELD_LIST|\S)/om, $para, 2) > 1) {
	    $para = $s[0];
	    $next = "$s[1]$s[-1]";
	}
	# Remove initial spaces
	my @spaces = $para =~ /^(?!\A)( +)/mg;
	my $spaces = defined $spaces[0] ? $spaces[0] : '';
	foreach (@spaces) {
	    $spaces = $_ if length($_) < length($spaces);
	}
	$para =~ s/^$spaces//mg;

	return RST::system_message(1, $source, $lineno+$lines,
				   qq(No content for meta tag "$field".),
				   $optlit)
	    if $para =~ /^$/;
	my $pending = new DOM('pending');
	push(@doms, $pending);
	$pending->{internal}{'.transform'} =
	    "docutils.transforms.components.Filter";
	$pending->{source} = $source;
	$pending->{lineno} = $lineno;
	$pending->{internal}{'.details'}{component} = "'writer'";
	$pending->{internal}{'.details'}{format} = "'html'";
	my $opt = $field;
	$opt =~ s/^([\w\.-]+)(?:=([\w\.-]+))?\s*//;
	my ($name, $nametag);
	$nametag = 'name' unless defined $nametag;
	if (defined $2) {
	    ($nametag, $name) = ($1, $2);
	}
	else {
	    ($nametag, $name) = ('name', $1);
	}
	my @attr = split(/\s*;\s*/, $opt);
	my %attr;
	foreach (@attr) {
	    if (/(.*)=(.*)/) {
		$attr{$1} = $2;
	    }
	    else {
		return
		    RST::system_message(3, $source, $lineno+$lines,
					qq(Error parsing meta tag attribute "$_": missing "=".),
					$optlit);
	    }
	}
	my $content = $para;
	chomp $content;
	$content =~ s/\n/ /g;
	my $dom = new DOM('meta', content=>$content,
			  $nametag=>$name, %attr);
	$pending->{internal}{'.details'}{nodes} = $dom;

	$lines += $para =~ tr/\n//;
	$para = $next;
    }

    push (@doms,
	  RST::system_message(3, $source, $lineno,
			      "Invalid meta directive.", $lit))
	if $para ne '';

    return @doms;
}

# Built-in handler for parsed-literal directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub parsed_literal {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $content, $content_lineno, $options) =
	map($dhash->{$_}, qw(args content content_lineno options));
    my ($err) = arg_check($name, $source, $lineno, $args, $lit, 0);
    return $err if $err;
    my @optlist = qw();
    $err = check_option_names($name, $options, \@optlist, $source, $lineno,
				 $lit);
    return $err if $err;
    my $lb = new DOM('parsed_literal', %RST::XML_SPACE);
    my @errs = RST::Inline($lb, $content, $source, $content_lineno);
    return $lb, @errs;
}

# Built-in handler for raw directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub raw {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
#use PrintVar;PrintVar::PrintVar(\@_);print "\n";
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $content, $options) =
	map($dhash->{$_}, qw(args content options));

    my @optlist = qw(file url);
    my $err = check_option_names($name, $options, \@optlist, $source, $lineno,
				 $lit);
    return $err if $err;

    return RST::system_message(3, $source, $lineno,
			       qq("$name" directive may not both specify an external file and have content.),
			       $lit)
	if defined $options->{file} && $content ne '';
    return RST::system_message(3, $source, $lineno,
			       qq(The "file" and "url" options may not be simultaneously specified for the "$name" directive.),
			       $lit)
	if defined $options->{file} && defined $options->{url};
    return RST::system_message(4, $source, $lineno,
			       qq(The "url" option is not yet implemented for the "$name" directive.),
			       $lit)
	if defined $options->{url};

    my %attr;
    if (defined $options->{file}) {
	$source =~ m|(.*/)|;
	my $opt = $options->{file};
	my $dir = $1;
	my @files = ("$dir$opt", "$dir$opt.rst", "$dir$opt.txt");
	my @foundfiles = grep(-r $_, @files);
	my $file = @foundfiles ? $foundfiles[0] : $args;
	my $text;
	if (open(FILE,$file)) {
	    $content = join('',<FILE>);
	    $attr{source} = $file;
	}
	else {
	    my $err = "IOError: " . system_error();
	    return RST::system_message(4, $source, $lineno,
				       qq(Problems with "$name" directive path:\n$err: '$args'.),
				       $lit);
	}
    }

    my $dom = new DOM('raw', format=>$args, %RST::XML_SPACE, %attr);
    chomp $content;
    $dom->append(newPCDATA DOM($content));

    return $dom;
}

# Built-in handler for replace directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub replace {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $content) = map($dhash->{$_}, qw(args content));

    my $fake = new DOM('fake');
    my $text = $args;
    $text .= "\n$content" if defined $content;
    return RST::system_message(3, $source, $lineno,
			       qq(The "$name" directive is empty; content required.))
	if $text =~ /^$/;
    return RST::system_message(3, $source, $lineno,
			       qq(Invalid context: the "$name" directive can only be used within a substitution definition.),
			       $lit)
	unless $parent->{tag} eq 'substitution_definition';
    RST::Paragraphs($fake, $text, $source, $lineno);
    my $last = $fake->last();
    if ($fake->num_contents() == 1 && $last->{tag} =~ 'paragraph') {
	my $content = $fake->{content}[0];
	my $contents = $content->{content};
	if (@$contents && $contents->[-1]{tag} eq '#PCDATA') {
	    chomp $contents->[-1]{text};
	}
	return $last->contents();
    }
    else {
	# This wasn't a simple paragraph
	return 
	    grep($_->{tag} eq 'system_message' && do {
		delete $_->{attr}{backrefs}; 1}, $fake->contents()),
	    RST::system_message(3, $source, $lineno,
				qq(Error in "$name" directive: may contain a single paragraph only.));
    }
}

# Built-in handler for role directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub role {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';

    my($args, $options) =
	map($dhash->{$_}, qw(args options));

    my ($role, $tag) = $args =~ /^([^\( ]*)(?:\s*\(\s*(.*?)\s*\))?/;
    return RST::UnknownRole($tag, $source, $lineno, $lit)
	if defined $tag && !defined $RST::MY_ROLES{$tag};
    my $msg = RST::DefineRole($role, $tag, %$options);
    $msg = $msg =~ /invalid/i ?
	qq(Error in "$name" directive:\n$msg) :
	qq(Invalid argument for "$name" directive:\n$msg)
	if $msg;

    return RST::system_message(3, $source, $lineno, $msg, $lit)
	if $msg;
    return;
}

# Built-in handler for sectnum directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub sectnum {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $content, $options) =
	map($dhash->{$_}, qw(args content options));

    return system_message($name, 3, $source, $lineno,
			  "no content permitted.", $dtext)
	if ($content ne '');
    my ($err) = arg_check($name, $source, $lineno, $args, $lit, 0);
    return $err if $err;
    my @optlist = qw(depth prefix start suffix);
    $err = check_option_names($name, $options, \@optlist, $source, $lineno,
			      $lit);
    return $err if $err;
    foreach my $optname (qw(depth start)) {
	if (defined $options->{$optname}) {
	    my $opt = $options->{$optname};
	    my $err = check_int_option($name, $optname, $opt, $opt, $source,
				       $lineno, $lit, 0);
	    return $err if $err;
	}
    }

    my $pending = new DOM('pending');
    $pending->{internal}{'.transform'} =
	"docutils.transforms.parts.Sectnum";
    %{$pending->{internal}{'.details'}} = %$options;
    return $pending;
}

# Built-in handler for rubric directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub rubric {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
#use PrintVar;PrintVar::PrintVar(\@_);print "\n";
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';

    (my $args = $dhash->{args}) =~ s/\n/ /g;

    return no_args($name, $source, $lineno, $lit) if $args eq '';
    return system_message($name, 3, $source, $lineno,
			  qq(no content permitted.), $lit)
	if $dhash->{content} ne '';

    my $rub = new DOM($name);
    $rub->append(newPCDATA DOM($args));

    return $rub;
}

# Built-in handler for sidebar directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub sidebar {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
#use PrintVar;PrintVar::PrintVar(\@_);print "\n";
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $content, $options, $content_lineno) =
	map($dhash->{$_}, qw(args content options content_lineno));

    $args =~ s/\n//g;
    my @optlist = qw(subtitle);
    my $err = check_option_names($name, $options, \@optlist, $source, $lineno,
				 $lit);
    return $err if $err;

    return RST::system_message(3, $source, $lineno,
			       qq(The "$name" directive may not be used within a sidebar element.),
			       $lit)
	if $parent->{tag} eq 'sidebar';
    return RST::system_message(3, $source, $lineno,
			       qq(The "$name" directive may not be used within topics or body elements.),
			       $lit)
	unless $parent->{tag} =~ /section|document/;

    my $sb = new DOM($name);
    my $title = new DOM('title');
    $sb->append($title);
    $title->append(newPCDATA DOM($args));
    if (defined $options->{subtitle}) {
	my $st = new DOM('subtitle');
	$st->append(newPCDATA DOM($options->{subtitle}));
	$sb->append($st);
    }
    RST::Paragraphs($sb, $content, $source, $content_lineno);
    return $sb;
}

# Built-in handler for table directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub table {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $content, $content_lineno, $options) =
	map($dhash->{$_}, qw(args content content_lineno options));

    my %common_opts = (class=>'@classes', align=>'align');
    my %myopts = ($name eq 'csv-table' ?
		  (widths=>'', 'header-rows'=>'', 'stub-columns'=>'',
		   'header'=>'') :
		  (),
		  %common_opts);
    if (defined $options->{align}) {
	my @vallist = qw(left center right);
	my $err = check_enum_option($name, 'align', $options->{align},
				    \@vallist, $source, $lineno, $lit);
	return $err if $err;
    }

    my @dom;
    my $table = new DOM('table');
    $table->{tableattr} = $main::opt_D{tableattr};
    push(@dom, $table);
    foreach my $opt (keys %myopts) {
	if (defined $myopts{$opt} && $myopts{$opt} ne '' && $options->{$opt}) {
	    if ($myopts{$opt} =~ /^@(.*)/) {
		$table->{attr}{$1} = [ $options->{$opt} ];
	    }
	    else {
		$table->{attr}{$myopts{$opt}} = $options->{$opt};
	    }
	}
#	$table->{attr}{$myopts{$opt}} = $options->{$opt}
    }

    my $title = $args;
    if ($title !~ /^(..)?$/) {
	my $titledom = new DOM('title');
	my $fake = new DOM('fake');
	RST::Paragraphs($fake, $title, $source, $lineno);
	push @dom, grep($_->{tag} ne 'paragraph', $fake->contents());
	my @paras = grep($_->{tag} eq 'paragraph', $fake->contents());
	if (@paras == 1 && $paras[0]{tag} eq 'paragraph') {
 	    $titledom->append($paras[0]->contents());
 	    $table->append($titledom);
 	}
    }

    if ($name eq 'csv-table') {
	return RST::system_message(3, $source, $lineno,
				   qq("$name" directive may not both specify an external file and have content.),
				   $lit)
	    if $dhash->{content} ne '' && defined $options->{file};
	return RST::system_message(3, $source, $lineno,
				   qq(The "file" and "url" options may not be simultaneously specified for the "$name" directive.),
				   $lit)
	    if defined $options->{file} && defined $options->{url};
	my $content = $dhash->{content};
	if (defined $options->{file}) {
	    if (open CSV, $options->{file}) {
		$content = join '', <CSV>;
		return RST::system_message(3, $source, $lineno,
					   qq(No table data detected in CSV file.),
					   $lit)
		    if $content eq '';
	    }
	    else {
		my $err = system_error();
		return RST::system_message(4, $source, $lineno,
					   qq(Problems with "$name" directive path:\n$err: '$options->{file}'.),
					   $lit);
	    }
	    my $encoding = $options->{encoding} || '';
	    if ($encoding eq 'latin-1') {
		return RST::system_message(3, $source, $lineno,
					   qq(Error with CSV data in "$name" directive:\nstring with NUL bytes), $lit)
		    if $content =~ /\000/;
	    }
	    elsif ($encoding ne '') {
		use Encode qw(encode decode);
		$content = decode($encoding, $content);
	    }
	}
	elsif (defined $options->{url}) {
	    my $url = $options->{url};
	    return RST::system_message(4, $source, $lineno,
				       qq(Problems with "$name" directive URL "$url":\nunknown url type: $url.)
				       , $lit)
		unless $url =~ /^(($URIre::scheme):(?:$URIre::hier_part|$URIre::opaque_part))/o; 
	    return RST::system_message(4, $source, $lineno,
				       qq(The "url" option is not yet implemented for the "$name" directive.),
				       $lit)
		if defined $options->{url};
	}
	my $delim = $options->{delim};
	if (! defined $delim) {
	    $delim = ',';
	}
	elsif ($delim eq 'space') {
	    $delim = ' ';
	}
	elsif ($delim =~ /^0x([\da-f]{2})/i) {
	    $delim = chr hex $1;
	}
	elsif ($delim =~ /^U\+(\d+)/) {
	    return bad_option($name, 'delim', $delim, 3, $source, $lineno,
			      qq(code too large (long int too large to convert to int).),
			      $lit)
		if $1 > 0xffffffff;
	    
	}
	elsif (length($delim) ne 1) {
	    return bad_option($name, 'delim', $delim, 3, $source, $lineno,
			      qq('$delim' invalid; must be a single character or a Unicode code.),
			      $lit);
	}
	$delim = "\Q$delim";
	return RST::system_message(2, $source, $lineno,
				   qq(The "$name" directive requires content; none supplied.),
				   $lit)
	    if $content eq '' &&
	    ! defined $options->{file} && ! defined $options->{url};
	my %lines;
	my $rows;
	$rows = ParseCSV($content, \%lines, $delim);
	return RST::system_message(3, $source, $lineno,
				   qq(Error with CSV data in "$name" directive:\n$rows), $lit)
	    unless ref($rows) eq 'ARRAY';
	return $rows if ref($rows) eq 'DOM';
	my $cols = 0;
	grep (do {$cols = @$_ if @$_ > $cols}, @$rows);
	my $tg = new DOM('tgroup', cols=>$cols);
	$table->append($tg);
	my @widths;
	if (defined (my $widths = $options->{widths})) {
	    @widths = split /[, ]\s*/, $widths;
	    return RST::system_message(3, $source, $lineno,
				       qq("$name" widths do not match the number of columns in table ($cols).),
				       $lit)
		if @widths != $cols;
	    foreach (@widths) {
		my $err = check_int_option($name, 'widths', $_, $widths,
					   $source, $lineno, $lit, 1);
		return $err if $err;
	    }
	}
	return RST::system_message(3, $source, $lineno,
				   qq($options->{'stub-columns'} stub column(s) specified but only $cols column(s) of data supplied ("$name" directive).),
				   $lit)
	    if ($options->{'stub-columns'} || 0) > $cols;
	return RST::system_message(3, $source, $lineno,
				   qq(Insufficient data supplied ($cols column(s)); no data remaining for table body, required by "$name" directive.),
				   $lit)
	    if ($options->{'stub-columns'} || 0) == $cols;
	for (my $i=0; $i < $cols; $i++) {
	    my $cs = new DOM('colspec');
	    $tg->append($cs);
	    $cs->{attr}{colwidth} = $i < @widths ? $widths[$i] :
		int(100/$cols);
	    $cs->{attr}{stub} = 1 if defined $options->{'stub-columns'} &&
		$i < $options->{'stub-columns'};
	}
	my $heads;
	if (defined $options->{'header'}) {
	    $heads = ParseCSV($options->{'header'}, \%lines, $delim);
	    return RST::system_message(3, $source, $lineno,
				       qq(Error with CSV data in "$name" directive:\n$heads), $lit)
		unless ref($heads) eq 'ARRAY';
	}
	return RST::system_message(3, $source, $lineno,
				   qq($options->{'header-rows'} header row(s) specified but only @{[0+@$rows]} row(s) of data supplied ("$name" directive).),
				   $lit)
	    if ($options->{'header-rows'} || 0) > @$rows;
	return RST::system_message(3, $source, $lineno,
				   qq(Insufficient data supplied (@{[0+@$rows]} row(s)); no data remaining for table body, required by "$name" directive.),
				   $lit)
	    if ($options->{'header-rows'} || 0) == @$rows;
	my @heads = defined $heads ? @$heads : $options->{'header-rows'}
	? splice(@$rows, 0, $options->{'header-rows'}) : ();
	
	foreach my $section (qw(thead tbody)) {
	    my $hb_rows = $section eq 'thead' ? \@heads : $rows;
	    next unless @$hb_rows;
	    my $sec = new DOM($section);
	    $tg->append($sec);
	    foreach my $row (@$hb_rows) {
		my $r = new DOM('row');
		$sec->append($r);
		for (my $entry = 0; $entry < $cols; $entry++) {
		    my $e = new DOM('entry');
		    $r->append($e);
		    my $lines = $lines{$row}[$entry] || 0;
		    RST::Paragraphs($e, $row->[$entry], $source,
				    $content_lineno+$lines);
		    $e->{entryattr} = $main::opt_D{entryattr}
		    if defined $main::opt_D{entryattr} &&
			$main::opt_D{entryattr} ne '';
		}
	    }
	}
    }
    elsif ($name eq 'list-table') {
	my $rows = ParseListTable($content, $source, $content_lineno);
	return RST::system_message(3, $source, $lineno,
				   qq(Error parsing content block for the "$name" directive: $rows.), $lit)
	    unless ref($rows) eq 'ARRAY';
	return RST::system_message(3, $source, $lineno,
				   qq($options->{'header-rows'} header row(s) specified but only @{[0+@$rows]} row(s) of data supplied ("$name" directive).),
				   $lit)
	    if ($options->{'header-rows'} || 0) > @$rows;
	return RST::system_message(3, $source, $lineno,
				   qq(Insufficient data supplied (@{[0+@$rows]} row(s)); no data remaining for table body, required by "$name" directive.),
				   $lit)
	    if ($options->{'header-rows'} || 0) == @$rows;
	my $cols = @{$rows->[0]};
	my $tg = new DOM('tgroup', cols=>$cols);
	$table->append($tg);
	my @widths;
	if (defined (my $widths = $options->{widths})) {
	    @widths = split /[, ]\s*/, $widths;
	    return RST::system_message(3, $source, $lineno,
				       qq("$name" widths do not match the number of columns in table ($cols).),
				       $lit)
		if @widths != $cols;
	    foreach (@widths) {
		my $err = check_int_option($name, 'widths', $_, $widths,
					   $source, $lineno, $lit, 1);
		return $err if $err;
	    }
	}
	return RST::system_message(3, $source, $lineno,
				   qq($options->{'stub-columns'} stub column(s) specified but only $cols column(s) of data supplied ("$name" directive).),
				   $lit)
	    if ($options->{'stub-columns'} || 0) > $cols;
	return RST::system_message(3, $source, $lineno,
				   qq(Insufficient data supplied ($cols column(s)); no data remaining for table body, required by "$name" directive.),
				   $lit)
	    if ($options->{'stub-columns'} || 0) == $cols;
	for (my $i=0; $i < $cols; $i++) {
	    my $cs = new DOM('colspec');
	    $tg->append($cs);
	    $cs->{attr}{colwidth} = $i < @widths ? $widths[$i] :
		int(100/$cols);
	    $cs->{attr}{stub} = 1 if defined $options->{'stub-columns'} &&
		$i < $options->{'stub-columns'};
	}
	my $heads;
	my @heads = defined $heads ? @$heads : $options->{'header-rows'}
	? splice(@$rows, 0, $options->{'header-rows'}) : ();
	
	foreach my $section (qw(thead tbody)) {
	    my $hb_rows = $section eq 'thead' ? \@heads : $rows;
	    next unless @$hb_rows;
	    my $sec = new DOM($section);
	    $tg->append($sec);
	    foreach my $row (@$hb_rows) {
		my $r = new DOM('row');
		$sec->append($r);
		for (my $entry = 0; $entry < $cols; $entry++) {
		    my $e = new DOM('entry');
		    $r->append($e);
		    $e->append(@{$row->[$entry]});
		    $e->{entryattr} = $main::opt_D{entryattr}
		    if defined $main::opt_D{entryattr} &&
			$main::opt_D{entryattr} ne '';
		}
	    }
	}
    }
    else {
	return RST::system_message(2, $source, $lineno,
				   qq(The "$name" directive requires content; none supplied.),
				   $lit)
	    if $content eq '';
	my $fake = new DOM('fake');
	RST::Paragraphs($fake, $content, $source, $content_lineno);
	$table->append($fake->{content}[0]->contents())
	    if $fake->{content}[0]{tag} eq 'table';
    }
    return @dom;
}

# Parses a comma-separated-value (CSV) table
# Arguments: text string, ref to lines hash, delimiter
# Returns: reference to an array of parsed rows, each of which is a 
#          reference to an array of strings for the elements of that row
#          OR a string error message
# Side-effects: Sets the lines hash to have as keys the row reference
#               and as values a reference to an array containing the 
#               relative line number where each row entry starts (for
#               giving better error messages);
sub ParseCSV {
    my ($string, $lines, $delim) = @_;
    my @rows;
    my @split = split /[ ]*(\".*?\"|[^$delim\n]+)/s, $string;
    shift @split;
    my $row;
    my $line = 0;
    while (my($val, $sep) = splice @split, 0, 2) {
	$sep = '' if ! defined $sep;
	push @rows, ($row = []) unless $row;
	return "newline inside string" if $val =~ /^\"[^\"]*$/;
	$val = $1 if $val =~ /^\"(.*)\"\s*$/s;
	push @$row, $val;
	push @{$lines->{$row}}, $line;
	$row = undef if $sep =~ /\n/;
	$line += ($val =~ tr/\n//) + ($sep =~ tr/\n//);
    }
    return \@rows;
}

# Parses a list table
# Arguments: text string, source file, source line number
# Returns: reference to an array of parsed rows, each of which is a 
#          reference to an array of references to of array of DOM objects
#          for the elements of that row
#          OR a string error message
sub ParseListTable {
    my ($string, $source, $lineno) = @_;
    my @rows;
    my $fake = new DOM('fake');
    RST::Paragraphs($fake, $string, $source, $lineno);
    my $cols = 0;
    my $bl1 = $fake->{content}[0];
    return "exactly one bullet list expected"
	if $bl1->{tag} ne 'bullet_list' ||
	grep($_->{tag} eq 'bullet_list', $fake->contents()) > 1;
    for (my $row=0; $row < $bl1->num_contents(); $row++) {
	my $li1 = $bl1->{content}[$row];
	my $bl2 = $li1->{content}[0];
	return "two-level bullet list expected, but row @{[$row+1]} does not contain a second-level bullet list"
	    unless $li1->{tag} eq 'list_item' && $bl2->{tag} eq 'bullet_list';
	return "uniform two-level bullet list expected, but row @{[$row+1]} does not contain the same number of items as row 1 (@{[$bl2->num_contents()]} vs $cols)"
	    if $row > 0 && $bl2->num_contents() ne $cols;
 	for (my $col=0; $col < $bl2->num_contents(); $col++) {
	    push @{$rows[$row]}, $bl2->{content}[$col]{content};
 	}
	$cols = $bl2->contents() if $row eq 0;
	
    }
    return \@rows;
}

# Built-in handler for target-notes directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub target_notes {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $content, $content_lineno, $options) =
	map($dhash->{$_}, qw(args content content_lineno options));
    my ($err) = arg_check($name, $source, $lineno, $args, $lit, 0);
    return $err if $err;
    my @optlist = qw();
    $err = check_option_names($name, $options, \@optlist, $source, $lineno,
			      $lit);
    return $err if $err;
    return system_message($name, 3, $source, $lineno,
			  "no content permitted.", $dtext)
	if ($content ne '');

    my $pending = new DOM('pending');
    $pending->{internal}{'.transform'} =
	"docutils.transforms.references.TargetNotes";
    %{$pending->{internal}{'.details'}{depth}} = %$options
	if defined $options;
    return $pending;
}

# Built-in handler for test_directive directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub test_directive {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($dname, $args, $content, $options) =
	map($dhash->{$_}, qw(name args content options));

    my $contlit = $content;
    my $contstr = $content eq '' ? ' None' : '';
    $args = "'$args'" if $args ne '';
    my $opt;
    foreach $opt (sort keys %$options) {
	my $err = check_required_option($dname, $opt, $options->{$opt},
					$source, $lineno, $lit);
	return $err if $err;
    }
    my $optstring =
	join('; ', map(do { my ($opt, $val) = ($_, $options->{$_});
			    $val =~ s/\n/\\n/g;
			    "'$opt': '$val'"; }, sort keys %$options));
    return RST::system_message(1, $source, $lineno,
			       qq(Directive processed. Type="$dname", arguments=[$args], options={$optstring}, content:$contstr),
			       $contlit);
}

# Built-in handler for title directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub title {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my $args = $dhash->{args};
    return no_args($name, $source, $lineno, $lit) if $args eq '';
    return system_message($name, 3, $source, $lineno,
			  "no content permitted.", $dtext)
	if ($dhash->{content} ne '');
    $RST::TOPDOM->{'.details'}{title} = $args;
    return;
}

# Built-in handler for topic directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub topic {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $content, $content_lineno) =
	map($dhash->{$_}, qw(args content content_lineno));

    my ($err) = arg_check($name, $source, $lineno, $args, $lit, 1);
    return $err if $err;
    return RST::system_message(2, $source, $lineno,
			       qq(Content block expected for the "$name" directive; none found.),
			       $lit)
	if $content =~ /^$/;

    my $topic = new DOM('topic');
    my $title = new DOM('title');
    $topic->append($title);
    $title->append(newPCDATA DOM($args));
    RST::Paragraphs($topic, $content, $source, $content_lineno);
    return $topic;
}

# Built-in handler for unicode directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub unicode {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $content, $options) =
	map($dhash->{$_}, qw(args content options));

    my $fake = new DOM('fake');
    my $text = $args;
    return no_args($name, $source, $lineno, $lit) if $text eq '';
    return RST::system_message(3, $source, $lineno,
			       qq(Invalid context: the "$name" directive can only be used within a substitution definition.),
			       $lit)
	unless $parent->{tag} eq 'substitution_definition';
    my @optlist = qw(trim ltrim rtrim);
    my $err = check_option_names($name, $options, \@optlist, $source, $lineno,
			      $lit);
    return $err if $err;

    # Remove comments
    $text =~ s/\s*\.\..*//;
    
    my @chars = split /\s+/, $text;
    for (my $c=0; $c < @chars; $c++) {
	my $char = $chars[$c];
	if ($char =~ /^(?:0x|\\u|U[+]?)([\da-f]+)$/i ||
	    $char =~/^&\#x([\da-f]+);$/i) {
	    my $hex = $1;
	    $hex =~ s/^0+//;
	    my $icc = "Invalid character code: $char";
	    return RST::system_message(3, $source, $lineno,
				       qq($icc\nValueError: code too large (unichr(int("$1", 16)))),
				       $lit)
		if length $hex > 8;
	    my $dec = hex $hex;
	    return RST::system_message(3, $source, $lineno,
				       qq($icc\nunichr(int("$1", 16))),
				       $lit)
		if $dec > 0xffff;
	    $chars[$c] = chr($dec);
	}
    }
    @$options{keys %$options} = (1) x keys %$options;
    if ($options->{trim}) {
	$options->{ltrim} = $options->{rtrim} = $options->{trim};
	delete $options->{trim};
    }
    @{$parent->{attr}}{keys %$options} = values %$options;
    return @chars ?  newPCDATA DOM(join "\n", @chars) : ();
}

################ RST::Directive internal routines ################

# INTERNAL ROUTINE.
# Returns a system error if a directive argument check fails.
# May also be useful to plug-in directives.
# Arguments: name, source, lineno, args, lit text, expected # of args
# Returns: DOM if the check fails
sub arg_check {
    my ($name, $source, $lineno, $args, $lit, $exp) = @_;

    my @dom;
    my (@args) = split(/\s+/, $args);
    my $got = @args;
    push(@dom, system_message($name, 3, $source, $lineno,
			      "$exp argument(s) required, $got supplied.",
			      $lit))
	unless $got >= $exp;
    
    return @dom;
}

# INTERNAL ROUTINE.
# Returns a system message specifying a bad option.
# May also be useful to plug-in directives.
# Arguments: directive name, option name, option value, level, source, 
#            line number, message, optional literal block
# Returns: system_message DOM
sub bad_option {
    my($name, $opt, $val, $level, $source, $lineno, $msg, $lit) = @_;

    my $valstr = $val eq '' ? "None" : "'$val'";
    return
	system_message($name, $level, $source, $lineno,
		       qq(invalid option value: (option: "$opt"; value: $valstr)\n$msg),
		       $lit);
}

# INTERNAL ROUTINE.
# Returns a system message DOM if the option value does not parse as one
# of an enumerated list.
# May also be useful to plug-in directives.
# Arguments: directive name, option name, option value, ref to array of
#            enum names, source, line number, literal
# Returns: error DOM or None
sub check_enum_option {
    my($name, $opt, $val, $vallist, $source, $lineno, $lit) = @_;

    my $search = join("|", @$vallist);
    if ($val !~ /^($search)$/i) {
	my @list = map(qq("$_"), @$vallist);
	$list[-1] =~ s/^/or /;
	my $list = join(', ',@list);
	return bad_option($name, $opt, $val, 3, $source, $lineno,
			  qq(must supply an argument; choose from $list.),
			  $lit)
	    if $val eq '';
	return bad_option($name, $opt, $val, 3, $source, $lineno,
			  qq("$val" unknown; choose from $list.), $lit);
    }
    return;
}

# INTERNAL ROUTINE.
# Returns a system message DOM if the option value does not parse as integer.
# May also be useful to plug-in directives.
# Arguments: directive name, option name, option value, complete option string,
#            source, line number, literal,
#            optional argument indicating minimum OK value (0 or 1)
# Returns: error DOM or None
sub check_int_option {
    my($name, $opt, $val, $option, $source, $lineno, $lit, $pos) = @_;
    $pos = '' unless defined $pos;
    return bad_option($name, $opt, $option, 3, $source, $lineno,
		      "invalid literal for int(): None.", $lit)
	if $val eq '';
    return bad_option($name, $opt, $option, 3, $source, $lineno,
		      "invalid literal for int(): $val.", $lit)
	unless $val =~ /^(-\s*)?\d+$/;
    return bad_option($name, $opt, $option, 3, $source, $lineno,
		      "negative value; must be positive or zero.", $lit)
	if $pos eq '0' && $val =~ /^-\s*\d+$/;
    return bad_option($name, $opt, $option, 3, $source, $lineno,
		      "negative or zero value; must be positive.", $lit)
	if $pos eq '1' && $val < 1;
    return;
}

# INTERNAL ROUTINE.
# Returns a system message DOM if any of the options in the option hash are
# not in the legal option list array.
# May also be useful to plug-in directives.
# Arguments: directive name, reference to option hash, reference to array
#            of legal option names, source, line number, literal
sub check_option_names {
    my ($name, $options, $optlist, $source, $lineno, $lit) = @_;

    my @badoptions = grep(/ /, sort keys %$options);
    return system_message($name, 3, $source, $lineno,
			  qq(invalid option data: extension option field name may not contain multiple words.),
			  $lit)
	if @badoptions;

    my %optlist;
    @optlist{@$optlist} = (1) x @$optlist;
    @badoptions = grep(! $optlist{$_}, sort keys %$options);
    return system_message($name, 3, $source, $lineno, 
			  qq(unknown option: "$badoptions[0]".), $lit)
	if @badoptions;

    return;
}

# INTERNAL ROUTINE.
# Returns a system message DOM if the option value is empty.
# May also be useful to plug-in directives.
# Arguments: directive name, option name, option value, source, line number, 
#            literal
# Returns: error DOM or none
sub check_required_option {
    my($name, $opt, $val, $source, $lineno, $lit) = @_;

    return bad_option($name, $opt, $val, 3, $source, $lineno,
		      qq(argument required but none supplied.), $lit)
	if $val eq '';
}

# INTERNAL ROUTINE.
# Returns a system message DOM if the option value does not parse as a
# positive measure of a defined unit.
# May also be useful to plug-in directives.
# Arguments: directive name, option name, option value, source, line number, 
#            literal, list of extra units
# Returns: error DOM or none
# Uses globals: @RST::UNITS
# Causes side-effects: option value has whitespace removed if valid
sub check_units_option {
    my($name, $opt, $val, $source, $lineno, $lit, @extra_units) = @_;

    my $search = join("|", @RST::UNITS, @extra_units);
    if ($_[2] !~ s/^(\d+(?:\.\d*)?|\.\d+)\s*($search)\s*$/$1$2/i) {
	return bad_option($name, $opt, $val, 3, $source, $lineno,
			  qq(must supply an argument.), $lit)
	    if $val eq '';
	my $units = join ' ', map(qq("$_"), @RST::UNITS, @extra_units);
 	return bad_option($name, $opt, $val, 3, $source, $lineno,
 			  qq(not a positive measure of one of the following units:\n$units.),
			  $lit);
    }
}

# Adds a handler for a directive.  Used by plug-in directives to 
# register a routine to call for a given directive name.
# Arguments: directive name, reference to directive subroutine
sub handle_directive {
    my($name, $sub) = @_;

    $RST::DIRECTIVES{lc $name} = $sub;
}

# INTERNAL ROUTINE.
# Returns a system message when no arguments were supplied
# Arguments: directive name, level, source, lineno, literal string,
# Returns: system_message DOM
sub no_args {
    my ($name, $source, $lineno, $lit) = @_;
    return system_message($name, 3, $source, $lineno,
			  qq(1 argument(s) required, 0 supplied.), $lit);
}


# INTERNAL ROUTINE.
# Parses the text of a directive into its arguments, options, and contents.
# May also be useful to plug-in directives.
# Arguments: Directive text, literal text, source, lineno
# Returns: Error DOM or `directive arguments hash ref`_
sub parse_directive {
    my($dtext, $lit, $source, $lineno) = @_;

    my ($pre, $directive, $body) = $dtext =~ /(\s*)([\w\.-]+)\s*:: *(.*)/s;
    my $dname = $directive;
    $directive =~ tr/[A-Z].-/[a-z]__/;

    # Parse the body into arguments, options, and content
    my $args;
    if ((my @s = split /(^ *($RST::FIELD_LIST|::)|\n\n)/mo, $body, 2) > 1) {
	$args = $s[0];
	$body = "$s[1]$s[-1]";
    }
    else {
	$args = $body;
	$body = "";
    }

    my $content_lineno = $lineno + ($args =~ tr/\n//);
    $args =~ s/^\n//;
    $args =~ s/^   //mg;
    $args =~ s/\n$//;
    if (! defined $args && $body !~ /^ *($RST::FIELD_LIST|::)/o
	&& (my @s = split /^(\n(?:\n+))/m, $lit, 2) > 1) {
	# Any ensuing paragraph is a block quote
	$lit = "$s[0]\n";
	$body =~ /^\n\n/m;
	$content_lineno += 2;
	$body = "$s[0]\n";
    }
    my $spaces = "   ";
    my %options;
    $body =~ s/^$spaces//mg;
    my $err = 0;
    if (defined $args || $body =~ /^ *($RST::FIELD_LIST:::)/o) {
	my $options;
	if ((my @s = split /^(\n+)/m, $body, 2) > 1) {
	    $options = "$s[0]";
	    my $pre = "$s[0]$s[1]";
	    $body = "$s[-1]";
	    $content_lineno += ($pre =~ tr/\n//);
	}
	else {
	    $options = $body;
	    $body = "";
	}
	my @options = split /^(?=:)/m, $options;
	my $option;
	foreach $option (@options) {
	    my ($opt,$val) = $option =~ /^:([^:\n]*): *(.*)/s;
	    return system_message($directive, 3, $source, $lineno,
				  "invalid option block.", $lit)
		if $opt eq '';
	    if (defined $options{$opt}) {
		$err = 1;
		return
		    system_message($directive, 3, $source, $lineno,
				   qq(invalid option data: duplicate option "$opt".),
				   $lit);
	    }
	    chomp $val;
	    if ($val =~ /^(?!\A)( *)/m) {
		my $spaces = $1;
		$val =~ s/^$spaces//gm;
	    }
	    $options{$opt} = $val;
	}
    }
#print "[$args][",join(',',map("$_=>$options{$_}",sort keys %options)),"][$body]\n";

    my $dhash = {name=>$dname, args=>$args, content=>$body,
		 content_lineno=>$content_lineno};
    $dhash->{options} = \%options if %options;
    return $dhash;
}

# INTERNAL ROUTINE.
# Returns a canonically formatted version of the last system error.
# Arguments: None
# Returns: error string
sub system_error {
    return "[Errno " . ($!+0) . "] $!";
}

# INTERNAL ROUTINE.
# Returns a DOM object for a system message, with 'Error in "name" directive"
# prepended to the message.
# May also be useful to plug-in directives.
# Arguments: directive name, level, source, lineno, message, literal string,
#            attribute pairs
sub system_message {
    my ($name, $level, $source, $lineno, $msg, $lit, %attr) = @_;
    return RST::system_message($level, $source, $lineno,
			       qq(Error in "$name" directive:\n$msg), $lit,
			       %attr);
}

package Util;

# Does a "deep" copy of a data structure
# Inputs: variable
# Returns: deep copy of variable
sub DeepCopy {
    my($var) = @_;
    return $var if ref($var) eq '';
    if ("$var" =~ /HASH/) {
	my(%val);
	@val{keys %$var} = map(DeepCopy($_),values %$var);
	return \%val;
    }
    elsif ("$var" =~ /ARRAY/) {
	my(@val) = map(DeepCopy($_), @$var);
	return \@val;
    }
    elsif ("$var" =~ /SCALAR/) {
	my($val) = DeepCopy($$var);
	return \$val;
    }
    elsif ("$var" =~ /CODE/) {
	return $var;
    }
    else {
	my $val = "$var";
	return $val;
    }
}

1;
