PERL_FLAG_badwrt1 = -I.
PERL_FLAG_badwrt2 = -I.
PERL_FLAG_badwrt3 = -I.
PERL_FLAG_badwrt4 = -I.
PERL_FLAG_html = -I.
PERL_FLAG_writer = -I.

RST_FLAG_indtarget = -D xformoff='Decorations'
RST_FLAG_badwrt1 = -w bad1
RST_FLAG_badwrt2 = -w bad2
RST_FLAG_badwrt3 = -w bad3
RST_FLAG_badwrt4 = -w bad4
RST_FLAG_dom = -D trusted
RST_FLAG_html = -D generator=0
RST_FLAG_nowrt = -w nosuchwriter
RST_FLAG_restructured = -D xformoff='Decorations'
RST_FLAG_transforms = -D xformoff='Decorations'
RST_FLAG_pending = -d -D xformoff='Decorations'
RST_FLAG_writer = -w cover -D no_line_directives

# Don't define "-W nobackn"
DOM_FLAGS :=
