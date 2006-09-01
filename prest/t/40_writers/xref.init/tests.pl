my @idx_rsts = grep s/\.idx/.rst/, <*.idx>;
my @toc_rsts = grep s/\.toc/.rst/, <*.toc>;
my @xref_rsts = grep s/\.xref/.rst/, <*.xref>;
(@idx_rsts, @toc_rsts, @xref_rsts);
