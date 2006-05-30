my @toc_rsts = grep s/\.toc/.rst/, <*.toc>;
('index.rst', @toc_rsts, 'xreftest.rst');
