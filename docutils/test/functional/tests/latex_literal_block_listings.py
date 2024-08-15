# Source and destination file names
test_source = "latex_literal_block.rst"
test_destination = "latex_literal_block_listings.tex"

# Keyword parameters passed to publish_file()
writer = "latex"
settings_overrides = {
    'stylesheet': 'docutils',
    'legacy_column_widths': True,
    'use_latex_citations': False,
    'literal_block_env': 'lstlisting',
    'latex_preamble': r"""
% PDF Standard Fonts
\usepackage{mathptmx} % Times
\usepackage[scaled=.90]{helvet}
\usepackage{courier}
% LaTeX syntax highlight with "listings":
\lstloadlanguages{[LaTeX]TeX} %  comma separated list of languages
\newcommand{\DUCLASSlatex}{\lstset{language=[LaTeX]TeX}}
""",
}
