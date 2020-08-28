# Source and destination file names.
test_source = "latex_literal_block.txt"
test_destination = "latex_literal_block_listings.tex"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "latex"

# Extra setting we need
settings_overrides['legacy_class_functions'] = False
settings_overrides['stylesheet'] = 'docutils'
settings_overrides['syntax_highlight'] = 'none'

settings_overrides['literal_block_env'] = 'lstlisting'
settings_overrides['latex_preamble'] = r"""
% PDF Standard Fonts
\usepackage{mathptmx} % Times
\usepackage[scaled=.90]{helvet}
\usepackage{courier}
% LaTeX syntax highlight with "listings":
\lstloadlanguages{[LaTeX]TeX} %  comma separated list of languages
\newcommand{\DUCLASSlatex}{\lstset{language=[LaTeX]TeX}}
"""
