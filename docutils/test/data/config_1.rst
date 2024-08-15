# Test config file (new format)

[general]

source-link: on
generator: true
raw-enabled: off

[restructuredtext parser]

trim-footnote-reference-space: 1
tab-width = 8


[html4css1 writer]

stylesheet-path: stylesheets/default.css
output_encoding: ascii:xmlcharrefreplace

# general settings or settings from other components may be used,
datestamp: %Y-%m-%d %H:%M UTC


[pep_html writer]

template: pep-html-template
stylesheet-path: stylesheets/pep.css
python-home: http://www.python.org
no-random: yes


[html5 writer]

# overwrite general setting (above) for the html5 writer
source_link: off
