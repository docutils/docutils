# Default settings for all tests.

settings_overrides = {
    '_disable_config': True,
    'report_level': 2,
    'halt_level': 5,
    'warning_stream': '',
    'input_encoding': 'utf-8',
    'embed_stylesheet': False,
    'auto_id_prefix': '%',
    # avoid "Pygments not found"
    'syntax_highlight': 'none'
}

# initialize with the settings & definitions from test 1:
with open('functional/tests/standalone_rst_s5_html_1.py',
          encoding='utf-8') as _f:
    exec(_f.read())

# overrides specific to this test:
test_destination = 'standalone_rst_s5_html_2.html'
del settings_overrides['theme']         # use the default
settings_overrides['current_slide'] = 1
