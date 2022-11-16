# Source and destination file names
test_source = "pep_html.txt"
test_destination = "pep_html.html"

# Keyword parameters passed to publish_file()
reader_name = "pep"
writer_name = "pep_html"
settings_overrides = {
    'python_home': "http://www.python.org",
    'pep_home': "http://www.python.org/peps",
    'no_random': 1,
    'cloak_email_addresses': 1,
    # local copy of default stylesheet:
    'stylesheet_path': 'functional/input/data/html4css1.css',
    }
