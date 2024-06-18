# Source and destination file names
test_source = "pep_html.txt"
test_destination = "pep_html.html"

# Keyword parameters passed to publish_file()
reader = "pep"
writer = "pep_html"
settings_overrides = {
    'python_home': "http://www.python.org",
    'pep_home': "http://www.python.org/peps",
    'no_random': True,
    'cloak_email_addresses': True,
    # local copy of default stylesheet:
    'stylesheet_path': 'functional/input/data/html4css1.css',
    }
