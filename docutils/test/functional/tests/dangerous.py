# Source and destination file names
test_source = "dangerous.txt"
test_destination = "dangerous.html"

# Keyword parameters passed to publish_file()
writer = "html4"
settings_overrides = {
    'file_insertion_enabled': False,
    'raw_enabled': False,
    # local copy of default stylesheet:
    'stylesheet_path': 'functional/input/data/html4css1.css',
    }
