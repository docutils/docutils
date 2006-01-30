# Purging old driver\.dir directory
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
# START  Test command-line options
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

# START \+ -h flag good
Description: This program is like the Unix diff program except that
# END   \+ -h flag good

# START \+ -V flag good
\$Id\: diffre\.prl\.root,v( 1\.1 2004/03/19 21:54:50 nodine Exp |.*)\$
# END   \+ -V flag good

# START - Insufficient arguments--should error
Usage: diffre \[options\] re-file file2
# END   - Insufficient arguments--should error

# START - Undefined flag--should error
Unknown option: Q
Usage: diffre \[options\] re-file file2
# END   - Undefined flag--should error

