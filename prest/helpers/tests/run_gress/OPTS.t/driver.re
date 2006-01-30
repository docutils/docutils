# Purging old driver\.dir directory
# Unpacking data from init
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
# START  Test command-line options
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

# START \+ -h flag good
Description: This program runs a set of gress tests in
# END   \+ -h flag good

# START \+ -s flag good
Test Run By (nodine|.*) on (Sat Feb 28 21:06:08 CST 2004|.*)
# Beginning A\.t/driver tests
2c2
< PATH=:\\\.:\\\.\\\./\\\.\\\./\\\.\\\./\\\.\\\./bin:\\\.\\\./\\\.\\\./\\\.\\\./bin:\\\.\\\./\\\.\\\./\\\.\\\./\\\.\\\./src:\\\.\\\./\\\.\\\./\\\.\\\./src:/bin
---
> PATH=/my/bin:(\.:/bin:\.\./\.\./\.\./\.\./bin:\.\./\.\./\.\./bin:\.\./\.\./\.\./\.\./src:\.\./\.\./\.\./src:|.*)
FAILURE: A\.t/driver had 1 error

FAILURE: directory A\.t had a total of 1 error

FAILURE: total of 1 error
# END   \+ -s flag good

# START \+ -V flag good
\$Id\: run_gress\.prl\.root,v( 1\.3 2004/02/29 02:37:09 nodine Exp |.*)\$
# END   \+ -V flag good

# START - Undefined flag--should error
Unknown option: Q
Usage: run_gress \[options\] file1\|dir1 \.\.\.
# END   - Undefined flag--should error

