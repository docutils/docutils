# Purging old driver\.dir directory
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
# START  Test command-line options
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

# START \+ -h flag good
Description: This program generates a \.sh file from a \.job file 
# END   \+ -h flag good

# START \+ No -i flag good
if \[ -d rundir\.init \]
# END   \+ No -i flag good

# START \+ No -i flag but with -s flag good
if \[ -d myrundir\.init \]
# END   \+ No -i flag but with -s flag good

# START \+ -i flag good
if \[ -d initdir \]
# END   \+ -i flag good

# START \+ No -p flag good
PATH=\$PATH_H:\.:\.\./\.\./\.\./\.\./bin:\.\./\.\./\.\./bin:\.\./\.\./\.\./\.\./src:\.\./\.\./\.\./src::\$PATH_REQ
# END   \+ No -p flag good

# START \+ With -p flag good
PATH=\$PATH_H:\.:\.\./\.\./\.\./\.\./bin:\.\./\.\./\.\./bin:\.\./\.\./\.\./\.\./src:\.\./\.\./\.\./src:mydir1:mydir2:\$PATH_REQ
# END   \+ With -p flag good

# START \+ No -s flag good
cd rundir
# END   \+ No -s flag good

# START \+ -s flag good
cd myrundir
# END   \+ -s flag good

# START \+ -V flag good
\$Id\: gen_gress\.prl\.root,v( 1\.1 2004/02/27 22:18:10 nodine Exp |.*)\$
# END   \+ -V flag good

# START - Undefined flag--should error
Unknown option: Q
Usage: gen_gress \[options\] file
# END   - Undefined flag--should error

