# Purging old driver\.dir directory
# Unpacking data from init
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
# START  Test error messages
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

# START - Cannot create log file--should error
Test Run By (nodine|.*) on (Sat Feb 28 21:11:48 CST 2004|.*)
ERROR: Cannot open A\.t\.log for write at (\.\./\.\./\.\./\.\./bin/|.*)run_gress line (72|.*)\.
# END   - Cannot create log file--should error

# START \+ Can create log file good
Test Run By (nodine|.*) on (Sat Feb 28 21:11:48 CST 2004|.*)
# Beginning A\.t/a tests
No errors

PASSED: directory A\.t had no errors

PASSED: No errors
# END   \+ Can create log file good

# START - Cannot rename log file--should error
Test Run By (nodine|.*) on (Sat Feb 28 21:11:48 CST 2004|.*)
ERROR: Cannot rename A\.t\.log at (\.\./\.\./\.\./\.\./bin/|.*)run_gress line (68|.*)\.
# END   - Cannot rename log file--should error

