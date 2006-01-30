# Purging old driver\.dir directory
# Unpacking data from init
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
# START  Functional tests for run_gress
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

# START - Run a directory that contains errors--should error
Test Run By (nodine|.*) on (Sat Feb 28 20:26:59 CST 2004|.*)
# Beginning A\.t/a tests
No errors

# Beginning A\.t/b tests
7c7
< There will be a mismatch on this line
---
> There will be a mismatch on this line\.
FAILURE: A\.t/b had 1 error

# Beginning A\.t/c tests
7c7
< There will be a mismatch on this line
---
> There will be a mismatch on this line\.
11c11
< There will also be a mismatch on this line
---
> There will also be a mismatch on this line\.
FAILURE: A\.t/c had 2 errors

# WARNING: There is no executable A\.t/d\.sh corresponding to A\.t/d\.re

FAILURE: directory A\.t had a total of 3 errors

FAILURE: total of 3 errors
# END   - Run a directory that contains errors--should error

# START \+ Log file good
# END   \+ Log file good

# START \+ Run a file with no errors good
Test Run By (nodine|.*) on (Sat Feb 28 20:27:00 CST 2004|.*)
# Beginning A\.t/a tests
No errors

PASSED: directory A\.t had no errors

PASSED: No errors
# END   \+ Run a file with no errors good

# START \+ New log file good
# END   \+ New log file good

# START \+ Previous log file good
# END   \+ Previous log file good

# START - Run with no tests--should error
Test Run By (nodine|.*) on (Sat Feb 28 20:27:02 CST 2004|.*)
FAILURE: total of 0 tests run
# END   - Run with no tests--should error

# START - Run two directories containing errors--should error
Test Run By (nodine|.*) on (Sat Feb 28 20:27:03 CST 2004|.*)
# Beginning A\.t/a tests
No errors

# Beginning A\.t/b tests
7c7
< There will be a mismatch on this line
---
> There will be a mismatch on this line\.
FAILURE: A\.t/b had 1 error

# Beginning A\.t/c tests
7c7
< There will be a mismatch on this line
---
> There will be a mismatch on this line\.
11c11
< There will also be a mismatch on this line
---
> There will also be a mismatch on this line\.
FAILURE: A\.t/c had 2 errors

# WARNING: There is no executable A\.t/d\.sh corresponding to A\.t/d\.re

FAILURE: directory A\.t had a total of 3 errors

# Beginning B\.t/a tests
No errors

# Beginning B\.t/b tests
7c7
< There will be a mismatch on this line
---
> There will be a mismatch on this line\.
FAILURE: B\.t/b had 1 error

FAILURE: directory B\.t had a total of 1 error

FAILURE: total of 4 errors
# END   - Run two directories containing errors--should error

# START - Run all directories--should error
Test Run By (nodine|.*) on (Sat Feb 28 20:27:06 CST 2004|.*)
# Beginning A\.t/a tests
No errors

# Beginning A\.t/b tests
7c7
< There will be a mismatch on this line
---
> There will be a mismatch on this line\.
FAILURE: A\.t/b had 1 error

# Beginning A\.t/c tests
7c7
< There will be a mismatch on this line
---
> There will be a mismatch on this line\.
11c11
< There will also be a mismatch on this line
---
> There will also be a mismatch on this line\.
FAILURE: A\.t/c had 2 errors

# WARNING: There is no executable A\.t/d\.sh corresponding to A\.t/d\.re

FAILURE: directory A\.t had a total of 3 errors

# Beginning B\.t/a tests
No errors

# Beginning B\.t/b tests
7c7
< There will be a mismatch on this line
---
> There will be a mismatch on this line\.
FAILURE: B\.t/b had 1 error

FAILURE: directory B\.t had a total of 1 error

# Beginning C\.t/a tests
No errors

PASSED: directory C\.t had no errors

FAILURE: total of 4 errors
# END   - Run all directories--should error

# START \+ Run passing tests from multiple directories good
Test Run By (nodine|.*) on (Sat Feb 28 20:27:10 CST 2004|.*)
# Beginning A\.t/a tests
No errors

PASSED: directory A\.t had no errors

# Beginning B\.t/a tests
No errors

PASSED: directory B\.t had no errors

# Beginning C\.t/a tests
No errors

PASSED: directory C\.t had no errors

PASSED: No errors
# END   \+ Run passing tests from multiple directories good

# START \+ Wildcard argument good
Test Run By (nodine|.*) on (Sat Feb 28 20:27:12 CST 2004|.*)
# Beginning A\.t/a tests
No errors

PASSED: directory A\.t had no errors

# Beginning B\.t/a tests
No errors

PASSED: directory B\.t had no errors

# Beginning C\.t/a tests
No errors

PASSED: directory C\.t had no errors

PASSED: No errors
# END   \+ Wildcard argument good

