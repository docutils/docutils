# Purging old short\.dir directory
# Unpacking data from init
# Unpacking data from short\.init
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
# START  Test functional features with short tests
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

# START \+ Simple re difference good
3d2
< This line has been deleted\\\.
6d4
< This line has also been deleted\\\.
7a6
> This line has been added in the modified section\.
8a8
> This line is added\.
# END   \+ Simple re difference good

# START \+ Simple text difference good
# END   \+ Simple text difference good

# START \+ Simple text difference with -D FOO good
# END   \+ Simple text difference with -D FOO good

# START \+ Simple difference with -r good
3d2
< This line has been deleted\.
6d4
< This line has also been deleted\.
7a6
> This line has been added in the modified section\.
8a8
> This line is added\.
# END   \+ Simple difference with -r good

# START \+ Get minimal difference when quick\+dirty method does not work good
# END   \+ Get minimal difference when quick\+dirty method does not work good

# START \+ Many lines deleted from first file good
# END   \+ Many lines deleted from first file good

# START \+ Totally different files good
# END   \+ Totally different files good

# START \+ File a is prefix of file b good
# END   \+ File a is prefix of file b good

