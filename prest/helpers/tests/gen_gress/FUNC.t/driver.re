# Purging old driver\.dir directory
# Unpacking data from init
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
# START  Functional tests
#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

# START \+ Title lines good
cd rundir
echo "#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*"
echo "# START  This is a title line"
echo "#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*"
echo

  cmd 1
  cmd 2
# END   \+ Title lines good

# START \+ Positive tests good
cd rundir
echo "# START \+ Positive test good"
  cmd 1
  cmd 2
echo "# END   \+ Positive test good"
echo ""

# END   \+ Positive tests good

# START \+ Negative tests good
cd rundir
echo "# START - Negative test--should error"
  cmd 1
  cmd 2
echo "# END   - Negative test--should error"
echo ""

# END   \+ Negative tests good

# START \+ Two title lines good
cd rundir
echo "#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*"
echo "# START  Title 1"
echo "#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*"
echo

  cmd 1
  cmd 2
echo "#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*"
echo "# START  Title 2"
echo "#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*"
echo

  cmd 1
  cmd 2
# END   \+ Two title lines good

