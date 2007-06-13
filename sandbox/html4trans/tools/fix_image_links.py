#!/usr/bin/env python
# -*- coding: iso-8859-1 -*-

# Fix the image links in the html4trans sample output.

import sys

# Read file
if len(sys.argv) < 2:
    raise IndexError, "need path argument"

path = sys.argv[1]

print "opening", path

samplefile = open(path, 'r')
content = samplefile.readlines()
samplefile.close()

# Replace and write

samplefile = open(path, 'w')
for line in content:
    line = line.replace('../../../docs/user/rst/images/',
                 '../../../docutils/docs/user/rst/images/')
    samplefile.write(line)
    
print "finished replacing in", path    
    
    
                 


