#!/usr/bin/python

"""
Author: grubert@users.sourceforge.net
Revision: $Revision$

Split docutils/tools/test.txt up to test one by one.
"""

import sys, os, os.path

if len(sys.argv)>1:
	test_filename = sys.argv[1]
else:
	# run from sandbox subdirectory.
	test_filename = "../../tools/test.txt"

if not os.path.exists(test_filename):
	print "Testfile not found:", test_filename
	sys.exit()

print "Using file:", test_filename

class Output:
	def __init__(self,test_dir="test"):
		self._file = 0
		self._file_cnt = 0
		self._test_dir = test_dir
		self._test_file = "from_test_txt"
	def close(self):
		if self._file:
			self._file.close()
	def next_file(self):
		self.close()
		self._file_cnt += 1
		self._file = open("%s/%s-%02d.txt.new"
					%(self._test_dir,self._test_file,self._file_cnt),"w")
	def write_line(self,str):
		self._file.write(str+"\n")
	def get_count(self):
		return self._file_cnt
		
out = Output()
out.next_file()

line_before = -1
section_one = 1		# header plus first section into first file
for line in open(test_filename).readlines():
	line = line.rstrip()	# remove end of line
	# we split on "------" sections.
	if ((len(line)>0)and(line == "-"*(len(line)))and(len(line)==len(line_before))):
		if not section_one:
			out.next_file()
		print section_one,line_before,line
		section_one = 0
	if not (type(line_before)==type(-1)):
		# special treatment of start block
		out.write_line(line_before)
	line_before = line

out.close()
