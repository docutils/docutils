#!/usr/bin/env python

# Author: Guenter Milde
# Contact: 
# Revision: $Revision$
# Date: $Date$
# Copyright: Licensed under the Academic Free License version 1.2

"""
Test script for the "latex2e_adaptive_preamble" writer
"""

# prepend parent dir to the PYTHONPATH
import sys, os.path
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))


from docutils.core import publish_string, publish_file

from latex2e_adaptive_preamble import Writer
import textsamples

# Sample input text::

internal_samples = "".join([
                            textsamples.title,
                            textsamples.bibliographic_list,
                            textsamples.table_of_contents,
                            # textsamples.admonitions,
                            textsamples.literal_block,
                            textsamples.line_block,
                            textsamples.table,
                            # textsamples.system_message
                           ])

# samples from the docutils svn data
syntax_samples_dir = '../../../docutils/test/functional/input/'
syntax_sample_files = ['data/standard.txt',
                       'data/table_colspan.txt',
                       'data/table_rowspan.txt',
                       # Tests for the LaTeX writer
                       'data/latex2e.txt',
                       'data/nonalphanumeric.txt',
                       'data/unicode.txt',
                       'data/custom_roles.txt',
                       # 'data/errors.txt'
                      ]
    
syntax_samples = '\n'.join([open(syntax_samples_dir+samplefile).read()
                          for samplefile in syntax_sample_files])


# Convert and Print
# -----------------

# Uncomment as desired

# document tree (as pseudoxml rendering) for comparision
# doctree = publish_string(sample, writer_name="pseudoxml")

# Quick test
output = publish_string(internal_samples, writer=Writer())

# or full test (takes longer, includes intended errors)
#output = publish_string(syntax_samples, writer=Writer())

# Replace image links
output = output.replace('../../../docs/user/rst/images/',
                        '../../../../docutils/docs/user/rst/images/')


# print doctree # document tree (as pseudoxml rendering) for comparision
print output  # * html4trans output


# save output to data sub-dir
outfile = open("../data/latex2e-adaptive-preamble-sample.tex", 'w')
outfile.write(output)



