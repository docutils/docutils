#!/usr/bin/env python

# :Author: Guenter Milde
# :Contact: milde users.berlios.de
# :Revision: $Revision$
# :Date: $Date$
# :Copyright: Licensed under the Academic Free License version 1.2
# 
# ::

"""
Test script for the "html4trans" writer

This is no unit test but a script for interactive checking and modifying 
to get a picture of the working 
"""

import sys, os.path

from docutils.core import publish_string, publish_file

# Prepend parent dir to the PYTHONPATH and import writer module::

sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from html4trans import Writer

# Import some rst syntax samples
import textsamples


# Customisable Settings
# =====================
# 
# Customise by (un)commenting appropriate lines

# Quick test samples
# ------------------
# ::

internal_samples = [
                    textsamples.title,
                    textsamples.bibliographic_list,
                    textsamples.table_of_contents,
                    #textsamples.admonitions,
                    textsamples.literal_block,
                    textsamples.line_block,
                    textsamples.table,
                    #textsamples.system_message
                   ]

# Samples from the docutils svn data
# ----------------------------------
# ::

syntax_samples_dir = '../../../docutils/test/functional/input/'

syntax_sample_files = ['data/standard.txt',
                       'data/header_footer.txt',
                       'data/table_colspan.txt',
                       'data/table_rowspan.txt',
                       'data/table_complex.txt',
                       'data/list_table.txt',
                       'data/custom_roles.txt',
                       #'data/errors.txt'
                      ]

# read coice of syntax samples
syntax_samples = [open(syntax_samples_dir+samplefile).read()
                  for samplefile in syntax_sample_files]


# Quick test or full text
# -----------------------
# ::

#samples = internal_samples  # quick test of some selected samples
samples = syntax_samples  # (takes longer, includes intended errors)

# Configuration settings
# ----------------------

# no CSS stylesheet needed, so do not include (referencing does not harm)
# ::

overrides = {'embed_stylesheet': False}

# Path of output file
# -------------------
# ::
  
outpath = "../data/html4trans-sample.html"

# Convert and Print
# =================

# Join samples to string::

sample_string = '\n'.join(samples)

# Document tree (as pseudoxml rendering) for comparision::

## Uncomment to activate:
# doctree = publish_string(sample_string, writer_name="pseudoxml")
# print doctree 

# Convert to LaTeX::

output = publish_string(sample_string, 
                        settings_overrides=overrides,
                        writer=Writer())

# Replace image links::

output = output.replace('../../../docs/user/rst/images/',
                        '../../../docutils/docs/user/rst/images/')

# Print and save::

print output

outfile = open(outpath, 'w')
outfile.write(output)



