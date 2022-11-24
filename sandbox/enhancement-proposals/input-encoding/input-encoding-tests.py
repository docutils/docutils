#!/usr/bin/env python3
# encoding: utf-8
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause
#
# input-encoding-tests.py:
# ========================
from __future__ import print_function

import locale
from pprint import pprint
import sys

if sys.version_info < (3,):
    sys.path.append('/usr/lib/python3/dist-packages/')

import docutils
from docutils.io import FileInput
if sys.version_info < (3,):
    from docutils.utils.error_reporting import locale_encoding
else:
    from docutils.io import _locale_encoding as locale_encoding


samples = {'utf-8': u'Grüße',
           'utf-8-sig': u'Grüße',
           'utf-16': u'Grüße',
           'utf-16-le': u'Grüße',
           'utf-16-be': u'Grüße',
           'latin1': u'Grüße',
           'latin2': u'cześć',
           'latin4': u'škoda',
           'latin10': u'škoda',
           'cp775': u'cześć',
           }

samples_encoded = dict((encoding, text.encode(encoding))
                     for encoding, text in samples.items())

pprint(samples_encoded)


# for k,v in samples_encoded.items():
#     print(samples[k].encode('utf-8'), '(encoding: %s)'%k)
#     for encoding in samples:
#         if encoding == k:
#             continue
#         try:
#             text = v.decode(encoding)
#             print('  decoded with', encoding, '->', text)
#         except UnicodeError as err:
#             print('  decoded with', encoding, '-> fail')

# write sample files:

for encoding, data in samples_encoded.items():
    with open('samples/sample-'+encoding, mode='w', encoding=encoding) as f:
        f.write(data.decode(encoding))

for encoding, data in samples_encoded.items():
    with open('samples/self-declaring-'+encoding, mode='w', encoding=encoding) as f:
        if encoding in ('utf-16-be', 'utf-16-le'):
            f.write('\ufeff') # BOM (ZWNJ)
        else:
            f.write('.. encoding: %s\n'%encoding)
        f.write(data.decode(encoding))


# read sample files:

print('\nreading with standard `open`')
for encoding in sorted(samples):
    with open('samples/self-declaring-'+encoding) as f:
        try:
            text = f.read()
            print(encoding, repr(text), len(text))
        except UnicodeError:
            print(encoding, 'fail')

print('\nreading with `docutils.io.FileInput`')
for encoding in sorted(samples):
    f = FileInput(source_path='samples/sample-'+encoding)
    try:
        text = f.read()
        # l > 5 points to spurious bytes in the data
        l = len(text)
        if sys.version_info < (3,):
            text = text.encode('utf8')
        print(encoding, text, l)
    except UnicodeError as err:
        print(encoding, 'fail')

print('\nreading self-declaring file with `docutils.io.FileInput`')
for encoding in sorted(samples):
    f = FileInput(source_path='samples/self-declaring-'+encoding)
    try:
        text = f.read()
        # l > 5 points to spurious bytes in the data
        l = len(text.split()[-1]) 
        if sys.version_info < (3,):
            text = text.encode('utf8')
        print(encoding, repr(text), l)
    except UnicodeError as err:
        print(encoding, 'fail', err)


print('\nDocutils', docutils.__version__,
      ' Python', sys.version.split()[0])
print('preferred encoding:', locale.getpreferredencoding())
print('locale encoding:', locale_encoding)

