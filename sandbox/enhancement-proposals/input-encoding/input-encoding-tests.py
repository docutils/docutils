#!/usr/bin/env python3
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

import codecs
import locale
from pprint import pprint
import sys

# additional codecs from https://codeberg.org/milde/inspecting-codecs
import inspecting_codecs

import docutils
from docutils.io import FileInput
from docutils.io import _locale_encoding as locale_encoding


print('\nDocutils', docutils.__version__,
      ' Python', sys.version.split()[0])
print('preferred encoding:', locale.getpreferredencoding())
print('locale encoding:', locale_encoding)


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


print('\nreading with codec "utf_sig".')
for encoding in sorted(samples):
    with open('samples/sample-'+encoding, encoding='utf-sig') as f:
        try:
            text = f.read()
            print(encoding, repr(text), len(text))
        except UnicodeError:
            print(encoding, 'fail')

print('\nreading self-declaring files with codec "declared".')
for encoding in sorted(samples):
    with open('samples/self-declaring-'+encoding, encoding='declared') as f:
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
        print(encoding, repr(text), l)
    except UnicodeError as err:
        print(encoding, 'fail')

print('\nreading self-declaring file with `docutils.io.FileInput`')
for encoding in sorted(samples):
    f = FileInput(source_path='samples/self-declaring-'+encoding)
    try:
        text = f.read()
        # l > 5 points to spurious bytes in the data
        l = len(text.split()[-1])
        print(encoding, repr(text), l)
    except UnicodeError as err:
        print(encoding, 'fail', err)
