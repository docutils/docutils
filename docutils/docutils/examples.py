# Authors: David Goodger
# Contact: goodger@python.org
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
This module contains practical examples of Docutils client code.  These
functions may be used as-is, or as models for variations.
"""

from docutils import core


def html_parts(input_string, destination_path=None,
               input_encoding='unicode', doctitle=1):
    """
    Given an input string, returns a dictionary of HTML document parts.

    Dictionary keys are the names of parts, and values are Unicode strings;
    encoding is up to the client.

    Parameters:

    - `input_string`: A multi-line text string; required.
    - `destination_path`: Path to the file or object which will receive the
      output; optional.  Used for determining relative paths (stylesheets,
      source links, etc.).
    - `input_encoding`: The encoding of `input_string`.  If it is an encoded
      8-bit string, provide the correct encoding.  If it is a Unicode string,
      use "unicode", the default.
    - `doctitle`: Disable the promotion of a lone top-level section title to
      document title (and subsequent section title to document subtitle
      promotion); enabled by default.
    """
    overrides = {'input_encoding': input_encoding,
                 'doctitle_xform': doctitle}
    parts = core.publish_parts(
        source=input_string, destination_path=destination_path,
        writer_name='html', settings_overrides=overrides)
    return parts

def html_fragment(input_string, destination_path=None,
                  input_encoding='unicode', output_encoding='unicode',
                  doctitle=1):
    """
    Given an input string, returns an HTML fragment as a string.

    The return value is the contents of the <body> tag, less the title,
    subtitle, and docinfo.

    Parameters (see `html_parts()` for the remainder):

    - `output_encoding`: The desired encoding of the output.  If a Unicode
      string is desired, use the default value of "unicode" .
    """
    parts = html_parts(
        input_string=input_string, destination_path=destination_path,
        input_encoding=input_encoding, doctitle=doctitle)
    fragment = parts['fragment']
    if output_encoding != 'unicode':
        fragment = fragment.encode(output_encoding)
    return fragment
