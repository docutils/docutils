#! /usr/bin/env python
"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Transforms for PEP processing.

- `Headers`: Used to transform a PEP's initial RFC-2822 header.  It remains a
  field list, but some entries get processed.
"""

__docformat__ = 'reStructuredText'

import sys
import os
import re
import time
from docutils import nodes, utils
from docutils import ApplicationError, DataError
from docutils.transforms import Transform, TransformError


class Headers(Transform):

    """
    Process fields in a PEP's initial RFC-2822 header.
    """

    pep_cvs_url = ('http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/python/'
                   'python/nondist/peps/pep-%04d.txt')
    rcs_keyword_substitutions = (
          (re.compile(r'\$' r'RCSfile: (.+),v \$$', re.IGNORECASE), r'\1'),
          (re.compile(r'\$[a-zA-Z]+: (.+) \$$'), r'\1'),)

    def transform(self):
        if not len(self.document):
            raise DataError('Document tree is empty.')
        header = self.document[0]
        if not isinstance(header, nodes.field_list) or \
              header.get('class') != 'rfc2822':
            raise DataError('Document does not begin with an RFC-2822 '
                            'header; it is not a PEP.')
        pep = title = None
        for field in header:
            if field[0].astext().lower() == 'pep': # should be the first field
                pep = int(field[1].astext())
                break
        if pep is None:
            raise DataError('Document does not contain an RFC-2822 "PEP" '
                            'header.')
        for field in header:
            name = field[0].astext().lower()
            body = field[1]
            if len(body) > 1:
                raise DataError('PEP header field body contains multiple '
                                'elements:\n%s' % field.pformat(level=1))
            elif len(body):
                if not isinstance(body[0], nodes.paragraph):
                    raise DataError('PEP header field body may only contain '
                                    'a single paragraph:\n%s'
                                    % field.pformat(level=1))
            elif name == 'last-modified':
                date = time.strftime(
                      '%d-%b-%Y',
                      time.localtime(os.stat(self.document['source'])[8]))
                body += nodes.paragraph()
                uri = self.pep_cvs_url % int(pep)
                body[0][:] = [nodes.reference('', date, refuri=uri)]
            else:
                continue
            para = body[0]
            if name == 'title':
                title = body.astext()
                # @@@ Insert a "pending" element here, since we don't really
                # want a separate document title?
            elif name in ('author', 'discussions-to'):
                for node in para:
                    if isinstance(node, nodes.reference) \
                          and node.has_key('refuri') \
                          and node['refuri'][:7] == 'mailto:':
                        node['refuri'] += '?subject=PEP%%20%s' % pep
            elif name in ('replaces', 'replaced-by'):
                newbody = []
                space = nodes.Text(' ')
                for refpep in body.astext().split():
                    pepno = int(refpep)
                    newbody.append(nodes.reference(
                          refpep, refpep, refuri='pep-%04d.html' % pepno))
                    newbody.append(space)
                para[:] = newbody[:-1] # drop trailing space
            elif name == 'last-modified':
                utils.clean_rcs_keywords(para, self.rcs_keyword_substitutions)
                date = para.astext()
                uri = self.pep_cvs_url % int(pep)
                para[:] = [nodes.reference('', date, refuri=uri)]
            elif name == 'version' and len(body):
                utils.clean_rcs_keywords(para, self.rcs_keyword_substitutions)
