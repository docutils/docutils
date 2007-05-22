# -*- coding: utf-8 -*-
"""
    Sphinx - Python documentation webserver
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: 2007 by Armin Ronacher.
    :license: Python license.
"""
import sys
from wsgiref.simple_server import make_server
from sphinx.web.application import make_app

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print 'usage: %s <doc_root>' % sys.argv[0]
        sys.exit(-1)
    app = make_app({'data_root_path': sys.argv[1]})

    #XXX: make this configurable
    try:
        from werkzeug.debug import DebuggedApplication
    except ImportError:
        pass
    else:
        app = DebuggedApplication(app, True)

    srv = make_server('localhost', 3000, app)
    try:
        print 'Running on http://%s:%d/' % srv.socket.getsockname()
        srv.serve_forever()
    except KeyboardInterrupt:
        pass
