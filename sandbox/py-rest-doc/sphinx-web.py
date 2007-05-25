# -*- coding: utf-8 -*-
"""
    Sphinx - Python documentation webserver
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: 2007 by Armin Ronacher.
    :license: Python license.
"""
import sys
import getopt
from wsgiref.simple_server import make_server

from sphinx.web.application import make_app

def main(argv):
    opts, args = getopt.getopt(argv[1:], "dh")
    opts = dict(opts)
    if len(args) != 1 or '-h' in opts:
        print 'usage: %s [-d] <doc_root>' % argv[0]
        print ' -d: use werkzeug debugger if installed'
        return 2
    orig_app, app = make_app({'data_root_path': args[0]})

    if '-d' in opts:
        try:
            from werkzeug.debug import DebuggedApplication
        except ImportError:
            pass
        else:
            app = DebuggedApplication(app, True)

    # check if there is a superuser
    if not orig_app.userdb.users:
        print 'Warning: you have no user database or no master "admin" account.'
        create = raw_input('Do you want to create an admin account now? [y/n] ')
        if not create or create.lower().startswith('y'):
            import getpass
            print 'Creating "admin" user.'
            pw1 = getpass.getpass('Enter password: ')
            pw2 = getpass.getpass('Enter password again: ')
            if pw1 != pw2:
                print 'Error: Passwords don\'t match.'
                sys.exit(1)
            orig_app.userdb.set_password('admin', pw1)
            orig_app.userdb.privileges['admin'] = set(['master'])
            orig_app.userdb.save()

    srv = make_server('localhost', 3000, app)
    try:
        print 'Running on http://%s:%d/' % srv.socket.getsockname()
        srv.serve_forever()
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    sys.exit(main(sys.argv))
