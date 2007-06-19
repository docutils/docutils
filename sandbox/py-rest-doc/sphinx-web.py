# -*- coding: utf-8 -*-
"""
    Sphinx - Python documentation webserver
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: 2007 by Armin Ronacher.
    :license: Python license.
"""
import sys
import signal
import getopt
from wsgiref.simple_server import make_server

from sphinx.web.application import make_app

try:
    from werkzeug.debug import DebuggedApplication
except ImportError:
    DebuggedApplication = lambda x: x

class Restart(Exception):
    pass

def raise_restart(*args):
    raise Restart

signal.signal(signal.SIGUSR1, raise_restart)

def check_superuser(orig_app):
    """Check if there is a superuser and create one if necessary."""
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
            orig_app.userdb.privileges['admin'].add('master')
            orig_app.userdb.save()

def main(argv):
    opts, args = getopt.getopt(argv[1:], "dh")
    opts = dict(opts)
    if not args or '-h' in opts:
        print 'usage: %s [-d] <doc_root> [<hostname> [<port>]]' % argv[0]
        print ' -d: use werkzeug debugger if installed'
        return 2

    port = 3000
    hostname = 'localhost'
    if len(args) > 1:
        hostname = args[1]
        if len(args) > 2:
            port = int(args[2])
 
    debug = '-d' in opts

    while True:
        orig_app, app = make_app({
            'data_root_path':   args[0],
            'debug':            debug
        })
        check_superuser(orig_app)

        if debug:
            app = DebuggedApplication(app, True)

        srv = make_server(hostname, port, app)
        try:
            print 'Running on http://%s:%d/' % srv.socket.getsockname()
            srv.serve_forever()
        except Restart:
            print 'Got SIGUSR1, restarting...'
            srv.server_close()
            continue
        except KeyboardInterrupt:
            break


if __name__ == '__main__':
    sys.exit(main(sys.argv))
