# -*- coding: utf-8 -*-
"""
    sphinx.web.application
    ~~~~~~~~~~~~~~~~~~~~~~

    A simple WSGI application that serves an interactive version
    of the python documentation.

    :copyright: 2007 by Georg Brandl, Armin Ronacher.
    :license: Python license.
"""
from __future__ import with_statement

import re
import cPickle as pickle
from os import path

from .feed import Feed
from .antispam import AntiSpam
from .database import connect, set_connection, Comment
from .userdb import UserDatabase
from .util import Request, Response, RedirectResponse, SharedDataMiddleware, \
     NotFound, jinja_env
from ..search import SearchFrontend
from ..util import relative_uri, shorten_result


special_urls = set(['index', 'genindex', 'modindex'])

_mail_re = re.compile(r'^([a-zA-Z0-9_\.\-])+\@'
                      r'(([a-zA-Z0-9\-])+\.)+([a-zA-Z0-9]{2,})+$')


def get_target_uri(source_filename):
    if source_filename == 'index.rst':
        return ''
    if source_filename.endswith('/index.rst'):
        return source_filename[:-9] # up to /
    return source_filename[:-4] + '/'


def render_template(req, template_name, context=None):
    context = context or {}
    tmpl = jinja_env.get_template(template_name)

    def relative_path_to(otheruri, resource=False):
        if not resource:
            otheruri = get_target_uri(otheruri)
        return relative_uri(req.path, otheruri)
    context['pathto'] = relative_path_to

    # add it here a second time for templates that don't
    # get the builder information from the environment (such as search)
    context['builder'] = 'web'
    context['req'] = req

    return tmpl.render(context)


class DocumentationApplication(object):
    """
    Serves the documentation.
    """

    def __init__(self, conf):
        self.cache = {}
        self.data_root = conf['data_root_path']
        with file(path.join(self.data_root, 'environment.pickle')) as f:
            self.env = pickle.load(f)
        with file(path.join(self.data_root, 'searchindex.pickle')) as f:
            self.search_frontend = SearchFrontend(pickle.load(f))
        self.db_con = connect(path.join(self.data_root, 'sphinx.db'))
        self.antispam = AntiSpam(path.join(self.data_root, 'bad_content'))
        self.userdb = UserDatabase(path.join(self.data_root, 'docusers'))

    def search(self, req):
        """
        Search the database. Currently just a keyword based search
        """
        if not req.args.get('q'):
            return RedirectResponse('')
        return RedirectResponse('q/%s/' % req.args['q'])

    def show_source(self, req, page):
        """
        Show the highlighted source for a given page.
        """
        source_name = path.join(self.data_root, 'sources', page + '.txt')
        if not path.exists(source_name):
            return self.get_keyword_matches(req)
        with file(source_name) as f:
            return Response(f.read(), mimetype='text/plain')

    def get_page(self, req, url):
        """
        Show the requested documentation page or raise an
        `NotFound` exception to display a page with close matches.
        """
        page_id = url + '.rst'
        cache_possible = True
        comments_enabled = self.env.metadata.get(page_id, {}) \
                               .get('comments_enabled', True)

        # generate feed if wanted
        if req.args.get('feed') == 'comments':
            feed = Feed(req, 'Comments for %s' % url, 'List of comments for '
                        'the topic %s' % url, url)
            for comment in Comment.get_for_page(page_id):
                feed.add_item(comment.title, comment.author, comment.url,
                              comment.parsed_comment_body, comment.pub_date)
            return Response(feed.generate(), mimetype='application/rss+xml')

        # do the form validation and comment saving if the
        # request method is post.
        title = comment_body = ''
        author = req.session.get('author', '')
        author_mail = req.session.get('author_mail', '')
        form_error = None
        preview = None

        if comments_enabled and req.method == 'POST':
            title = req.form.get('title', '').strip()
            author = req.form.get('author', '').strip()
            author_mail = req.form.get('author_mail', '')
            comment_body = req.form.get('comment_body', '')
            fields = (title, author, author_mail, comment_body)

            if req.form.get('preview'):
                preview = Comment(page_id, title, author, author_mail,
                                  comment_body)
            elif req.form.get('homepage') or self.antispam.is_spam(fields):
                form_error = 'Your text contains blocked URLs or words.'
            else:
                if not all(fields):
                    form_error = 'You have to fill out all fields.'
                elif _mail_re.search(author_mail) is None:
                    form_error = 'You have to provide a valid mail address.'
                else:
                    self.cache.pop(page_id, None)
                    comment = Comment(page_id, title, author, author_mail,
                                      comment_body)
                    comment.save()
                    req.session.update(
                        author=author,
                        author_mail=author_mail
                    )
                    return RedirectResponse(comment.url)
            cache_possible = False

        # if the form validation fails the cache is used so that
        # we can put error messages and defaults to the page.
        if cache_possible:
            try:
                filename, mtime, text = self.cache[page_id]
            except KeyError:
                pass
            else:
                if path.getmtime(filename) == mtime:
                    return Response(text)

        # render special templates such as the index
        if url in special_urls:
            filename = path.join(self.data_root, 'specials.pickle')
            with open(filename, 'rb') as f:
                context = pickle.load(f)
            templatename = url + '.html'

        # render the page based on the settings in the pickle
        else:
            for filename in [path.join(self.data_root, url) + '.fpickle',
                             path.join(self.data_root, url, 'index.fpickle')]:
                if not path.exists(filename):
                    continue
                with open(filename, 'rb') as f:
                    context = pickle.load(f)
                    break
            else:
                raise NotFound()
            templatename = 'page.html'

        context.update(
            comments_enabled=comments_enabled,
            comments=Comment.get_for_page(page_id),
            preview=preview,
            form={
                'title':            title,
                'author':           author,
                'author_mail':      author_mail,
                'comment_body':     comment_body,
                'error':            form_error
            }
        )
        text = render_template(req, templatename, context)

        if cache_possible:
            self.cache[page_id] = (filename, path.getmtime(filename), text)
        return Response(text)

    def get_recent_comments_feed(self, req):
        """
        Get the feed of recent comments.
        """
        feed = Feed(req, 'Recent Comments', 'Recent Comments', '')
        for comment in Comment.get_recent():
            feed.add_item(comment.title, comment.author, comment.url,
                          comment.parsed_comment_body, comment.pub_date)
        return Response(feed.generate(), mimetype='application/rss+xml')

    def get_admin_page(self, req, page):
        """
        Get some administration pages.
        """
        is_logged_in = req.user is not None
        if is_logged_in:
            privileges = self.userdb.privileges[req.user]
            is_master_admin = 'master' in privileges
            can_change_password = 'frozenpassword' not in privileges
        else:
            privileges = set()
            can_change_password = is_master_admin = False

        if page == 'login':
            if req.user is not None:
                return RedirectResponse('admin/')
            login_failed = False
            if req.method == 'POST':
                if req.form.get('cancel'):
                    return RedirectResponse('')
                username = req.form.get('username')
                password = req.form.get('password')
                if self.userdb.check_password(username, password):
                    req.login(username)
                    return RedirectResponse('admin/')
                login_failed = True
            return Response(render_template(req, 'admin/login.html', {
                'login_failed': login_failed
            }))
        elif not is_logged_in:
            return RedirectResponse('admin/login/')
        elif page == 'logout':
            req.logout()
            return RedirectResponse('admin/')
        elif page == 'change_password' and can_change_password:
            change_failed = change_successful = False
            if req.method == 'POST':
                if req.form.get('cancel'):
                    return RedirectResponse('admin/')
                pw = req.form.get('pw1')
                if pw and pw == req.form.get('pw2'):
                    self.userdb.set_password(req.user, pw)
                    self.userdb.save()
                    change_successful = True
                else:
                    change_failed = True
            return Response(render_template(req, 'admin/change_password.html', {
                'change_failed':        change_failed,
                'change_successful':    change_successful
            }))
        elif page.split('/')[0] == 'moderate_comments':
            details_for = page[18:] + '.rst' or None
            to_delete = set()
            edit_detail = None

            if 'edit' in req.args:
                try:
                    edit_detail = Comment.get(int(req.args['edit']))
                except ValueError:
                    pass

            if req.method == 'POST':
                for item in req.form.getlist('delete'):
                    try:
                        to_delete.add(int(item))
                    except ValueError:
                        pass
                if req.form.get('cancel'):
                    return RedirectResponse('admin/')
                elif req.form.get('confirmated'):
                    for comment_id in to_delete:
                        try:
                            Comment.get(comment_id).delete()
                        except ValueError:
                            pass
                    return RedirectResponse('admin/' + page)
                elif req.form.get('aborted'):
                    return RedirectResponse('admin/' + page)
                elif req.form.get('edit') and not to_delete:
                    try:
                        edit_detail = Comment.get(int(req.args['edit']))
                    except ValueError:
                        pass
                    else:
                        edit_detail.author = req.form.get('author', '')
                        edit_detail.author_mail = req.form.get('author_mail', '')
                        edit_detail.title = req.form.get('title', '')
                        edit_detail.comment_body = req.form.get('comment_body', '')
                        edit_detail.save()
                        self.cache.pop(edit_detail.associated_page, None)
                    return RedirectResponse('admin/' + page)

            return Response(render_template(req, 'admin/moderate_comments.html', {
                'pages_with_comments': [{
                    'page_id':      page_id,
                    'title':        page_id,        #XXX: get title somehow
                    'has_details':  details_for == page_id,
                    'comments':     comments
                } for page_id, comments in Comment.get_overview(details_for)],
                'to_delete':        to_delete,
                'ask_confirmation': req.method == 'POST' and to_delete,
                'edit_detail':      edit_detail
            }))
        elif page == 'manage_users' and is_master_admin:
            add_user_mode = False
            user_privileges = {}
            users = sorted((user, []) for user in self.userdb.users)
            to_delete = set()
            generated_user = generated_password = None
            user_exists = False

            if req.method == 'POST':
                for item in req.form.getlist('delete'):
                    try:
                        to_delete.add(item)
                    except ValueError:
                        pass
                for name, item in req.form.iteritems():
                    if name.startswith('privileges-'):
                        user_privileges[name[11:]] = [x.strip() for x
                                                      in item.split(',')]
                if req.form.get('cancel'):
                    return RedirectResponse('admin/')
                elif req.form.get('add_user'):
                    username = req.form.get('username')
                    if username:
                        if username in self.userdb.users:
                            user_exists = username
                        else:
                            generated_password = self.userdb.add_user(username)
                            self.userdb.save()
                            generated_user = username
                    else:
                        add_user_mode = True
                elif req.form.get('aborted'):
                    return RedirectResponse('admin/manage_users/')

            users = {}
            for user in self.userdb.users:
                if not user in user_privileges:
                    users[user] = sorted(self.userdb.privileges[user])
                else:
                    users[user] = user_privileges[user]

            new_users = users.copy()
            for user in to_delete:
                new_users.pop(user, None)

            self_destruction = not req.user in new_users or \
                               'master' not in new_users[req.user]

            if req.method == 'POST' and (not to_delete or
               (to_delete and req.form.get('confirmated'))) and \
               req.form.get('update'):
                old_users = self.userdb.users.copy()
                for user in old_users:
                    if user not in new_users:
                        del self.userdb.users[user]
                    else:
                        self.userdb.privileges[user].clear()
                        self.userdb.privileges[user].update(new_users[user])
                self.userdb.save()
                return RedirectResponse('admin/manage_users/')

            return Response(render_template(req, 'admin/manage_users.html', {
                'users':                users,
                'add_user_mode':        add_user_mode,
                'to_delete':            to_delete,
                'ask_confirmation':     req.method == 'POST' and to_delete \
                                        and not self_destruction,
                'generated_user':       generated_user,
                'generated_password':   generated_password,
                'self_destruction':     self_destruction,
                'user_exists':          user_exists
            }))
        elif page == '':
            return Response(render_template(req, 'admin/index.html', {
                'is_master_admin':      is_master_admin,
                'can_change_password':  can_change_password
            }))
        else:
            raise RedirectResponse('admin/')

    pretty_type = {
        'data': 'module data',
        'cfunction': 'C function',
        'cmember': 'C member',
        'cmacro': 'C macro',
        'ctype': 'C type',
        'cvar': 'C variable',
    }

    def get_keyword_matches(self, req, term=None, avoid_fuzzy=False):
        """
        Find keyword matches. If there is an exact match, just redirect:
        http://docs.python.org/os.path.exists would automatically
        redirect to http://docs.python.org/modules/os.path/#os.path.exists.
        Else, show a page with close matches.

        Module references are processed first so that "os.path" is handled as
        a module and not as member of os.
        """
        if term is None:
            term = req.path.strip('/')

        matches = self.env.find_keyword(term, avoid_fuzzy)
        if not matches:
            return
        if isinstance(matches, tuple):
            return RedirectResponse(get_target_uri(matches[1]) + '#' + matches[2])
        else:
            # get some close matches
            close_matches = []
            good_matches = 0
            for ratio, type, filename, anchorname, desc in matches:
                link = get_target_uri(filename) + '#' + anchorname
                good_match = ratio > 0.75
                good_matches += good_match
                close_matches.append({
                    'href':         relative_uri(req.path, link),
                    'title':        anchorname,
                    'good_match':   good_match,
                    'type':         self.pretty_type.get(type, type),
                    'description':  desc,
                })
            return Response(render_template(req, 'not_found.html', {
                'close_matches':        close_matches,
                'good_matches_count':   good_matches,
                'keyword':              term
            }), status=404)

    def __call__(self, environ, start_response):
        """
        Dispatch requests.
        """
        set_connection(self.db_con)
        req = Request(environ)
        if not req.path.endswith('/') and req.method == 'GET':
            query = req.environ.get('QUERY_STRING', '')
            if query:
                query = '?' + query
            resp = RedirectResponse(req.path + '/' + query)
        elif req.path.startswith('/source/'):
            sourcename = req.path[8:].strip('/')
            resp = self.show_source(req, sourcename)
        else:
            url = req.path.strip('/') or 'index'
            if url == 'search':
                resp = self.search(req)
            elif url == 'index' and 'q' in req.args:
                resp = RedirectResponse('q/%s/' % req.args['q'])
            elif url == 'index' and req.args.get('feed') == 'recent_comments':
                resp = self.get_recent_comments_feed(req)
            elif url.startswith('q/'):
                resp = self.get_keyword_matches(req, url[2:])
            elif url == 'admin' or url.startswith('admin/'):
                resp = self.get_admin_page(req, url[6:])
            else:
                try:
                    resp = self.get_page(req, url)
                except NotFound:
                    resp = self.get_keyword_matches(req)
        return resp(environ, start_response)


def make_app(conf):
    """
    Create the WSGI application based on a configuration dict.
    Handled configuration values so far:

    `data_root_path`
        the folder containing the documentation data as generated
        by sphinx with the web builder.
    """
    app = DocumentationApplication(conf)
    app = SharedDataMiddleware(app, {
        '/style':   path.join(conf['data_root_path'], 'style')
    })
    return app
