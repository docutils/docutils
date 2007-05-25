# -*- coding: utf-8 -*-
"""
    sphinx.web.admin
    ~~~~~~~~~~~~~~~~

    Admin application parts.

    :copyright: 2007 by Georg Brandl, Armin Ronacher.
    :license: Python license.
"""

from .util import Response, RedirectResponse, render_template
from .database import Comment


def get_admin_page(req, page, userdb, cache):
    """
    Get some administration pages.
    """
    is_logged_in = req.user is not None
    if is_logged_in:
        privileges = userdb.privileges[req.user]
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
            if userdb.check_password(username, password):
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
                userdb.set_password(req.user, pw)
                userdb.save()
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
            elif req.form.get('confirmed'):
                for comment_id in to_delete:
                    try:
                        Comment.get(comment_id).delete()
                    except ValueError:
                        pass
                return RedirectResponse('admin/' + page)
            elif req.form.get('aborted'):
                return RedirectResponse('admin/' + page)
            elif req.form.get('edit') and not to_delete:
                if 'delete_this' in req.form:
                    try:
                        to_delete.add(req.form['delete_this'])
                    except ValueError:
                        pass
                else:
                    try:
                        edit_detail = c = Comment.get(int(req.args['edit']))
                    except ValueError:
                        pass
                    else:
                        if req.form.get('view'):
                            return RedirectResponse(c.url)
                        c.author = req.form.get('author', '')
                        c.author_mail = req.form.get('author_mail', '')
                        c.title = req.form.get('title', '')
                        c.comment_body = req.form.get('comment_body', '')
                        c.save()
                        cache.pop(edit_detail.associated_page, None)
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
        users = sorted((user, []) for user in userdb.users)
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
                    if username in userdb.users:
                        user_exists = username
                    else:
                        generated_password = userdb.add_user(username)
                        userdb.save()
                        generated_user = username
                else:
                    add_user_mode = True
            elif req.form.get('aborted'):
                return RedirectResponse('admin/manage_users/')

        users = {}
        for user in userdb.users:
            if not user in user_privileges:
                users[user] = sorted(userdb.privileges[user])
            else:
                users[user] = user_privileges[user]

        new_users = users.copy()
        for user in to_delete:
            new_users.pop(user, None)

        self_destruction = not req.user in new_users or \
                           'master' not in new_users[req.user]

        if req.method == 'POST' and (not to_delete or
           (to_delete and req.form.get('confirmed'))) and \
           req.form.get('update'):
            old_users = userdb.users.copy()
            for user in old_users:
                if user not in new_users:
                    del userdb.users[user]
                else:
                    userdb.privileges[user].clear()
                    userdb.privileges[user].update(new_users[user])
            userdb.save()
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
        return RedirectResponse('admin/')
