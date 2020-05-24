#coding: utf8
from sanic.views import HTTPMethodView
from sanic.response import text, json
from sanic.exceptions import InvalidUsage, NotFound
from hashlib import md5
from models import User


class Controller(HTTPMethodView):

    def dispatch_request(self, request, *args, **kwargs):
        handler = getattr(self, request.method.lower(), None)
        self.request = request
        self.uri = str(request.raw_url, encoding="utf-8")
        self.prepare()
        self.before()
        return handler(*args, **kwargs)

    def before(self):
        pass

    def prepare(self):
        pass

    def err(self, code=-1, msg='', data={}):
        return self.render_json(code=code, msg=msg, data=data)

    def end(self, code=0, msg='', data={}):
        from pprint import pprint as pp
        return self.render_json(code=code, msg=msg, data=data)

    def render_str(self, s):
        return text(s)

    def render_json(self, **d):
        return json(d)

    def int_arg(self, name, default=0):
        v = self.arg(name)
        try:
            return int(v)
        except:
            return default

    @property
    def page_arg(self):
        page = self.arg('page', 1)
        try:
            page = int(page)
        except ValueError:
            page = 1
        finally:
            if page < 0:
                page = 1
        return page

    def arg(self, name, default=None, trim=False):
        v, found = None, False

        if self.request.form and name in self.request.form:
            v = self.request.form.get(name, default)
            found = True
        else:
            try:
                if self.request.json and name in self.request.json:
                    v = self.request.json.get(name, default)
                    found = True
            except InvalidUsage:
                pass

        if not found and self.request.args and name in self.request.args:
            v = self.request.args.get(name, default)
            found = True

        if found:
            if trim:
                return v.strip()
            else:
                return v
        return default

    def json_arg(self, name, default=None):
        return self.request.json.get(name, default)

    # def build_sessionid(self, user_id):
    #     s = 'yuyi-%s-yiyu' % user_id
    #     return '%s%s' % (md5(s.encode('UTF8')).hexdigest(), user_id)

    # @cacheproperty
    # def current_user(self):
    #     headers = self.request.headers

    #     sessionid = headers.get("SessionId", '') \
    #                 or headers.get("Sessionid", '') \
    #                 or headers.get("sessionid", '')
    #     if not sessionid or len(sessionid) <= 32:
    #         return None

    #     t, uid = sessionid[:32], sessionid[32:]
    #     t2 = self.build_sessionid(uid)
    #     return User.get_or_none(User.id==uid) if sessionid == t2 else None
