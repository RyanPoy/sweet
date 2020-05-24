#coding: utf8
from sanic.views import HTTPMethodView
from sanic.response import text, json
from sanic.exceptions import InvalidUsage, NotFound
from hashlib import md5
from models import User


class Dispatcher(HTTPMethodView):

    def dispatch_request(self, request, *args, **kwargs):
        uri = str(request.raw_url, encoding="utf-8")
        controller_class, action_name, kwargs = route.match(url)
        c = controller_class(request)
        self.request = request

        self.uri = 
        self.prepare()
        self.before()
        return handler(*args, **kwargs)


