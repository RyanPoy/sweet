#coding: utf8
from collections import OrderedDict as oDict


class Form(object):
    
    def __init__(self, url='', model=None, method="GET", _id="", multipart=False, remote=False, charset="UTF8", html={}):
        self.url = url
        self.model = model
        self.method = method
        self.multipart = multipart
        self.remote = remote
        self.charset = (charset or "UTF8").upper()
        self.id = _id
        self.html = html or {}

    def begin_render(self):
        d = oDict()
        if self.id: d['id'] = self.id
        d['action'] = self.url
        d['method'] = self.method
        d['accept-charset'] = self.charset
        if self.method.upper() == 'POST' and self.multipart:
            d['enctype'] = "multipart/form-data"

        d.update(self.html)
        return '<form %s>' % ' '.join([ '%s="%s"' % (k, v) for k, v in d.items() ]) 

    def end_render(self):
        return '</form>'

    def button(self, value="", name="button", _id="", tp="submit", disabled=False, html={}):
        value = value or "Button"
        tp = tp or 'submit'
        if self.url:
            return self._button_tag(value, name, _id, tp, disabled, html)
        return ""

    def _button_tag(self, value, name, _id, tp, disabled, html={}):
        d = oDict()
        if _id: d['id'] = _id
        d['name'] = name
        d['type'] = tp
        if disabled: d['disabled'] = 'disabled'

        d.update(html)
        return '<button %s>%s</button>' % (' '.join([ '%s="%s"' % (k, v) for k, v in d.items() ]), value)


    # def text(self, name):
    #     return '<input name="%s" type="text" value="" />' % name

    # def password(self, name):
    #     return '<input name="%s" type="password" value="" />' % name

    # def textarea(self, name):
    #     return '<textarea name="%s"></textarea>' % name