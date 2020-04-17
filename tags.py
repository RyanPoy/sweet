#coding: utf8
from collections import OrderedDict as oDict
from datetime import datetime, date


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
        d = oDict()
        if _id: d['id'] = _id
        d['name'] = name or "button"
        d['type'] = tp or 'submit'
        if disabled: d['disabled'] = 'disabled'

        d.update(html)
        return '<button %s>%s</button>' % \
                    (' '.join([ '%s="%s"' % (k, v) for k, v in d.items() ]), 
                        value or "Button"
                    )

    def checkbox(self, name="checkbox", value="1", _id="", checked=False, disabled=False, html={}):
        d = oDict()
        name = name or "checkbox"
        d['id'] = _id or name
        d['name'] = name
        d['type'] = "checkbox"
        d['value'] = value
        if checked:
            d['checked'] = "checked"
        if disabled:
            d['disabled'] = "disabled"

        d.update(html)
        return '<input %s />' % ' '.join([ '%s="%s"' % (k, v) for k, v in d.items() ])

    def text_field(self, name, value='', _id='', tp='text', disabled=False, _class='', html={}):
        d = oDict()
        d['id'] = _id or name
        d['name'] = name
        d['type'] = tp or 'text'
        if value:
            d['value'] = value
        if disabled:
            d['disabled'] = "disabled"
        if _class:
            d['class'] = _class
        d.update(html)

        return '<input %s />' % ' '.join([ '%s="%s"' % (k, v) for k, v in d.items() ])

    def color_field(self, name, value='', _id='', disabled=False, _class='', html={}):
        return self.text_field(name=name, value=value, _id=_id, tp="color", disabled=disabled, _class=_class, html=html)

    def date_field(self, name, value='', _id='', disabled=False, _class='', html={}):
        return self.text_field(name=name, value=value, _id=_id, tp="date", disabled=disabled, _class=_class, html=html)

    def datetime_field(self, name, value='', _min='', _max='', _id='', disabled=False, _class='', html={}):
        def _(v):
            if isinstance(v, datetime):
                return datetime.strftime(v, '%Y-%m-%dT%H:%M:%S')
            elif isinstance(v, date):
                return date.strftime(v, '%Y-%m-%dT00:00:00')

        if value: value = _(value)
        if _min and 'min' not in html: html['min'] = _(_min)
        if _max and 'max' not in html: html['max'] = _(_max)

        return self.text_field(name=name, value=value, _id=_id, tp="datetime-local", disabled=disabled, _class=_class, html=html)

    # def password(self, name):
    #     return '<input name="%s" type="password" value="" />' % name

    # def textarea(self, name):
    #     return '<textarea name="%s"></textarea>' % name