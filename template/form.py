#coding: utf8
from collections import OrderedDict as oDict
from datetime import datetime, date
import string
import re


label_value_p = re.compile(r'[-_\s]')
class Form(object):
    
    def __init__(self, action, model=None, method="GET", id_="", multipart=False, remote=False, charset="UTF8", html=None):
        self.action = action
        self.model = model
        self.method = method
        self.multipart = multipart
        self.remote = remote
        self.charset = (charset or "UTF8").upper()
        self.id = id_
        self.html = html or {}

    def begin_render(self):
        d = oDict()
        if self.id: d['id'] = self.id
        d['action'] = self.action
        d['method'] = self.method
        d['accept-charset'] = self.charset
        if self.method.upper() == 'POST' and self.multipart:
            d['enctype'] = "multipart/form-data"

        d.update(self.html)
        return '<form %s>' % ' '.join([ '%s="%s"' % (k, v) for k, v in d.items() ]) 

    def end_render(self):
        return '</form>'

    def button(self, value=None, name="button", tp="submit", disabled=False, html=None):
        html = html or {}
        d = oDict()
        d['name'] = name or "button"
        d['type'] = tp or 'submit'
        if disabled: d['disabled'] = 'disabled'

        d.update(html)
        return '<button %s>%s</button>' % \
                    (' '.join([ '%s="%s"' % (k, v) for k, v in d.items() ]), 
                        value or "Button"
                    )

    def text(self, name, value=None, id_='', tp='text', placeholder='', size='', maxlength='', disabled=False, class_='', html=None, autoid=True):
        html = html or {}
        d = oDict()
        id_ = self._buildid_(id_, autoid, name)
        if id_:
            d['id'] = id_
        d['name'] = self._build_name(name)

        d['type'] = tp or 'text'
        if value or value == '':
            d['value'] = value
        elif self.model:
            d['value'] = self._get_model_value(name)
        if placeholder:
            d['placeholder'] = placeholder
        if size or size == 0:
            d['size'] = size
        if maxlength or maxlength == 0:
            d['maxlength'] = maxlength

        if disabled:
            d['disabled'] = "disabled"
        if class_:
            d['class'] = class_
        d.update(html)

        return '<input %s />' % ' '.join([ '%s="%s"' % (k, v) for k, v in d.items() ])

    def password(self, name, value=None, id_='', placeholder='', size='', maxlength='', disabled=False, class_='', html=None):
        return self.text(name=name, value=value, id_=id_, placeholder=placeholder, tp="password", size=size, maxlength=maxlength, disabled=disabled, class_=class_, html=html)

    def tel(self, name, value=None, id_='', placeholder='', size='', maxlength='', disabled=False, class_='', html=None):
        return self.text(name=name, value=value, id_=id_, placeholder=placeholder, tp="tel", size=size, maxlength=maxlength, disabled=disabled, class_=class_, html=html)

    def search(self, name, value=None, id_='', placeholder='', size='', maxlength='', disabled=False, class_='', html=None):
        return self.text(name=name, value=value, id_=id_, placeholder=placeholder, tp="search", size=size, maxlength=maxlength, disabled=disabled, class_=class_, html=html)

    def email(self, name, value=None, id_='', placeholder='', disabled=False, class_='', html=None):
        return self.text(name=name, value=value, id_=id_, placeholder=placeholder, tp="email", disabled=disabled, class_=class_, html=html)

    def color(self, name, value=None, id_='', placeholder='', disabled=False, class_='', html=None):
        return self.text(name=name, value=value, id_=id_, placeholder=placeholder, tp="color", disabled=disabled, class_=class_, html=html)

    def url(self, name, value=None, id_='', placeholder='', disabled=False, class_='', html=None):
        return self.text(name=name, value=value, id_=id_, placeholder=placeholder, tp="url", disabled=disabled, class_=class_, html=html)

    def file(self, name, value=None, id_='', placeholder='', accept=None, multiple=False, disabled=False, class_='', html=None):
        html = html or {}
        if accept and 'accept' not in html:
            html['accept'] = accept
        return self.text(name=name, value=value, id_=id_, placeholder=placeholder, tp="file", disabled=disabled, class_=class_, html=html)

    def hidden(self, name, value=None, id_='', tp='text', disabled=False, class_='', html=None):
        return self.text(name=name, value=value, id_=id_, tp="hidden", disabled=disabled, class_=class_, html=html)

    def label(self, name, value='', class_='', html=None):
        html = html or {}
        d = oDict()
        if self.model:
            d['for'] = '%s_%s' % (self.model.name_for_view(), name)
        else:
            d['for'] = name
        if not value:
            value = ' '.join([
                string.capwords(x.strip()) for x in label_value_p.split(name) if x and x.strip()
            ])
        if class_:
            d['class'] = class_
        d.update(html)
        return '<label %s>%s</label>' % (
                    ' '.join([ '%s="%s"' % (k, v) for k, v in d.items() ]),
                    value
                )

    def number(self, name, value=None, id_='', placeholder='', tp='', _min='', _max='', step='', disabled=False, class_='', html=None):
        html = html or {}
        tp = tp or 'number'
        if _min and 'min' not in html: html['min'] = _min
        if _max and 'max' not in html: html['max'] = _max
        if step and 'step' not in html: html['step'] = step
        return self.text(name=name, value=value, id_=id_, placeholder=placeholder, tp=tp, disabled=disabled, class_=class_, html=html)

    def month(self, name, value=None, id_='', placeholder='', _min='', _max='', step='', disabled=False, class_='', html=None):
        return self.number(name=name, value=value, id_=id_, placeholder=placeholder, _min=_min, _max=_max, step=step, tp="month", disabled=disabled, class_=class_, html=html)

    def week(self, name, value=None, id_='', placeholder='', _min='', _max='', step='', disabled=False, class_='', html=None):
        return self.number(name=name, value=value, id_=id_, placeholder=placeholder, _min=_min, _max=_max, step=step, tp="week", disabled=disabled, class_=class_, html=html)

    def date(self, name, value=None, id_='', placeholder='', disabled=False, class_='', html=None):
        return self.text(name=name, value=value, id_=id_, placeholder=placeholder, tp="date", disabled=disabled, class_=class_, html=html)

    def time(self, name, value=None, id_='', placeholder='', _min='', _max='', step='', disabled=False, class_='', html=None):
        return self.number(name=name, value=value, id_=id_, placeholder=placeholder, _min=_min, _max=_max, step=step, tp="time", disabled=disabled, class_=class_, html=html)

    def datetime(self, name, value=None, placeholder='', _min='', _max='', id_='', step='', disabled=False, class_='', html=None):
        def _(v):
            if isinstance(v, datetime):
                return datetime.strftime(v, '%Y-%m-%dT%H:%M:%S')
            elif isinstance(v, date):
                return date.strftime(v, '%Y-%m-%dT00:00:00')

        html = html or {}
        if value is None and self.model:
            value = self._get_model_value(name)

        if value: value = _(value)
        if _min and 'min' not in html: html['min'] = _(_min)
        if _max and 'max' not in html: html['max'] = _(_max)
        if step and 'step' not in html: html['step'] = step

        return self.text(name=name, value=value, id_=id_, placeholder=placeholder, tp="datetime-local", disabled=disabled, class_=class_, html=html)

    def radio(self, name, value, id_='', checked=False, disabled=False, class_='', html=None):
        html = html or {}
        if not id_:
            if self.model is None:
                id_ = '%s_%s' % (name, value.replace(' ', '_'))
            else:
                id_ = '%s_%s_%s' % (self.model.name_for_view(), name, value)

        if disabled:
            html['disabled'] = 'disabled'
        elif checked is True and 'checked' not in html:
            html['checked'] = 'checked'
        elif self.model is not None and str(self._get_model_value(name)) == str(value):
            html['checked'] = 'checked'

        return self.text(name=name, value=value, id_=id_, tp="radio", disabled=disabled, class_=class_, html=html)

    def checkbox(self, name, value="1", id_="", checked=False, disabled=False, class_='', html=None):
        html = html or {}
        d = oDict()
        if self.model is None:
            name = name or "checkbox"
            if checked is True and 'checked' not in html: 
                d['checked'] = "checked"
            d.update(html)
        else:
            if str(self._get_model_value(name)) == str(value):
                d['checked'] = "checked"
            if not id_:
                id_ = '%s_%s_%s' % (self.model.name_for_view(), name, value)
            d.update(html)

        return self.text(name=name, value=value, id_=id_, tp="checkbox", disabled=disabled, class_=class_, html=d)

    def range(self, name, value=None, id_='', _in=None, step='', disabled=False, class_='', html=None):
        html = html or {}
        if _in and len(_in) >= 2:
            if 'min' not in html: html['min'] = _in[0]
            if 'max' not in html: html['max'] = _in[1]
        if step and 'step' not in html: html['step'] = step
        return self.text(name=name, value=value, id_=id_, tp="range", disabled=disabled, class_=class_, html=html)

    def submit(self, value='Save changes', disabled=False, class_='', html=None):
        html = html or {}
        if 'data-disable-with' not in html:
            html['data-disable-with'] = value
        return self.text(name='commit', value=value, tp="submit", disabled=disabled, class_=class_, html=html, autoid=False)

    def textarea(self, name, value=None, id_='', placeholder='', size=None, rows=None, cols=None, escape=True, disabled=False, class_='', html=None):
        html = html or {}
        d = oDict()
        id_ = self._buildid_(id_, False, name)
        d['id'] = id_ or name
        d['name'] = self._build_name(name)
        if not value:
            value = self._get_model_value(name) or ''

        if placeholder:
            d['placeholder'] = placeholder
        if size or size == 0:
            d['size'] = size
        if rows or rows == 0:
            d['rows'] = rows
        if cols or cols == 0:
            d['cols'] = cols
        if disabled:
            d['disabled'] = "disabled"
        if class_:
            d['class'] = class_
        d.update(html)

        return '<textarea %s>%s</textarea>' % (
                    ' '.join([ '%s="%s"' % (k, v) for k, v in d.items() ]),
                    value
                )

    def _build_name(self, name):
        if self.model:
            return "%s['%s']" % (self.model.name_for_view(), name)
        return name

    def _buildid_(self, id_, autoid, name):
        if id_:
            return id_
        if self.model:
            return '%s_%s' % (self.model.name_for_view(), name)
        if autoid:
            return name
        return ''

    def _get_model_value(self, name):
        if self.model:
            return getattr(self.model, name, '')
        return None

