#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import re


class ActiveRoute(object):
    """ rule format like the following:
    <name> <=> <name:str>
    <name:int>
    <name:float>
    <name:list>
    <name:regex>
    """
    TYPES = {
        None:     '\w+',
        ':':      '\w+',
        ':str':   '\w+',
        ':int':   '\d+',
        ':float': '\d+(\.\d+)?',
        ':list':  '\w+(,\w+)*?',
    }

    def __init__(self, rule, controller, function, method='GET'):
        self.src_rule   = rule
        # self.rule       = self._parse_rule(rule)
        self.controller = controller
        self.function   = function
        self.method     = method

    def _purify_rule(self):
        rule = self.src_rule

        if not rule:
            return rule

        if rule == '/':
            return '^/$'

        if rule.endswith('[/]'):
            return '^%s?$' % rule
        if rule.endswith('[/]?'):
            return '^%s$' % rule

        if rule[-1] == '/':
            return '^%s[/]?$' % rule[:-1]
        if rule[-1] != '/':
            return '^%s[/]?$' % rule

    @property
    def rule(self):
        if not hasattr(self, '_rule'):
            self._rule = self._parse_rule()
        return self._rule

    def _parse_rule(self):
        rule, new_rule = self._purify_rule(), []
        p = re.compile(r'<([a-zA-Z_][a-zA-Z_0-9]*)(:[^>]*)?>')
        offset = 0
        for match in p.finditer(rule):
            new_rule.append(rule[offset:match.start()])

            name, _type = match.groups()
            _type = self.TYPES.get(_type, None) or _type[1:]
            
            pattern = '(?P<%s>%s)' % (name, _type)
            new_rule.append(pattern)
            offset = match.end()
        new_rule.append(rule[offset:])
        return ''.join(new_rule)

    def match(self, url):
        m = re.match(self.rule, url)
        return m.groupdict() if m else False
