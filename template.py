# coding: utf8
import re
import os
from writer import CodeWriter
from tokens import *

normpath = os.path.normpath
dirname = os.path.dirname
abspath = os.path.abspath
joinpath = os.path.join


class ParseError(Exception):

    def __init__(self, message, filename=None, lineno=0):
        self.message = message
        self.filename = filename
        self.lineno = lineno
    
    def __str__(self):
        return '%s at %s' % (self.message, self.filename)
    

class TokenStack(object):
    
    def __init__(self):
        self.stack = []
        self._if_num = 0
        self._end_num = 0

    def push(self, token):
        self.stack.append(token)
        
        if token.MUST_END:
            self._end_num += 1

        if token.HAS_SIBLING:
            self._if_num += 1

    def pop(self):
        if not self.stack:
            return None
        token = self.stack.pop()
        
        if token.MUST_END:
            self._end_num -= 1

        if token.HAS_SIBLING:
            self._if_num -= 1

        return token
    
    def has_if(self):
        return self._if_num > 0
    
    def has_end(self):
        return self._end_num > 0
        
    def empty(self):
        return len(self.stack) <= 0

    @property
    def last(self):
        return self.stack[-1]
    

class Template(object):
    
    re_nodes = re.compile(r"(?s)(<%[=#]?.*?%>)")

    def __init__(self, content, fname=None, loader=None):
        self.token_stack = TokenStack()
        self.content = content
        self.fname = fname or '<string>'
        self.loader = loader
        self.tokens = self._parse()
        self.compiled = self._compile()
        self.pp()

    @classmethod
    def from_file(cls, fname, loader=None):
        content = []
        with open(fname) as f:
            content = [ l for l in f ]
        return cls(''.join(content), fname, loader)

    def pp(self):
        print ('\n ----- compiled -----\n')
        print (self.compiled)
        print ('\n --- end compiled ---\n')
        return self
    
    def _parse(self):
        special_ops = set(['continue', 'pass', 'break'])
        tokens, tstack = [], self.token_stack
        for t in self.re_nodes.split(self.content):
#             t = t.lstrip('\n')
            if t.startswith('<%='):
                tokens.append(ExpressionToken(t[3:-2]))
            elif t.startswith('<%#'):
                tokens.append(CommentToken(t[3:-2]))
            elif t.startswith('<%'):
                e = t[2:-2].strip()
                op = e.split(' ', 1)[0]  
                if op == 'if':
                    if_token = IfExpressionToken(e)
                    tokens.append(if_token)
                    tstack.push(if_token)
                elif op == 'elif':
                    if not tstack.has_if():
                        raise ParseError("Missing <%% if %%> block before %s block" % t, self.fname)
                    tokens.append(ElifExpressionToken(e))
                elif op == 'else':
                    if not tstack.has_if():
                        raise ParseError("Missing <%% if %%> block before %s block" % t, self.fname)
                    tokens.append(ElseExpressionToken(e))
                elif op == 'for':
                    for_token = ForExpressionToken(e)
                    tokens.append(for_token)
                    tstack.push(for_token)
                elif op == 'end':
                    if not tstack.has_end(): 
                        raise ParseError("Missing <%% if %%> block or <%% for %%> block before %s block" % t, self.fname)
                    tstack.pop()
                    tokens.append(EndExpressionToken(e))
                elif op in special_ops:
                    tokens.append(SpecialExpressionToken(e))
                elif op == 'include': # should expand the template in include block
                    block = IncludeBlock(e)
                    if not block.tmpl_path:
                        raise ParseError("Missing include template name for %s block" % t, self.fname)
                    tokens.append(block)
                    try:
                        tmpl = self.loader.load(block.tmpl_path, self.fname)
                    except Exception as ex:
                        raise ParseError("Error: %s for %s block" % (ex, t), self.fname)
                    for n in tmpl.tokens:
                        tokens.append(n)
                elif op == 'extends':
                    block = ExtendsBlock(e)
                    tokens.append(block)
                    tmpl = self.loader.load(block.tmpl_path, self.fname)
                    for n in tmpl.tokens:
                        tokens.append(n)
                else:
                    tokens.append(ExpressionToken(e))
            else:
                tokens.append(TextToken(t))

        if not tstack.empty() or tstack.has_end():
            raise ParseError("Missing <%% end %%> block for <%% %s %%> block" % tstack.last.content, self.fname)

        return tokens

    def _compile(self):
        writer = CodeWriter()
        writer.begin()
        for t in self.tokens:
            t.compile(writer)
        writer.end()
        return str(writer)
        
    def generate(self, **kwargs):
        compile(self.compiled, '<string>', 'exec')
        exec(self.compiled, kwargs)
        _tt_exec = kwargs['_tt_exec']
        return _tt_exec()


class TemplateLoader(object):
    
    def __init__(self, root_abs_dir):
        self.root_dir = normpath(abspath(root_abs_dir))
        self.tmpl_dict = {}

    def build_path(self, tmpl_path, parent_path=None):
        if not parent_path:
            return normpath(joinpath(self.root_dir, tmpl_path))
        return normpath(joinpath(dirname(parent_path), tmpl_path))

    def load(self, tmpl_path, parent_path=None):
        abs_path = self.build_path(tmpl_path, parent_path)
        return Template.from_file(abs_path, loader=self)

    
if __name__ == '__main__':
    from cProfile import Profile
    n = 500000
    user = {
        'age': '露西',
        'age': 23
    }
    s = """<h1>{{ user.name{% }} %}</h1><h2>{{ user.age }}</h2>"""*n
    p = Profile()
    t = Template(s)
    p.run("t.generate(user=user)")
    p.print_stats()
