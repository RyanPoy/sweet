# coding: utf8
import re
from writer import CodeWriter
from tokens import *


class ParseError(Exception):

    def __init__(self, message, filename=None, lineno=0):
        self.message = message
        self.filename = filename
        self.lineno = lineno
    
    def __str__(self):
        return '%s at %s' % (self.message, self.filename)
    

class Template(object):
    
    re_nodes = re.compile(r"(?s)(<%[=#]?.*?%>)")
            
    def __init__(self, content, fname=None):
        self.last_op, self.op_stack = '', []
        self.if_op_str_stack = 0
    
        self.content = content
        self.fname = fname or '<string>'
        self.tokens = self._parse()
        self.compiled = self._compile()
        self.pp()

    def pp(self):
        print ('\n ----- compiled -----\n')
        print (self.compiled)
        print ('\n --- end compiled ---\n')
        return self
    
    def _insert_op(self, op):
        self.last_op = op
        self.op_stack.append(op)
    
    def _pop_op(self):
        if self.op_stack:
            self.last_op = self.op_stack.pop()
            if isinstance(self.last_op, (ForExpressionToken, IfExpressionToken)):
                if self.if_op_stack:
                    self.if_op_str_stack -= 1
        else:
            self.last_op = ''
    
    def _parse(self):
        special_ops = set(['continue', 'pass', 'break'])
        nodes = []
        for t in self.re_nodes.split(self.content):
            if t.startswith('<%='):
                nodes.append(ExpressionToken(t[3:-2]))
            elif t.startswith('<%#'):
                nodes.append(CommentToken(t[3:-2]))
            elif t.startswith('<%'):
                e = t[2:-2].strip()
                op = e.split(' ', 1)[0]  
                if op == 'if':
                    self._insert_op(t)
                    self.if_op_str_stack += 1
                    nodes.append(IfExpressionToken(e))
                elif op == 'elif':
                    if not self.if_op_str_stack:
                        raise ParseError("Missing <%% if %%> block before %s block" % t, self.fname)
                    nodes.append(ElifExpressionToken(e))
                elif op == 'else':
                    if not self.if_op_str_stack:
                        raise ParseError("Missing <%% if %%> block before %s block" % t, self.fname)
                    nodes.append(ElseExpressionToken(e))
                elif op == 'for':
                    self._insert_op(t)
                    nodes.append(ForExpressionToken(e))
                elif op == 'end':
                    if not self.op_stack:
                        raise ParseError("Missing <%% if %%> block or <%% for %%> block before %s block" % t, self.fname)
                    self._pop_op()
                    nodes.append(EndExpressionToken(e))
                elif op in special_ops:
                    nodes.append(SpecialExpressionToken(e))
                else:
                    nodes.append(ExpressionToken(e))
            else:
                nodes.append(TextToken(t))
                
        if self.op_stack:
            self._pop_op()
            raise ParseError("Missing <%% end %%> block for %s block" % self.last_op, self.fname)

        return nodes

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
