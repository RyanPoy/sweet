# coding: utf8
import re
from writer import CodeWriter
from tokens import *


class Template(object):
    
    def __init__(self, content):
        self.content = content
        self.tokens = self._parse()
        self.compiled = self._compile()
        self.pp()

    def pp(self):
        print ('\n ----- compiled -----\n')
        print (self.compiled)
        print ('\n --- end compiled ---\n')
        return self
    
    # re_nodes = re.compile(r"(?s)({{.*?}}|{%.*?%}|{#.*?#})")
#     re_nodes = re.compile(r"(?s)(<%=.*?%>|<%.*?%>|<%#.*?%>)")
    re_nodes = re.compile(r"(?s)(<%[=#]?.*?%>)")
    def _parse(self):
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
                    nodes.append(IfExpressionToken(e))
                elif op == 'elif':
                    nodes.append(ElifExpressionToken(e))
                elif op == 'else':
                    nodes.append(ElseExpressionToken(e))
                elif op in ('end'):
                    nodes.append(EndExpressionToken(e))
                else:
                    nodes.append(ExpressionToken(e))
            else:
                nodes.append(TextToken(t))
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
