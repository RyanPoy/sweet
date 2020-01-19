# coding: utf8
import re
from writer import CodeWriter
from tokens import *


class ParseError(Exception):

    def __init__(self, message, filename=None, lineno=0):
        self.message = message
        # The names "filename" and "lineno" are chosen for consistency
        # with python SyntaxError.
        self.filename = filename
        self.lineno = lineno
    
    def __str__(self):
        return '%s at %s' % (self.message, self.filename)
    

class Template(object):
    
    last_op, last_op_expression = '', ''
    op_stack, op_expression_stack = [], [] 
#     re_nodes = re.compile(r"(?s)(<%=.*?%>|<%.*?%>|<%#.*?%>)")
    re_nodes = re.compile(r"(?s)(<%[=#]?.*?%>)")
            
    def __init__(self, content, fname=None):
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
    
    @classmethod
    def insert_op(cls, op, op_expression):
        cls.last_op = op
        cls.last_op_expression = op_expression
        
        cls.op_stack.append(op)
        cls.op_expression_stack.append(op_expression)
    
    @classmethod
    def pop_op(cls):
        cls.last_op = cls.op_stack.pop()
        cls.last_op_expression = cls.op_expression_stack.pop()
    
    def _parse(self):
        cls = self.__class__
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
                    cls.insert_op(op, t)
                    nodes.append(IfExpressionToken(e))
                elif op == 'elif':
                    nodes.append(ElifExpressionToken(e))
                elif op == 'else':
                    nodes.append(ElseExpressionToken(e))
                elif op == 'for':
                    cls.insert_op(op, t)
                    nodes.append(ForExpressionToken(e))
                elif op == 'end':
                    cls.pop_op()
                    nodes.append(EndExpressionToken(e))
                elif op in special_ops:
                    nodes.append(SpecialExpressionToken(e))
                else:
                    nodes.append(ExpressionToken(e))
            else:
                nodes.append(TextToken(t))
                
        if self.__class__.op_stack:
            print(self.op_stack)
            raise ParseError("Missing <%% end %%> block for %s block" % cls.last_op_expression, self.fname)

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
