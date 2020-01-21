#coding: utf8

class Token(object):
    
    MUST_END = False
    HAS_SIBLING = False
    
    def __init__(self, content):
        self.content = content
        self.tp = self.__class__.__name__
#         print (self)

    def compile(self, writer):
        pass
    
    def __str__(self):
        return 'Node[type=%s; name=%s]' % (self.tp, self.content) 


class TextToken(Token):
    """ Text Token
    """
    def compile(self, writer):
        writer.write_line("'''%s'''" % self.content)
        

class CommentToken(Token):
    pass


class ExpressionToken(Token):
    
    def compile(self, writer):
        writer.write_line("str(%s)" % self.content)
        

class SpecialExpressionToken(Token):
    """ continue
        pass
        break
    """
    def compile(self, writer):
        writer.write_line("%s" % self.content, False)


class IfExpressionToken(Token):
    
    MUST_END = True
    HAS_SIBLING = True # if =>  elif, else
    
    def compile(self, writer):
        writer.write_line("%s:" % self.content, False)
        writer.backward_indent()


class ElifExpressionToken(Token):
    
    def compile(self, writer):
        writer.forward_indent()
        writer.write_line("%s:" % self.content, False)
        writer.backward_indent()

        
class ElseExpressionToken(Token):
    
    def compile(self, writer):
        writer.forward_indent()
        writer.write_line("%s:" % self.content, False)
        writer.backward_indent()


class ForExpressionToken(Token):
    
    MUST_END = True

    def compile(self, writer):
        writer.write_line("%s:" % self.content, False)
        writer.backward_indent()


class EndExpressionToken(Token):

    def compile(self, writer):
        writer.forward_indent()


class IncludeBlock(Token):
    
    def __init__(self, content):
        super().__init__(content)
        self.tmpl_path = ''
        self.params = []

        vs = [ c for c in content.split() if c and c.strip() ]
        l = len(vs)
        if l >= 2:
            self.tmpl_path = vs[1]
        if l > 2:
            self.params = vs[2:] 

    def compile(self, writer):
        for p in self.params:
            writer.write_line('%s' % p, False)


class ExtendsBlock(Token):
    
    def __init__(self, content):
        super().__init__(content)
        vs = [ c for c in content.split() if c and c.strip() ]
        self.tmpl_path = '' if len(vs) < 2 else vs[1] 
