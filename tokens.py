#coding: utf8

class Token(object):
    
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

    def compile(self, writer):
        writer.write_line("%s:" % self.content, False)
        writer.backward_indent()


class EndExpressionToken(Token):

    def compile(self, writer):
        writer.forward_indent()
