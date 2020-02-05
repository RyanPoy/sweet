#coding :utf8

class Node(object):
    
    def __init__(self, content, children=None):
        self.content = content
        self.children = children or []
        self.attrs = []
    
    def compile_with(self, codegen):
        return self

    def __str__(self):
        return "%s[%s]" % (self.__class__.__name__, self.content)
    
#     def create(self, name, start_tag_name=None, content, children=None):
#         if name == 'Text':
#             return Text(content, children)
#         elif name == ''


class Text(Node):
    
    def compile_with(self, codegen):
        codegen.write_line("'''%s'''" % self.content)
        return self


class Expression(Node):

    def compile_with(self, codegen):
        codegen.write_line("str(%s)" % self.content)
        return self


class Comment(Node):
    pass


class If(Node):
    
    def compile_with(self, codegen):
        codegen.write_line("%s:" % self.content, False)
        codegen.backward_indent()
        for child in self.children:
            child.compile_with(codegen)
        return self


class Elif(Node):

    def compile_with(self, codegen):
        codegen.forward_indent()
        codegen.write_line("%s:" % self.content, False)
        codegen.backward_indent()
        return self

class Else(Node):
    
    def compile_with(self, codegen):
        codegen.forward_indent()
        codegen.write_line("%s:" % self.content, False)
        codegen.backward_indent()
        return self


class For(Node):
    
    def compile_with(self, codegen):
        codegen.write_line("%s:" % self.content, False)
        codegen.backward_indent()
        for child in self.children:
            child.compile_with(codegen)
        return self


class SpecialExpression(Node):
    """ continue
        pass
        break
    """
    def compile_with(self, codegen):
        codegen.write_line("%s" % self.content, False)
        return self

class Pass(SpecialExpression):
    pass

class Break(SpecialExpression):
    pass

class Continue(SpecialExpression):
    pass


class End(Node):
    pass


class EndIf(End):
    
    def compile_with(self, codegen):
        codegen.forward_indent()
        return self


class EndFor(End):
    
    def compile_with(self, codegen):
        codegen.forward_indent()
        return self


class EndBlock(End):
    pass


class Include(Node):
    
    def __init__(self, content, children=None):
        super().__init__(content, children)
        vs = [ x for x in content.split() if x ]
        if len(vs) < 2:
            self.template_name = ''
        else:
            self.template_name = vs[1]
            self.attrs = vs[2:] 


class Extends(Node):
    
    def __init__(self, content, children=None):
        super().__init__(content, children)
        vs = [ x for x in content.split() if x ]
        if len(vs) < 2:
            self.template_name = ''
        else:
            self.template_name = vs[1]


class Block(Node):
    pass
