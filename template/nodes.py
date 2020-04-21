#coding :utf8

class Node(object):
    
    def __init__(self, content='', children=None):
        self.content = content
        self.children = children or []
        self.attrs = []
        self.prepare()
    
    def prepare(self):
        pass

    def compile_with(self, codegen):
        return self

    def __str__(self):
        return "%s[%s]" % (self.__class__.__name__, self.content)
    

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
        codegen.write_line("''") # empty if
        for child in self.children:
            child.compile_with(codegen)
        return self


class Elif(Node):

    def compile_with(self, codegen):
        codegen.forward_indent()
        codegen.write_line("%s:" % self.content, False)
        codegen.backward_indent()
        codegen.write_line("''") # empty elif
        return self


class Else(Node):
    
    def compile_with(self, codegen):
        codegen.forward_indent()
        codegen.write_line("%s:" % self.content, False)
        codegen.backward_indent()
        codegen.write_line("''") # empty else
        return self


class For(Node):
    
    def compile_with(self, codegen):
        codegen.write_line("%s:" % self.content, False)
        codegen.backward_indent()
        codegen.write_line("''") # empty for

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
    
    def prepare(self):
        self.content = 'pass'


class Break(SpecialExpression):
    
    def prepare(self):
        self.content = 'break'


class Continue(SpecialExpression):
    
    def prepare(self):
        self.content = 'continue'


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


class NamedNode(Node):
    
    def prepare(self):
        vs = [ x for x in self.content.split() if x ]
        if len(vs) < 2:
            self.name = ''
        else:
            self.name = vs[1].replace('"', '').replace("'", '')
            self.attrs = vs[2:]


class Include(NamedNode):
    
    def compile_with(self, codegen):
        for attr in self.attrs:
            codegen.write_line("%s" % attr, False)
        return self

    
class Extends(NamedNode):
    pass


class Block(NamedNode):
    
    def compile_with(self, codegen):
        for child in self.children:
            child.compile_with(codegen)
        return self


class Using(Node):
    
    def prepare(self):
        """using funname(*args, **kwargs) do varname
        """
        self.func = ""
        self.var  = ""

        vs = [ x for x in self.content.split() if x ]
        if vs[0] != 'using' or vs[-2] != 'do':
            return

        self.func = " ".join(vs[1:-2])
        self.var  = vs[-1]

    def compile_with(self, codegen):
        codegen.write_line("%s = %s" % (self.var, self.func), False)
        codegen.write_line("str(%s.begin_render())" % self.var)
        for child in self.children:
            child.compile_with(codegen)
        codegen.write_line("str(%s.end_render())" % self.var)
        return self


class EndUsing(Node):

    pass
