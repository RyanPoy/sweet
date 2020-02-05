#coding :utf8
class Node(object):
    
    def __init__(self, content, children=None):
        self.content = content
        self.children = children or []
        self.attrs = []

    def __str__(self):
        return "%s[%s]" % (self.__class__.__name__, self.content)
    
#     def create(self, name, start_tag_name=None, content, children=None):
#         if name == 'Text':
#             return Text(content, children)
#         elif name == ''


class Text(Node):
    pass


class Expression(Node):
    pass


class Comment(Node):
    pass


class If(Node):
    pass


class Elif(Node):
    pass


class Else(Node):
    pass


class For(Node):
    pass


class Break(Node):
    pass


class Continue(Node):
    pass


class End(Node):
    pass


class EndIf(End):
    pass


class EndFor(End):
    pass


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
