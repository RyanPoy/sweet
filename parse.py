#coding: utf8
from nodes import Text, Expression, Comment, If, EndIf, Elif, Else, For, EndFor,\
    Include, Extends, EndBlock, Block, Continue, Break, Pass
from libs import StringReader


class ParseError(Exception):

    def __init__(self, message, filename=None, lineno=0):
        self.message = message
        self.filename = filename
        self.lineno = lineno
    
    def __str__(self):
#         return '%s at %s: %s' % (self.message, self.filename, self.lineno)
        return '%s' % (self.message)


class DeferBlock(object):
    def __init__(self, name):
        self.name = name


class Nodes(object):
    
    def __init__(self):
        self._data = []
        self._blocks = {}
        self.can_extends = True
        self.is_extends = False
        
    def append(self, node):
        if self.can_extends:
            if not isinstance(node, Text):
                self.can_extends = False
            elif node.content.strip():
                self.can_extends = False

        if self.is_extends:
            if isinstance(node, Block):
                if node.name not in self._blocks:
                    self._data.append(DeferBlock(node.name))
                self._blocks[node.name] = node
        else:
            self._data.append(node)
            
        return self
    
    @property
    def data(self):
        ns = []
        for n in self._data:
            if isinstance(n, DeferBlock):
                n = self._blocks[n.name]
            ns.append(n)
        return ns


def parse(reader, loader, parent_tag=''):
    nodes = Nodes()
    while True:
        p0 = reader.find('<%')
        if p0 < 0:  # not found begin tag
            if not reader.eof():  # end remaining text
                nodes.append(Text(reader.read()))
            break
        if p0 != 0:  # not begin
            nodes.append(Text(reader.read(p0)))  # text
            continue

        # process this line, the p0 must equals 0
        reader.skip(2)  # skip the <% 
        p1 = reader.find('%>')
        if p1 == -1:  # found the begin tag but not found end tag
            nodes.append(Text(reader.read()))  # @TODO: maybe should throw exception
            break

        content = reader.read(p1)
        reader.skip(2)  # skip the %>

        content = content.strip()
        if not content:
            continue
        
        if content[0] == '=':  # <%=  xxx %>
            nodes.append(Expression(content[1:].strip()))  # base expression
        elif content[0] == '#':  # <%# xxx %>
            nodes.append(Comment(content[1:].strip()))
        elif content.startswith('if'):  # <% if xxx %>
            children = parse(reader, loader, parent_tag='if').data
            if not children or not isinstance(children[-1], EndIf):
                raise ParseError("Missing '<%% end %%>' for '<%% %s %%>'" % content, reader.lineno)
            nodes.append(If(content, children))
        elif content.startswith('elif'):  # <% elif xxx %>
            if parent_tag != 'if':
                raise ParseError("Missing '<%% if %%>' before '<%% %s %%>'" % content, reader.lineno)
            nodes.append(Elif(content))
        elif content.startswith('else'):
            if parent_tag != 'if':
                raise ParseError("Missing '<%% if %%>' before '<%% %s %%>'" % content, reader.lineno)
            nodes.append(Else(content))
        elif content.startswith('for'):
            children = parse(reader, loader, parent_tag='for').data
            if not children or not isinstance(children[-1], EndFor):
                raise ParseError("Missing '<%% end %%>' for '<%% %s %%>'" % content, reader.lineno)
            nodes.append(For(content, children))
        elif content.startswith('continue'):
            nodes.append(Continue())
        elif content.startswith('break'):
            nodes.append(Break())
        elif content.startswith('pass'):
            nodes.append(Pass())
        elif content.startswith('block'):
            children = parse(reader, loader, parent_tag='block').data
            if not children or not isinstance(children[-1], EndBlock):
                raise ParseError("Missing '<%% end %%>' for '<%% %s %%>'" % content, reader.lineno)
            nodes.append(Block(content, children))
        elif content.startswith('end'):  # <% end %>
            if parent_tag == 'if':
                nodes.append(EndIf(content))
                break
            elif parent_tag == 'for':
                nodes.append(EndFor(content))
                break
            elif parent_tag == 'block':
                nodes.append(EndBlock(content))
                break
            else:
                raise ParseError("Missing '<% if|for|block %>' before '<% end %>'", reader.lineno)
        elif content.startswith('include'):  # <% include %>
            include = Include(content)
            if not include.template_name:
                raise ParseError("Missing template file path for '<%% %s %%>'" % content, reader.lineno)
            nodes.append(include)
#             if loader:
            t = loader.load(include.template_name).parse()
            for n in t.nodes:
                nodes.append(n)
        elif content.startswith('extends'): # <% extends %>
            if not nodes.can_extends:
                raise ParseError("'<%% %s %%>' must begin of the template content" % content, reader.lineno)
            extends = Extends(content)
            if not extends.template_name:
                raise ParseError("Missing template file path for '<%% %s %%>'" % content, reader.lineno)
#             nodes.append(extends)
            t = loader.load(extends.template_name).parse()
            tmp_nodes = Nodes()
            tmp_nodes.is_extends = True
            for n in t.nodes:
                tmp_nodes.append(n)
            for n in nodes.data:
                tmp_nodes.append(n)
            nodes = tmp_nodes

    return nodes


if __name__ == '__main__':
    from cProfile import Profile
    n = 30000
    s = """<h1><%= user.name %></h1><h2><%= user.age %></h2>"""*n
    reader = StringReader(s)
    p = Profile()
    p.run("parse(reader)")
    p.print_stats()