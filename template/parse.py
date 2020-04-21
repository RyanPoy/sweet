#coding: utf8
from sweet.template.nodes import Text, Expression, Comment, If, EndIf, Elif, Else, For, EndFor,\
    Include, Extends, EndBlock, Block, Continue, Break, Pass, Using, EndUsing


class NodeList(object):
    
    def __init__(self):
        self.data = []
        self.types = set((If, Elif, Else, For, Block, Include, Extends, EndIf, EndFor, EndBlock, Break, Continue, Pass))

    def push(self, node):
        self.maybe_modify_last_textnode(node)
        return self.data.append(node)
    
    def maybe_modify_last_textnode(self, current_node):
        if not self.data:
            return
        last_node = self.data[-1]
        if isinstance(last_node, Text) and type(current_node) in self.types:
            # if current node is If, Elif, Else, For and last node is Text
            # should remove space which between last node and current node
            content = last_node.content
            idx = content.rfind('\n')
            if idx != -1 and not content[idx:].strip():
                last_node.content = content[:idx]
        elif isinstance(current_node, Text) and type(last_node) in self.types:
            # if current node is Text and last node is If, Elif, Else, For
            # should remove space which between last node and current node
            content = current_node.content
            idx = content.find('\n')
            if idx != -1 and not content[:idx].strip():
                current_node.content = content[idx:]

    def can_extends(self):
        nodes = self.data
        if not nodes:
            return True
        for n in nodes:
            if not isinstance(n, Text):
                return False
            if n.content.strip():
                return False
        return True


def parse(template, parent_tag=''):
    scanner = template.scanner
    loader = template.loader
    nodes = NodeList()
    while True:
        p0 = scanner.find('<%')
        if p0 < 0:  # not found begin tag
            if not scanner.eof():  # end remaining text
                nodes.push(Text(scanner.read()))
            break
        if p0 != 0:  # not begin
            nodes.push(Text(scanner.read(p0)))  # text
            continue

        # process this line, the p0 must equals 0
        scanner.skip(2)  # skip the <% 
        p1 = scanner.find('%>')
        if p1 == -1:  # found the begin tag but not found end tag
            nodes.push(Text(scanner.read()))  # @TODO: maybe should throw exception
            break

        content = scanner.read(p1)
        scanner.skip(2)  # skip the %>

        content = content.strip()
        if not content:
            continue
        
        if content[0] == '=':  # <%=  xxx %>
            origin_content, content = content, content[1:].strip()
            if content.startswith('using'):
                children = parse(template, parent_tag='using')
                if not children or not isinstance(children[-1], EndUsing):
                    raise template.format_error("Missing '<%% end %%>' for '<%% %s %%>'" % origin_content)
                using = Using(content, children)
                if not using.func or not using.var:
                    raise template.format_error("'<%%%s %%>' format error" % origin_content)
                nodes.push(using)
            else:
                nodes.push(Expression(content))  # base expression
        elif content[0] == '#':  # <%# xxx %>
            nodes.push(Comment(content[1:].strip()))
        elif content.startswith('if'):  # <% if xxx %>
            children = parse(template, parent_tag='if')
            if not children or not isinstance(children[-1], EndIf):
                raise template.format_error("Missing '<%% end %%>' for '<%% %s %%>'" % content)
            nodes.push(If(content, children))
        elif content.startswith('elif'):  # <% elif xxx %>
            if parent_tag != 'if':
                raise template.format_error("Missing '<%% if %%>' before '<%% %s %%>'" % content)
            nodes.push(Elif(content))
        elif content.startswith('else'):
            if parent_tag != 'if':
                raise template.format_error("Missing '<%% if %%>' before '<%% %s %%>'" % content)
            nodes.push(Else(content))
        elif content.startswith('for'):
            children = parse(template, parent_tag='for')
            if not children or not isinstance(children[-1], EndFor):
                raise template.format_error("Missing '<%% end %%>' for '<%% %s %%>'" % content)
            nodes.push(For(content, children))
        elif content.startswith('continue'):
            nodes.push(Continue())
        elif content.startswith('break'):
            nodes.push(Break())
        elif content.startswith('pass'):
            nodes.push(Pass())
        elif content.startswith('block'):
            children = parse(template, parent_tag='block')
            if not children or not isinstance(children[-1], EndBlock):
                raise template.format_error("Missing '<%% end %%>' for '<%% %s %%>'" % content)
            nodes.push(Block(content, children))
        elif content.startswith('end'):  # <% end %>
            if parent_tag == 'if':
                nodes.push(EndIf(content))
                break
            elif parent_tag == 'for':
                nodes.push(EndFor(content))
                break
            elif parent_tag == 'block':
                nodes.push(EndBlock(content))
                break
            elif parent_tag == 'using':
                nodes.push(EndUsing(content))
                break
            else:
                raise template.format_error("Missing '<% if|for|block %>' before '<% end %>'")
        elif content.startswith('include'):  # <% include %>
            include = Include(content)
            if not include.name:
                raise template.format_error("Missing template file path for '<%% %s %%>'" % content)
            nodes.push(include)
            try:
                t = loader.load(include.name, template.name).parse()
                for n in t.nodes:
                    nodes.push(n)
            except FileNotFoundError as ex:
                raise template.format_error(str(ex))
        elif content.startswith('extends'): # <% extends %>
            if not nodes.can_extends():
                raise template.format_error("'<%% %s %%>' must begin of the template content" % content)
            extends = Extends(content)
            if not extends.name:
                raise template.format_error("Missing template file path for '<%% %s %%>'" % content)
            nodes.push(extends)
        else:
            raise template.format_error("Unsupport '<%% %s %%>'" % content)
    return nodes.data


# if __name__ == '__main__':
#     from cProfile import Profile
#     from sweet.template import Template
#     n = 30000
#     s = """<h1><%= user.name %></h1><h2><%= user.age %></h2>"""*n
#     t = Template(s)
#     p = Profile()
#     p.run("parse(t)")
#     p.print_stats()
