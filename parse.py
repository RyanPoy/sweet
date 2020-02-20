#coding: utf8
from nodes import Text, Expression, Comment, If, EndIf, Elif, Else, For, EndFor,\
    Include, Extends, EndBlock, Block, Continue, Break, Pass


def can_extends(nodes):
    if not nodes:
        return True
    for n in nodes:
        if not isinstance(n, Text):
            return False
        if n.content.strip():
            return False
    return True


def parse(template, parent_tag=''):
    reader = template.reader
    loader = template.loader
    nodes = []
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
            children = parse(template, parent_tag='if')
            if not children or not isinstance(children[-1], EndIf):
                raise template.format_error("Missing '<%% end %%>' for '<%% %s %%>'" % content)
            nodes.append(If(content, children))
        elif content.startswith('elif'):  # <% elif xxx %>
            if parent_tag != 'if':
                raise template.format_error("Missing '<%% if %%>' before '<%% %s %%>'" % content)
            nodes.append(Elif(content))
        elif content.startswith('else'):
            if parent_tag != 'if':
                raise template.format_error("Missing '<%% if %%>' before '<%% %s %%>'" % content)
            nodes.append(Else(content))
        elif content.startswith('for'):
            children = parse(template, parent_tag='for')
            if not children or not isinstance(children[-1], EndFor):
                raise template.format_error("Missing '<%% end %%>' for '<%% %s %%>'" % content)
            nodes.append(For(content, children))
        elif content.startswith('continue'):
            nodes.append(Continue())
        elif content.startswith('break'):
            nodes.append(Break())
        elif content.startswith('pass'):
            nodes.append(Pass())
        elif content.startswith('block'):
            children = parse(template, parent_tag='block')
            if not children or not isinstance(children[-1], EndBlock):
                raise template.format_error("Missing '<%% end %%>' for '<%% %s %%>'" % content)
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
                raise template.format_error("Missing '<% if|for|block %>' before '<% end %>'")
        elif content.startswith('include'):  # <% include %>
            include = Include(content)
            if not include.name:
                raise template.format_error("Missing template file path for '<%% %s %%>'" % content)
            nodes.append(include)
            t = loader.load(include.name).parse()
            for n in t.nodes:
                nodes.append(n)
        elif content.startswith('extends'): # <% extends %>
            if not can_extends(nodes):
                raise template.format_error("'<%% %s %%>' must begin of the template content" % content)
            extends = Extends(content)
            if not extends.name:
                raise template.format_error("Missing template file path for '<%% %s %%>'" % content)
            nodes.append(extends)
    return nodes


if __name__ == '__main__':
    from cProfile import Profile
    from template import Template
    n = 30000
    s = """<h1><%= user.name %></h1><h2><%= user.age %></h2>"""*n
    t = Template(s)
    p = Profile()
    p.run("parse(t)")
    p.print_stats()
