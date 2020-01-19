# coding: utf8
import re
from cProfile import Profile
from tokens import TextToken, ExpressionToken, CommentToken

# re_nodes = re.compile(r"(?s)({{.*?}}|{%.*?%}|{#.*?#})")
re_nodes = re.compile(r"(?s)(<%=.*?%>|<%.*?%>|<%#.*?%>)")
def parse(s):
    nodes = []
    for t in re_nodes.split(s):
        if t.startswith('<%='):
            nodes.append(ExpressionToken(t[3:-2].strip()))
        elif t.startswith('<%#'):
            nodes.append(CommentToken(t[3:-2]))
        else:
            nodes.append(TextToken(t))

    return nodes

    
if __name__ == '__main__':
    n = 50000
    s = """<h1>{{ user.name }}</h1><h2>{{ user.age }}</h2>"""*n
    p = Profile()
    p.run("parse(s)")
    p.print_stats()

