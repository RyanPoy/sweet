# coding: utf8
import os
from parse import parse
normpath = os.path.normpath
dirname = os.path.dirname
abspath = os.path.abspath
joinpath = os.path.join


class StringReader(object):

    def __init__(self, s):
        self.s = s
        self.pos = 0
        self._length = len(s)
        self.lineno = 1
         
    @property
    def length(self):
        return self._length
 
    def find(self, target, p0=0, p1=None):
        s = self.pos + p0
        e = self.po + p1 if p1 else self.length
        idx = self.s.find(target, s, e)
        if idx == -1:
            return idx
        return idx - self.pos 
 
    def eof(self):
        return self.pos >= self.length
     
    def skip(self, delta):
        if delta < 0:
            delta = 0
        self.pos += delta
        if self.pos >= self.length:
            self.pos = self.length
        return self
 
    def read(self, num=None):
        p2 = min(self.pos + num, self.length) if num else self.length
        s = self.s[self.pos:p2]
        self.pos = p2
        self.lineno += s.count('\n')
        return s
    
    @property
    def remain_s(self):
        return self.s[self.pos:]
    
    def __str__(self):
        return self.remain_s


class Loader(object):

    def __init__(self, root_abs_dir):
        self.root_dir = normpath(abspath(root_abs_dir))
        self.tmpl_dict = {}

    def build_path(self, tmpl_path, parent_path=None):
        if not parent_path:
            return normpath(joinpath(self.root_dir, tmpl_path))
        return normpath(joinpath(dirname(parent_path), tmpl_path))

    def load(self, tmpl_path, parent_path=None):
        abs_path = self.build_path(tmpl_path, parent_path)
        with open(abs_path) as f:
            content = f.read()
            return Template(content, name=abs_path, loader=self)


class Template(object):
    
    def __init__(self, content, name="<string>", loader=None):
        self.nodes = []
        self.name = name
        self.loader = loader
        self.is_parsed = False

        self.reader = StringReader(content)
#         self.codegen = CodeGenerator()
        
    def parse(self):
        if not self.is_parsed:
            self.nodes = parse(self.reader)
            self.is_parsed = True
        return self

    def compile(self):
        pass


if __name__ == '__main__':
    from cProfile import Profile
    n = 30000
    s = """<h1><%= user.name %></h1><h2><%= user.age %></h2>"""*n
    reader = StringReader(s)
    p = Profile()
    p.run("parse(reader)")
    p.print_stats()
