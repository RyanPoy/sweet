# coding: utf8
import os
from parse import parse
from nodes import Extends, Include
from lib import StringReader, CodeGenerator

normpath = os.path.normpath
dirname = os.path.dirname
abspath = os.path.abspath
joinpath = os.path.join


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
        if abs_path not in self.tmpl_dict:
            with open(abs_path) as f:
                content = f.read()
                tmpl = Template(content, name=abs_path, loader=self)
                self.tmpl_dict[abs_path] = tmpl
        return self.tmpl_dict[abs_path]


class MemLoader(Loader):

    def __init__(self, root_abs_dir):
        super().__init__(root_abs_dir)
        self.content_dict = {}

    def load(self, tmpl_path, parent_path=None):
        content = self.content_dict[tmpl_path]
        return Template(content, name=tmpl_path, loader=self)

    
class Template(object):
    
    def __init__(self, content, name="<string>", loader=None):
        self.nodes = []
        self.name = name
        self.loader = loader
        self.is_parsed = False

        self.reader = StringReader(content)
        self.codegen = CodeGenerator()
        self.compiled = ''
        
    def parse(self):
        if not self.is_parsed:
            self.nodes = parse(self.reader)
            self._expand()
            self.is_parsed = True
        return self
    
    def _expand(self):
        for node in self.nodes:
            if not self.loader or not isinstance(node, (Include, Extends)):
                continue
            self.loader.load(node.template_name)
        return self

    def compile(self):
        if not self.compiled:
            self.codegen.begin()
            for node in self.nodes:
                node.compile_with(self.codegen)
            self.codegen.end()
            self.compiled = str(self.codegen)
            
#             print('*'*10)
#             print(self.compiled)
#             print('*'*10)

        return self
    
    def render(self, **kwargs):
        self.parse()
        self.compile()
        exec(self.compiled, kwargs)
        _my_temp_exec = kwargs['_my_temp_exec']
        return _my_temp_exec()

