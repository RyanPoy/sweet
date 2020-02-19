# coding: utf8
import os
from parse import parse
from nodes import Extends, Include
from libs import StringReader, CodeGenerator

normpath = os.path.normpath
dirname = os.path.dirname
abspath = os.path.abspath
joinpath = os.path.join


class Loader(object):

    def __init__(self, root_abs_dir):
        self.root_dir = normpath(abspath(root_abs_dir))
        self.tmpl_dict_cache = {} # filename: Template 
    
    def load(self, tmpl_path, parent_path=None):
        abs_path = self.build_path(tmpl_path, parent_path)
        if abs_path not in self.tmpl_dict_cache:
            content = self.get_template_content(abs_path)
            tmpl = Template(content, name=abs_path, loader=self)
            self.tmpl_dict_cache[abs_path] = tmpl
        return self.tmpl_dict_cache[abs_path]


class FileLoader(Loader):
    
    def build_path(self, tmpl_path, parent_path=None):
        if not parent_path:
            return normpath(joinpath(self.root_dir, tmpl_path))
        return normpath(joinpath(dirname(parent_path), tmpl_path))

    def get_templte_content(self, filepath):
        with open(filepath) as f:
            return f.read()


class MemLoader(Loader):
    
    def __init__(self, content_dict):
        super().__init__('')
        self.content_dict = content_dict # filename: content
        
    def build_path(self, tmpl_path, parent_path=None):
        return tmpl_path

    def get_template_content(self, filepath):
        if filepath not in self.content_dict:
            raise FileNotFoundError('%s is not exist' % filepath)
        return self.content_dict[filepath]

    
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
            self.nodes = parse(self.reader, self.loader).data
            self.is_parsed = True
        return self
    
    def compile(self):
        if not self.compiled:
            self.codegen.begin()
            for node in self.nodes:
                node.compile_with(self.codegen)
            self.codegen.end()
            self.compiled = str(self.codegen)
        return self
    
    def render(self, **kwargs):
        self.parse()
        self.compile()
        try:
            exec(self.compiled, kwargs)
            _my_temp_exec = kwargs['_my_temp_exec']
            return _my_temp_exec()
        except:
            print (self.compiled)
            raise
