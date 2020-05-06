# coding: utf8
import os
from sweet.template.parse import parse
from sweet.template.libs import Scanner, CodeGen
from sweet.template.nodes import Extends, Block, Text
from sweet.template.form import Form

normpath = os.path.normpath
dirname = os.path.dirname
abspath = os.path.abspath
joinpath = os.path.join


class Loader(object):

    def __init__(self, root_abs_dir, debug=True):
        self.root_dir = normpath(abspath(root_abs_dir))
        self.tmpl_dict_cache = {} # filename: Template
        self.debug = debug 
    
    def load(self, tmpl_path, parent_path=None):
        abs_path = self.build_path(tmpl_path, parent_path)
        if abs_path not in self.tmpl_dict_cache:
            content = self.get_template_content(abs_path)
            tmpl = Template(content, name=abs_path, loader=self, debug=self.debug)
            self.tmpl_dict_cache[abs_path] = tmpl
        return self.tmpl_dict_cache[abs_path]


class FileLoader(Loader):
    
    def build_path(self, tmpl_path, parent_path=None):
        if not parent_path:
            return normpath(joinpath(self.root_dir, tmpl_path))
        return normpath(joinpath(dirname(parent_path), tmpl_path))

    def get_template_content(self, filepath):
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


class _DeferBlock(object):
    def __init__(self, name):
        self.name = name


class _Nodes(object):
    
    def __init__(self):
        self._data = []
        self._blocks = {}
        self.only_append_block = False
        
    def append(self, node):
        if isinstance(node, Block):
            if node.name not in self._blocks:
                self._data.append(_DeferBlock(node.name))
            self._blocks[node.name] = node
        else:
            if self.only_append_block:
                if isinstance(node, Text) and not node.content.strip():
                    self._data.append(node)
            else:
                self._data.append(node)
        return self
    
    @property
    def data(self):
        ns = []
        for n in self._data:
            if isinstance(n, _DeferBlock):
                n = self._blocks[n.name]
            ns.append(n)
        return ns


class Template(object):
    
    def __init__(self, content, name="<string>", loader=None, debug=True):
        self.nodes = None
        self.name = name
        self.loader = loader

        self.scanner = Scanner(content)
        self.compiled = ''
        self.debug = debug
        
    def parse(self):
        if self.debug or self.nodes is None:
            nodes = parse(self, self.loader)
            self.nodes = self._expand(nodes)
            if self.debug:
                self.scanner.reset()
        return self
    
    def compile(self):
        if self.debug or not self.compiled:
            codegen = CodeGen()
            codegen.begin()
            for node in self.nodes:
                node.compile_with(codegen)
            codegen.end()
            self.compiled = codegen.gen()
        return self
    
    def _expand(self, nodes):
        ns = _Nodes()
        for idx, n in enumerate(nodes):
            if isinstance(n, Extends): # Extends Must first Node
                assert (idx == 0 or idx == 1)
                t = self.loader.load(n.name, self.name).parse()
                for tmp_node in t.nodes:
                    ns.append(tmp_node)
                ns.only_append_block = True
            else:
                ns.append(n)
        return ns.data

    def render(self, **kwargs):
        self.parse()
        self.compile()
        try:
            all_kwargs = {
                'form': Form
            }
            all_kwargs.update(kwargs)
            exec(self.compiled, all_kwargs)
            _my_temp_exec = all_kwargs['_my_temp_exec']
            return _my_temp_exec()
        except:
            # print (self.compiled)
            raise
        
    def format_error(self, msg):
        return FormatError(msg, self.name, self.scanner.lineno)


class FormatError(Exception):

    def __init__(self, message, filename=None, lineno=0):
        self.message = message
        self.filename = filename
        self.lineno = lineno

    def __str__(self):
        return '%s on %s at line %s' % (self.message, self.filename, self.lineno)
