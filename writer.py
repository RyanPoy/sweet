#coding: utf8
from io import StringIO
from _sqlite3 import Statement

class CodeWriter(object):

    FLAG = '  '
    
    def __init__(self):
        self.io = StringIO()
        self.indent  = ''
        self.indent_num = 0

    def begin(self):
        self.io.write("def _tt_exec():\n")
        
        self.io.write(self.FLAG)
        self.io.write("_tt_buff = []\n")
        
        self.io.write(self.FLAG)
        self.io.write("_tt_append = _tt_buff.append\n")
        self.backward_indent()
        return self
    
    def end(self):
        self.forward_indent()
        self.io.write(self.FLAG)
        self.io.write("return ''.join(_tt_buff)")
        return self

    def forward_indent(self):
        self.indent_num -= 1
        self.indent = self.FLAG * self.indent_num
        return self
        
    def backward_indent(self):
        self.indent_num += 1
        self.indent = self.FLAG * self.indent_num
        return self 
        
    def write_line(self, s, is_statement=False):
        self.io.write(self.indent)
        if is_statement:
            self.io.write(s)
        else:
            self.io.write('_tt_append(%s)' % s) 
        self.io.write('\n')
        return self

    def __str__(self):
        self.io.seek(0)
        s = self.io.read()
        self.io.seek(self.io.tell())
        return s
