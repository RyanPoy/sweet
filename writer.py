#coding: utf8
from io import StringIO


class CodeWriter(object):

    FLAG = '  '
    
    def __init__(self):
        self.io = StringIO()
        self.indent  = ''
        self.indent_num = 0
    
    def begin(self):
        self.io.write("def _tt_exec():\n")
        self.backward_indent()

        self.write_line("_tt_buff = []", False)
        self.write_line("_tt_append = _tt_buff.append", False)
        return self
    
    def end(self):
        self.write_line("return ''.join(_tt_buff)", False)
        self.forward_indent()
        return self

    def forward_indent(self):
        self.indent_num -= 1
        self.indent = self.FLAG * self.indent_num
        return self
        
    def backward_indent(self):
        self.indent_num += 1
        self.indent = self.FLAG * self.indent_num
        return self 
        
    def write_line(self, s, append=True):
        self.io.write(self.indent)
        if not append:
            self.io.write(s)
        else:
            self.io.write('_tt_append(%s)' % s)
        self.io.write('\n')
        return self
    
    def __len__(self):
        return self.io.tell()
    
    def __str__(self):
        length = len(self)
        self.io.seek(0)
        s = self.io.read(length)
        self.io.seek(length)
        return s
