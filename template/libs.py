# coding: utf8
from io import StringIO


class Scanner(object):

    def __init__(self, s):
        self.s = s
        self._length = len(s)
        self.reset()
         
    @property
    def length(self):
        return self._length
 
    def find(self, target, p0=0, p1=None):
        s = self.pos + p0
        e = self.pos + p1 if p1 else self.length
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
    
    def reset(self):
        self.pos = 0
        self.lineno = 1

    @property
    def remain_s(self):
        return self.s[self.pos:]
    
    def __str__(self):
        return self.remain_s


class CodeGen(object):

    FLAG = '  '

    def __init__(self):
        self.io = StringIO()
        self.indent  = ''
        self.indent_num = 0

    def begin(self):
        self.io.write("def _my_temp_exec():\n")
        self.backward_indent()

        self.write_line("_my_temp_buff = []", False)
        self.write_line("_my_temp_append = _my_temp_buff.append", False)
        return self
    
    def end(self):
        self.write_line("return ''.join(_my_temp_buff)", False)
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
            self.io.write('_my_temp_append(%s)' % s)
        self.io.write('\n')
        return self
    
    def gen(self):
        length = len(self)
        self.io.seek(0)
        s = self.io.read(length)
        self.io.seek(length)
        return s

    def __len__(self):
        return self.io.tell()
    
    def __str__(self):
        return self.gen()

