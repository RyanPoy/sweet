#coding: utf8
from __init__ import TestCase
import unittest
from parse import ParseError
from template import  MemLoader


class ExtendsTest(TestCase):
    
    def test_should_ignore_not_block_tag_if_template_is_extends(self):
        loader = MemLoader({
            "base.html": "<% block title %><% end %>",
            "index.html": "<% extends base.html %><p>文本是要忽略的</p><% block title %><b>标题要保留</b><% end %>",
        })
        self.assertEqual('<b>标题要保留</b>', loader.load("index.html").render())
    
    def test_block_cover(self):
        loader = MemLoader({
            "base.html": "开始<% block title%>标题<% end %><% block body%>主体<% end %>结束",
            "index.html": """  <% extends base.html %><% block title%>真正的标题<% end %><% block body%>真正的主体<% end %>""",
        })
        self.assertEqual('真正的标题真正的主体', loader.load("index.html").render())

    def test_parse_extends_error_if_not_has_fname(self):
        loader = MemLoader({
            "base.html": "开始<% block title%>标题<% end %><% block body%>主体<% end %>结束",
            "index.html": """<% extends %><% block title%>真正的标题<% end %><% block body%>真正的主体<% end %>""",
        })
        with self.assertRaises(ParseError) as err:
            loader.load('index.html').render()
        self.assertEqual("Missing template file path for '<% extends %>'", str(err.exception))
        
    def test_parse_extends_error_if_extends_does_not_begin_of_template_content(self):
        loader = MemLoader({
            "base.html": "开始<% block title%>标题<% end %><% block body%>主体<% end %>结束",
            "index.html": """extends前面有内容是要出错的<% extends base.html%>""",
        })
        with self.assertRaises(ParseError) as err:
            loader.load('index.html').render()
        self.assertEqual("'<% extends base.html %>' must begin of the template content", str(err.exception))
        
    def test_error_if_block_not_end(self):
        loader = MemLoader({
            'base.html': '<% block title %>标题文本',
            "index.html": '<% extends base.html %>'
        })
        with self.assertRaises(ParseError) as err:
            loader.load('index.html').render()
        self.assertEqual("Missing '<% end %>' for '<% block title %>'", str(err.exception))
        

if __name__ == '__main__':
    unittest.main()