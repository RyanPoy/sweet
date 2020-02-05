#coding: utf8
import unittest
import os
from __init__ import TestCase
from parse import ParseError
from lib import Template, Loader
from nodes import Text, Expression, Comment, If, EndIf, Elif, Else, For, EndFor,\
    Include, Extends, Block, EndBlock


class ParseTest(TestCase):
    
    def test_text(self):
        content = """
this is a string 1
this is a string 2
this is a string 3
"""
        nodes = Template(content).parse().nodes
        self.assertEqual(1, len(nodes))
        self.assertEqual(content, nodes[0].content)
    
    def test_base_expression(self):
        nodes = Template("她叫<%= name %>, 今年<%= age %>岁 ").parse().nodes
        self.assertEqual(5, len(nodes))

        self.assertTrue(isinstance(nodes[0], Text))
        self.assertEqual("她叫", nodes[0].content)

        self.assertTrue(isinstance(nodes[1], Expression))
        self.assertEqual("name", nodes[1].content)

        self.assertTrue(isinstance(nodes[2], Text))
        self.assertEqual(", 今年", nodes[2].content)

        self.assertTrue(isinstance(nodes[3], Expression))
        self.assertEqual("age", nodes[3].content)

        self.assertTrue(isinstance(nodes[4], Text))
        self.assertEqual("岁 ", nodes[4].content)

    def test_complex_expression(self):
        n = Template("<%= 1 + 2 %>").parse().nodes[0]
        self.assertEqual("1 + 2", n.content)
 
    def test_comment(self):
        nodes = Template("Hello<%# TODO i18n %> <%= name %>!").parse().nodes
        self.assertEqual(5, len(nodes))
        self.assertEqual("Hello", nodes[0].content)
 
        self.assertTrue(isinstance(nodes[1], Comment))
        self.assertEqual("TODO i18n", nodes[1].content)

        self.assertEqual(" ", nodes[2].content)
        self.assertEqual("name", nodes[3].content)
        self.assertEqual("!", nodes[4].content)

    def test_if_elif_else(self):
        nodes = Template("""<% if x > 10 %>大于10<% elif x < 10 %>小于10<% else %>等于10<% end %>""").parse().nodes
        self.assertEqual(1, len(nodes))
        self.assertTrue(isinstance(nodes[0], If))
        self.assertEqual("if x > 10", nodes[0].content)
        
        children = nodes[0].children
        self.assertEqual(6, len(children))
        
        self.assertEqual('大于10', children[0].content)
        
        self.assertTrue(isinstance(children[1], Elif))
        self.assertEqual('elif x < 10', children[1].content)
        
        self.assertEqual('小于10', children[2].content)
        
        self.assertTrue(isinstance(children[3], Else))
        self.assertEqual('else', children[3].content)
        
        self.assertEqual('等于10', children[4].content)
        
        self.assertTrue(isinstance(children[5], EndIf))
        self.assertEqual('end', children[5].content)
        
    def test_for(self):
        nodes = Template("""<% for x in range(5) %><%if x = 10 %><%=x%><% end %><% end %>""").parse().nodes
        self.assertEqual(1, len(nodes))
        self.assertTrue(isinstance(nodes[0], For))
        self.assertEqual('for x in range(5)', nodes[0].content)
        
        children = nodes[0].children
        self.assertEqual(2, len(children))
        self.assertTrue(isinstance(children[0], If))
        self.assertTrue(isinstance(children[1], EndFor))
        self.assertEqual('end', children[1].content)

    def test_parse_error_when_has_elif_but_not_has_if(self):
        with self.assertRaises(ParseError) as err:
            Template("""<%elif x = 10 %><%=x%><% end %>""").parse().nodes
        self.assertEqual("Missing '<% if %>' before '<% elif x = 10 %>'", str(err.exception))

    def test_parse_error_when_has_else_but_not_has_if(self):
        with self.assertRaises(ParseError) as err:
            Template("""<%else x = 10 %><%=x%><% end %>""").parse().nodes
        self.assertEqual("Missing '<% if %>' before '<% else x = 10 %>'", str(err.exception))    
        
    def test_parse_error_when_has_end_but_not_has_if_or_for(self):
        with self.assertRaises(ParseError) as err:
            Template("""<% end %>""").parse().nodes
        self.assertEqual("Missing '<% if|for|block %>' before '<% end %>'", str(err.exception))
    
    def test_parse_error_when_has_if_but_not_has_end(self):
        with self.assertRaises(ParseError) as err:
            Template("""<%if x = 10 %><%=x%>""").parse().nodes
        self.assertEqual("Missing '<% end %>' for '<% if x = 10 %>'", str(err.exception))

    def test_parse_error_when_has_for_but_not_has_end(self):
        with self.assertRaises(ParseError) as err:
            Template("""<%for x in range(10) %><%=x%>""").parse().nodes
        self.assertEqual("Missing '<% end %>' for '<% for x in range(10) %>'", str(err.exception))
        
    def tmpl_from(self, filepath):
        dirname = os.path.dirname(os.path.abspath(__file__))
        loader = Loader(os.path.join(dirname, 'htmls'))
        return loader.load(filepath)
    
    def test_parse_include(self):
        nodes = Template("""<ul>测试include是否OK<%include _partial.html members=users %></ul>""").parse().nodes
        self.assertEqual(3, len(nodes))
        self.assertEqual("<ul>测试include是否OK", nodes[0].content)
        self.assertTrue(isinstance(nodes[1], Include))
        include = nodes[1]
        self.assertEqual("_partial.html", include.template_name)
        self.assertEqual(["members=users"], include.attrs)
        self.assertEqual("</ul>", nodes[2].content)
        
    def test_parse_include_has_not_attrs(self):
        nodes = Template("""<ul>测试include是否OK<%include _partial.html%></ul>""").parse().nodes
        self.assertEqual(3, len(nodes))
        self.assertEqual("<ul>测试include是否OK", nodes[0].content)
        self.assertTrue(isinstance(nodes[1], Include))
        """_partial.html members=users %>"""
        include = nodes[1]
        self.assertEqual("_partial.html", include.template_name)
        self.assertEqual([], include.attrs)
        self.assertEqual("</ul>", nodes[2].content)
    
    def test_parse_include_error_if_not_has_fname(self):
        with self.assertRaises(ParseError) as err:
            Template("""<ul>测试include是否OK<%include %></ul>""").parse()
        
        self.assertEqual("Missing template file path for '<% include %>'", str(err.exception))
        
    def test_parse_extends(self):
        nodes = Template("""\r\n\t<% extends base.html %>""").parse().nodes
        self.assertEqual(2, len(nodes))
        self.assertEqual("\r\n\t", nodes[0].content)
        self.assertTrue(isinstance(nodes[1], Extends))
        self.assertEqual("base.html", nodes[1].template_name)
        
    def test_parse_extends_error_if_not_has_fname(self):
        with self.assertRaises(ParseError) as err:
            Template("""<% extends %>""").parse()
         
        self.assertEqual("Missing template file path for '<% extends %>'", str(err.exception))
        
    def test_parse_extends_error_if_extends_does_not_begin_of_template_content(self):
        with self.assertRaises(ParseError) as err:
            Template("""<p /><% extends base.html %>""").parse()
        self.assertEqual("'<% extends base.html %>' must begin of the template content", str(err.exception))

    def test_parse_block(self):
        nodes = Template("""<% extends base.html %><% block title %>标题文本<% end %>""").parse().nodes
        self.assertEqual(2, len(nodes))
        self.assertTrue(isinstance(nodes[1], Block))
        self.assertEqual('block title', nodes[1].content)
        self.assertEqual(2, len(nodes[1].children))
        children = nodes[1].children
        self.assertEqual("标题文本", children[0].content)
        self.assertTrue(isinstance(children[1], EndBlock))

    def test_error_if_block_not_end(self):
        with self.assertRaises(ParseError) as err:
            Template("""<% extends base.html %><% block title %>标题文本""").parse()
        self.assertEqual("Missing '<% end %>' for '<% block title %>'", str(err.exception))

    
if __name__ == '__main__':
    unittest.main()