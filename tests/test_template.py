import unittest
from template import Template, ParseError


class TemplateTest(unittest.TestCase):
 
    def test_text(self):
        content = """
this is a string 1
this is a string 2
this is a string 3
"""
        self.assertEqual(content, Template(content).generate())
  
    def test_base_variable(self):
        t = Template("""Her name is <%= name %>, and <%= age %> years old""")
        self.assertEqual("""Her name is Lucy, and 10 years old""",
                         t.generate(name="Lucy", age=10))
                   
    def test_base_cn_variable(self):
        t = Template("""她叫<%= name %>, 今年<%= age %>岁 """)
        self.assertEqual("""她叫露西, 今年10岁 """,
                         t.generate(name="露西", age=10))
                   
    def test_comment(self):
        t = Template("Hello<%# TODO i18n %> <%= name %>!")
        self.assertEqual("Hello 中国!", t.generate(name="中国"))
          
    def test_expressions(self):
        t = Template("<%= 1 + 2 %>")
        self.assertEqual("3", t.generate())
                    
        t = Template("1 + 2 = <%= 1 + 2 %>")
        self.assertEqual('1 + 2 = 3', t.generate())
                  
        t = Template("<%= sum([ x for x in range(10) ]) %>")
        self.assertEqual('45', t.generate())
             
        t = Template("<%= 1 / 2 %>")
        self.assertEqual('0.5', t.generate())
    
    def test_if_empty_body(self):
        template = Template("<% if True %><% else %><% end %>")
        self.assertEqual(template.generate(), "")
       
    def test_if_a_line(self):
        t = Template("""<% if x > 10 %>Great<% elif x < 10 %>Less<% else %>Equal<% end %>""")
        self.assertEqual("Great", t.generate(x=20))
        self.assertEqual("Equal", t.generate(x=10))
        self.assertEqual("Less", t.generate(x=5))
      
    def test_if_multi_lines(self):
        t = Template("""
<% if x > 10 %>
    Great
<% elif x < 10 %>
    Less
<% else %>
    Equal
<% end %>""")
        self.assertEqual("\n\n    Great\n", t.generate(x=20))
        self.assertEqual("\n\n    Equal\n", t.generate(x=10))
        self.assertEqual("\n\n    Less\n", t.generate(x=5))
     
    def test_multi_level_if(self):
        t = Template("""
<% if x > 10 %>
    <% if y < 10 %>
        x > 10 and y < 10
    <% elif y == 10 %>
        x > 10 and y = 10
    <% else %>
        x > 10 and y > 10
    <% end %>
<% elif x < 10 %>
    <% if y < 10 %>
        x < 10 and y < 10
    <% elif y == 10 %>
        x < 10 and y = 10
    <% else %>
        x < 10 and y > 10
    <% end %>
<% else %>
    <% if y < 10 %>
        x = 10 and y < 10
    <% elif y == 10 %>
        x = 10 and y = 10
    <% else %>
        x = 10 and y > 10
    <% end %>
<% end %>""")
        self.assertEqual("\n\n    \n        x > 10 and y < 10\n    \n", t.generate(x=20, y=5))
        self.assertEqual("\n\n    \n        x > 10 and y = 10\n    \n", t.generate(x=20, y=10))
        self.assertEqual("\n\n    \n        x > 10 and y > 10\n    \n", t.generate(x=20, y=20))
             
        self.assertEqual("\n\n    \n        x = 10 and y < 10\n    \n", t.generate(x=10, y=5))
        self.assertEqual("\n\n    \n        x = 10 and y = 10\n    \n", t.generate(x=10, y=10))
        self.assertEqual("\n\n    \n        x = 10 and y > 10\n    \n", t.generate(x=10, y=20))
             
        self.assertEqual("\n\n    \n        x < 10 and y < 10\n    \n", t.generate(x=5, y=5))
        self.assertEqual("\n\n    \n        x < 10 and y = 10\n    \n", t.generate(x=5, y=10))
        self.assertEqual("\n\n    \n        x < 10 and y > 10\n    \n", t.generate(x=5, y=20))
             
    def test_for(self):
        t = Template("""
<% for i in range(10) %>
    <%= i %>
<% end %>""")
        relt = ''.join(t.generate().split())
        self.assertEqual(relt, "0123456789")
          
    def test_for_contains_html_tag(self):
        t = Template("""
<ul>
<% for i in range(3) %>
    <li><%= i %></li>
<% end %>
</ul>""")
        self.assertEqual("\n<ul>\n\n    <li>0</li>\n\n    <li>1</li>\n\n    <li>2</li>\n\n</ul>", t.generate())
  
    def test_for_which_contains_continue_and_continue(self):
        t = Template("""
<% for i in range(10) %>
    <% if i == 2 %>
        <% continue %>
    <% end %>
    <%= i %>
    <% if i == 6 %>
        <% break %>
    <% end %>
<% end %>""")
        relt = ''.join(t.generate().split())
        self.assertEqual(relt, "013456")
 
    def test_parse_error_not_end_block(self):
        with self.assertRaises(ParseError) as err:
            Template("<% if True %>")
        self.assertEqual("Missing <% end %> block for <% if True %> block at <string>", str(err.exception))
           
        with self.assertRaises(ParseError) as err:
            Template("<% for x in range(10) %>")
        self.assertEqual("Missing <% end %> block for <% for x in range(10) %> block at <string>", str(err.exception))

        with self.assertRaises(ParseError) as err:
            Template("<% for x in range(10) %> <% if x == 0 %> <% end %>")
        self.assertEqual("Missing <% end %> block for <% for x in range(10) %> block at <string>", str(err.exception))
         
    def test_parse_error_when_missing_if_block(self):
        with self.assertRaises(ParseError) as err:
            Template("<% elif True %><% end %>")
        self.assertEqual("Missing <% if %> block before <% elif True %> block at <string>", str(err.exception))
             
        with self.assertRaises(ParseError) as err:
            Template("<% else %><% end %>")
        self.assertEqual("Missing <% if %> block before <% else %> block at <string>", str(err.exception))
         
    def test_parse_error_when_more_end_tag(self):
        with self.assertRaises(ParseError) as err:
            Template("<% end %>")
        self.assertEqual("Missing <% if %> block or <% for %> block before <% end %> block at <string>", str(err.exception))
        
        with self.assertRaises(ParseError) as err:
            Template("<% if %><% end %><% end %>")
        self.assertEqual("Missing <% if %> block or <% for %> block before <% end %> block at <string>", str(err.exception))
    
    def test_from_file(self):
        t = Template.from_file("./htmls/simple/index.html")
        relt = ''.join(t.generate().split())
        self.assertEqual(relt, "013456")

    
if __name__ == '__main__':
    unittest.main()
