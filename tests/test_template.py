# coding: utf8
from __init__ import TestCase
import unittest
import os
from template import Template


class UserForTest(object):

    def __init__(self, name, age):
        self.name = name
        self.age = age


class TemplateTest(TestCase):

    def setUp(self):
        self.dirname = os.path.dirname(os.path.abspath(__file__))
        self.users = [
            UserForTest("露西", 20),
            UserForTest("Lily", 10)
        ]
        
    def test_text(self):
        content = """
this is a string 1
this is a string 2
this is a string 3
"""
        self.assertEqual(content, Template(content).render())
  
    def test_variable(self):
        t = Template("""她叫<%= name %>, and <%= age %> years old """)
        self.assertEqual("""她叫露西, and 10 years old """,
                         t.render(name="露西", age=10))
                        
    def test_comment(self):
        t = Template("Hello<%# TODO i18n %> <%= name %>!")
        self.assertEqual("Hello 中国!", t.render(name="中国"))
  
    def test_expressions(self):
        t = Template("<%= 1 + 2 %>")
        self.assertEqual("3", t.render())
                         
        t = Template("1 + 2 = <%= 1 + 2 %>")
        self.assertEqual('1 + 2 = 3', t.render())
                       
        t = Template("<%= sum([ x for x in range(10) ]) %>")
        self.assertEqual('45', t.render())
                  
        t = Template("<%= 1 / 2 %>")
        self.assertEqual('0.5', t.render())
    
    def test_empty_if(self):
        t = Template("""<% if x > 10 %><% elif x < 10 %><% else %><% end %>""")
        self.assertEqual("", t.render(x=20))

    def test_if_lines(self):
        t = Template(
"""<% if x > 10 %>
    Great
<% elif x < 10 %>
    Less
<% else %>
    Equal
<% end %>""")
        self.assertEqual("\n    Great\n", t.render(x=20))
        self.assertEqual("\n    Equal\n", t.render(x=10))
        self.assertEqual("\n    Less\n", t.render(x=5))
 
    def test_multi_level_if(self):
        t = Template(
"""<% if x > 10 %>
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
        self.assertEqual("\n    \n        x > 10 and y < 10\n    \n", t.render(x=20, y=5))
        self.assertEqual("\n    \n        x > 10 and y = 10\n    \n", t.render(x=20, y=10))
        self.assertEqual("\n    \n        x > 10 and y > 10\n    \n", t.render(x=20, y=20))
                 
        self.assertEqual("\n    \n        x = 10 and y < 10\n    \n", t.render(x=10, y=5))
        self.assertEqual("\n    \n        x = 10 and y = 10\n    \n", t.render(x=10, y=10))
        self.assertEqual("\n    \n        x = 10 and y > 10\n    \n", t.render(x=10, y=20))
                 
        self.assertEqual("\n    \n        x < 10 and y < 10\n    \n", t.render(x=5, y=5))
        self.assertEqual("\n    \n        x < 10 and y = 10\n    \n", t.render(x=5, y=10))
        self.assertEqual("\n    \n        x < 10 and y > 10\n    \n", t.render(x=5, y=20))

    def test_for(self):
        t = Template(
"""<ul>
<% for (i, u) in enumerate(users) %>
    <li><%= i %>|<%= u.name %>|<%= u.age %></li>
<% end %>
</ul>""")
        self.assertEqual(
"""<ul>

    <li>0|露西|20</li>

    <li>1|Lily|10</li>

</ul>""", t.render(users=self.users))

    def test_for_contains_html_tag(self):
        t = Template(
"""<ul>
<% for i in range(3) %>
    <li><%= i %></li>
<% end %>
</ul>""")
        self.assertEqual("<ul>\n\n    <li>0</li>\n\n    <li>1</li>\n\n    <li>2</li>\n\n</ul>", t.render())
     
    def test_for_which_contains_continue_and_continue(self):
        t = Template(
"""<% for i in range(10) %>
    <% if i == 2 %>
        <% continue %>
    <% end %>
    <%= i %>
    <% if i == 6 %>
        <% break %>
    <% end %>
<% end %>""")
        relt = ''.join(t.render().split())
        self.assertEqual(relt, "013456")
        
    def test_for_empty_body(self):
        t = Template("""<% for i in range(10) %><% end %>""")
        self.assertEqual('', t.render())

#     def test_include(self):
#         loader = MemLoader()
        
#     def test_from_file(self):
#         p = os.path.join(self.dirname, 'htmls/simple/index.html')
#         t = Template.from_file(p)
#         relt = ''.join(t.generate().split())
#         self.assertEqual(relt, "013456")
# 
#     def test_include(self):
#         loader = Template(os.path.join(self.dirname, 'htmls'))
#         t = loader.load('include/index.html')
#         self.assertEqual("""<ul>
#     测试include是否OK
#     
#     <li>露西 | 20</li>
# 
#     <li>Lily | 10</li>
# 
# </ul>""", t.generate(users=self.users))


if __name__ == '__main__':
    unittest.main()
