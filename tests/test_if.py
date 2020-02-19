# coding: utf8
from __init__ import TestCase
import unittest
import os
from template import Template

class IfTemplateTest(TestCase):

    def setUp(self):
        self.dirname = os.path.dirname(os.path.abspath(__file__))

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


if __name__ == '__main__':
    unittest.main()
