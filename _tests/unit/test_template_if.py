# coding: utf8
from sweet._tests import TestCase
from sweet.template import Template, FormatError
import os


class TestTemplateIf(TestCase):

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
        self.assertEqual("\n    Great", t.render(x=20))
        self.assertEqual("\n    Equal", t.render(x=10))
        self.assertEqual("\n    Less", t.render(x=5))
 
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
        self.assertEqual("\n        x > 10 and y < 10", t.render(x=20, y=5))
        self.assertEqual("\n        x > 10 and y = 10", t.render(x=20, y=10))
        self.assertEqual("\n        x > 10 and y > 10", t.render(x=20, y=20))
                 
        self.assertEqual("\n        x = 10 and y < 10", t.render(x=10, y=5))
        self.assertEqual("\n        x = 10 and y = 10", t.render(x=10, y=10))
        self.assertEqual("\n        x = 10 and y > 10", t.render(x=10, y=20))
                 
        self.assertEqual("\n        x < 10 and y < 10", t.render(x=5, y=5))
        self.assertEqual("\n        x < 10 and y = 10", t.render(x=5, y=10))
        self.assertEqual("\n        x < 10 and y > 10", t.render(x=5, y=20))

    def test_parse_error_when_has_elif_but_not_has_if(self):
        with self.assertRaises(FormatError) as err:
            Template("""<%elif x = 10 %><%=x%><% end %>""").render(x=20)
        self.assertEqual("Missing '<% if %>' before '<% elif x = 10 %>' on <string> at line 1", str(err.exception))

    def test_parse_error_when_has_else_but_not_has_if(self):
        with self.assertRaises(FormatError) as err:
            Template("""<%else x = 10 %><%=x%><% end %>""").render(x=20)
        self.assertEqual("Missing '<% if %>' before '<% else x = 10 %>' on <string> at line 1", str(err.exception))
        
    def test_parse_error_when_has_end_but_not_has_if_or_for(self):
        with self.assertRaises(FormatError) as err:
            Template("""<% end %>""").parse().nodes
        self.assertEqual("Missing '<% if|for|block %>' before '<% end %>' on <string> at line 1", str(err.exception))
    
    def test_parse_error_when_has_if_but_not_has_end(self):
        with self.assertRaises(FormatError) as err:
            Template("""<%if x = 10 %>
                        <%=x%>""").render(x=20)
        self.assertEqual("Missing '<% end %>' for '<% if x = 10 %>' on <string> at line 2", str(err.exception))

        
if __name__ == '__main__':
    import unittest
    unittest.main()
