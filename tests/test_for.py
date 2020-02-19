# coding: utf8
from __init__ import TestCase, UserForTest
import unittest
import os
from template import Template


class ForTest(TestCase):

    def setUp(self):
        self.dirname = os.path.dirname(os.path.abspath(__file__))
        self.users = [
            UserForTest("露西", 20),
            UserForTest("Lily", 10)
        ]

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

if __name__ == '__main__':
    unittest.main()
