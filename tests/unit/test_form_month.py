# coding: utf8
from sweet.tests.unit import TestCase
import unittest
from template import Template


class MonthTest(TestCase):

    def test_for_tag(self):

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.month("user_born_on") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="month" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.month("user_born_on", "11") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="month" value="11" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.month("user_born_on", _min="01") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="month" min="01" />
</form>
""", t.render())
        

if __name__ == '__main__':
    unittest.main()
