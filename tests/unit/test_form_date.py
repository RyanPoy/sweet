# coding: utf8
from sweet.tests.unit import TestCase
import unittest
from template import Template


class DateTest(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.date('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="date" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.date('date', '2020-01-01') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="date" name="date" type="date" value="2020-01-01" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.date('date', _class='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="date" name="date" type="date" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.date('date', '2020-01-01', disabled=True, _class='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="date" name="date" type="date" value="2020-01-01" disabled="disabled" class="special_input" />
</form>
""", t.render())

if __name__ == '__main__':
    unittest.main()
