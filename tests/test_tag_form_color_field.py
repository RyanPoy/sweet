# coding: utf8
from __init__ import TestCase, UserForTest
import unittest
import os
from template import Template, FormatError


class TagFormColorFieldTest(TestCase):

    def test_form_with_url_and_color_field_tag(self):
        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.color_field('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="color" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.color_field('color', '#DEF726') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="color" name="color" type="color" value="#DEF726" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.color_field('color', html={'class': 'special_input'}) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="color" name="color" type="color" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.color_field('color', '#DEF726', disabled=True, html={'class': 'special_input'}) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="color" name="color" type="color" value="#DEF726" disabled="disabled" class="special_input" />
</form>
""", t.render())

if __name__ == '__main__':
    unittest.main()
