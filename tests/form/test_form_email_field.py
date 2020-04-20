# coding: utf8
from __init__ import TestCase, UserForTest
import unittest
import os
from template import Template, FormatError


class FormEmailFieldTest(TestCase):

    def test_form_with_url_and_email_field_tag(self):
        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.email_field('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="email" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.email_field('email', 'xxx@yyy.com') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="email" name="email" type="email" value="xxx@yyy.com" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.email_field('email', _class='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="email" name="email" type="email" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.email_field('email', 'xxx@yyy.com', disabled=True, _class='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="email" name="email" type="email" value="xxx@yyy.com" disabled="disabled" class="special_input" />
</form>
""", t.render())

if __name__ == '__main__':
    unittest.main()
