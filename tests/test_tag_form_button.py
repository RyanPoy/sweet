# coding: utf8
from __init__ import TestCase, UserForTest
import unittest
import os
from template import Template, FormatError


class TagFormButtonTest(TestCase):

    def test_form_with_url_and_button_tag(self):
        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.button() %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <button name="button" type="submit">Button</button>
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.button('Reset', tp='reset') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <button name="button" type="reset">Reset</button>
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.button('Button', tp='button') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <button name="button" type="button">Button</button>
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.button('Reset', tp='reset', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <button name="button" type="reset" disabled="disabled">Reset</button>
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.button('Save', html={'data-confirm': 'Are you sure?'}) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <button name="button" type="submit" data-confirm="Are you sure?">Save</button>
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.button('Checkout', html={"data-disable-with": "Please wait..."}) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <button name="button" type="submit" data-disable-with="Please wait...">Checkout</button>
</form>
""", t.render())


if __name__ == '__main__':
    unittest.main()
