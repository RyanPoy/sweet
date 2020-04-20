# coding: utf8
from __init__ import TestCase, UserForTest
import unittest
import os
from template import Template, FormatError


class FormCheckboxTest(TestCase):

    def test_form_with_url_and_checkbox_tag(self):
        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.checkbox('accept') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="accept" name="accept" type="checkbox" value="1" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.checkbox('rock', 'rock music') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="rock" name="rock" type="checkbox" value="rock music" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.checkbox('receive_email', 'yes', checked=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="receive_email" name="receive_email" type="checkbox" value="yes" checked="checked" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.checkbox('tos', 'yes', checked=False, html={"class": 'accept_tos'}) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="tos" name="tos" type="checkbox" value="yes" class="accept_tos" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.checkbox('eula', 'accepted', checked=False, disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="eula" name="eula" type="checkbox" value="accepted" disabled="disabled" />
</form>
""", t.render())


if __name__ == '__main__':
    unittest.main()
