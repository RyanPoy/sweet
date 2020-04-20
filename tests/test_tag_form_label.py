# coding: utf8
from __init__ import TestCase, UserForTest
import unittest
import os
from template import Template, FormatError


class TagFormLabelTest(TestCase):

    def test_form_with_url_and_label_tag(self):

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.label('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <label for="name">Name</label>
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.label('name', 'Your Name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <label for="name">Your Name</label>
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.label('name', _class='small_label') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <label for="name" class="small_label">Name</label>
</form>
""", t.render())
        

if __name__ == '__main__':
    unittest.main()
