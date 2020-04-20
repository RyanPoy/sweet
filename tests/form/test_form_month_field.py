# coding: utf8
from __init__ import TestCase, UserForTest
import unittest
import os
from template import Template, FormatError


class FormMonthTest(TestCase):

    def test_form_with_url_and_month_field(self):

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.month_field("user_born_on") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="month" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.month_field("user_born_on", "11") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="month" value="11" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.month_field("user_born_on", _min="01") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="month" min="01" />
</form>
""", t.render())
        

if __name__ == '__main__':
    unittest.main()
