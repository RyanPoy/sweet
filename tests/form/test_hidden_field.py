# coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from tests.__init__ import TestCase
import unittest
import os
from template import Template


class HiddenFieldTest(TestCase):

    def test_form_with_url_and_hidden_field_tag(self):

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.hidden_field('tags_list') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="tags_list" name="tags_list" type="hidden" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.hidden_field('token', 'VUBJKB23UIVI1UU1VOBVI@') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="token" name="token" type="hidden" value="VUBJKB23UIVI1UU1VOBVI@" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.hidden_field('collected_input', html={"onchange": "alert('Input collected!')" }) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="collected_input" name="collected_input" type="hidden" onchange="alert('Input collected!')" />
</form>
""", t.render())

        

if __name__ == '__main__':
    unittest.main()
