# coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from tests.__init__ import TestCase
import unittest
import os
from template import Template


class TelFieldTest(TestCase):

    def test_form_with_url_and_tel_field(self):
        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.tel_field('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="tel" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.tel_field('tel', '0123456789') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="tel" name="tel" type="tel" value="0123456789" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.tel_field('tel', _class='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="tel" name="tel" type="tel" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.tel_field('tel', '0123456789', _class='special_input', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="tel" name="tel" type="tel" value="0123456789" disabled="disabled" class="special_input" />
</form>
""", t.render())
        """
        
        # => 
        """

if __name__ == '__main__':
    unittest.main()
