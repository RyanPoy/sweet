# coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from tests.__init__ import TestCase
import unittest
import os
from template import Template


class NumberFieldTest(TestCase):

    def test_form_with_url_and_number_field(self):
        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.number_field('quantity') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.number_field('quantity', '1') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" value="1" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.number_field('quantity', _class='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.number_field('quantity', _class='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.number_field('quantity', _min=1) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" min="1" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.number_field('quantity', _max=9) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" max="9" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.number_field('quantity', _max=9) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" max="9" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.number_field('quantity', _min=1, _max=9) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" min="1" max="9" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.number_field('quantity', _min=1, _max=9, step=2) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" min="1" max="9" step="2" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.number_field('quantity', '1', _class='special_input', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" value="1" disabled="disabled" class="special_input" />
</form>
""", t.render())
        

if __name__ == '__main__':
    unittest.main()
