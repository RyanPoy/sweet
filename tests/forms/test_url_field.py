# coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from tests.__init__ import TestCase
import unittest
import os
from template import Template


class UrlFieldTest(TestCase):

    def test_form_with_url_and_url_field(self):
        
        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.url_field('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="url" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.url_field('url', 'http://www.baidu.com') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="url" name="url" type="url" value="http://www.baidu.com" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.url_field('url', _class='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="url" name="url" type="url" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.url_field('url', 'http://www.baidu.com', _class='special_input', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="url" name="url" type="url" value="http://www.baidu.com" disabled="disabled" class="special_input" />
</form>
""", t.render())


if __name__ == '__main__':
    unittest.main()
