# coding: utf8
from sweet.tests.unit import TestCase
import unittest
from sweet.template import Template


class UrlTest(TestCase):

    def test_for_tag(self):

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.url('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="url" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.url('url', 'http://www.baidu.com') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="url" name="url" type="url" value="http://www.baidu.com" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.url('url', _class='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="url" name="url" type="url" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.url('url', 'http://www.baidu.com', _class='special_input', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="url" name="url" type="url" value="http://www.baidu.com" disabled="disabled" class="special_input" />
</form>
""", t.render())


if __name__ == '__main__':
    import unittest
    unittest.main()
