# coding: utf8
from sweet.tests import TestCase
from sweet.template import Template


class HiddenTest(TestCase):

    def test_for_tag(self):

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.hidden('tags_list') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="tags_list" name="tags_list" type="hidden" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.hidden('token', 'VUBJKB23UIVI1UU1VOBVI@') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="token" name="token" type="hidden" value="VUBJKB23UIVI1UU1VOBVI@" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.hidden('collected_input', html={"onchange": "alert('Input collected!')" }) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="collected_input" name="collected_input" type="hidden" onchange="alert('Input collected!')" />
</form>
""", t.render())

        

if __name__ == '__main__':
    import unittest
    unittest.main()
