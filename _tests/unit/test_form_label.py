# coding: utf8
from sweet._tests import TestCase
from sweet.template import Template


class TestFormLabel(TestCase):

    def test_for_tag(self):

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.label('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <label for="name">Name</label>
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.label('name', 'Your Name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <label for="name">Your Name</label>
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.label('name', _class='small_label') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <label for="name" class="small_label">Name</label>
</form>
""", t.render())
        

if __name__ == '__main__':
    import unittest
    unittest.main()
