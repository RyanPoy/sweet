# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormSearch(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.search('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="search" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.search('search', 'Enter your search query here') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="search" name="search" type="search" value="Enter your search query here" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.search('search', None, class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="search" name="search" type="search" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.search('search', 'Enter your search query here', class_='special_input', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="search" name="search" type="search" value="Enter your search query here" disabled="disabled" class="special_input" />
</form>
""", t.render())


    def test_for_model(self):
        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.search('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_name" name="user['name']" type="search" value="Jon" />
</form>
""", t.render(user=User(name="Jon")))


if __name__ == '__main__':
    import unittest
    unittest.main()
