# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormColor(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.color('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="color" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.color('color', '#DEF726') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="color" name="color" type="color" value="#DEF726" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.color('color', class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="color" name="color" type="color" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.color('color', '#DEF726', disabled=True, class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="color" name="color" type="color" value="#DEF726" disabled="disabled" class="special_input" />
</form>
""", t.render())

    def test_for_model(self):
        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.color('like_color') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_like_color" name="user['like_color']" type="color" value="#FFF" />
</form>
""", t.render(user=User(name="Jon", age=20, like_color="#FFF")))


if __name__ == '__main__':
    import unittest
    unittest.main()
