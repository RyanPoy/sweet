# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormRange(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.range('quantity') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="range" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.range('quantity', '1') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="range" value="1" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.range('quantity', class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="range" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.range('quantity', _in=[1, 9]) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="range" min="1" max="9" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.range('quantity', _in=[1, 9], step=2) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="range" min="1" max="9" step="2" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.range('quantity', '1', class_='special_input', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="range" value="1" disabled="disabled" class="special_input" />
</form>
""", t.render())
        
    def test_for_model(self):
        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.range('age', _in=[16, 36]) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_age" name="user['age']" type="range" value="20" min="16" max="36" />
</form>
""", t.render(user=User(age=20)))


if __name__ == '__main__':
    import unittest
    unittest.main()
