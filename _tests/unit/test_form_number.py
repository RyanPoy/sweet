# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormNumber(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.number('quantity') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.number('quantity', '1') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" value="1" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.number('quantity', class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.number('quantity', _min=1) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" min="1" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.number('quantity', _max=9) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" max="9" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.number('quantity', _min=1, _max=9) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" min="1" max="9" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.number('quantity', _min=1, _max=9, step=2) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" min="1" max="9" step="2" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.number('quantity', '1', class_='special_input', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="quantity" name="quantity" type="number" value="1" disabled="disabled" class="special_input" />
</form>
""", t.render())
        
    def test_for_model(self):
        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.number('age') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_age" name="user['age']" type="number" value="20" />
</form>
""", t.render(user=User(name='Jon', age=20)))

if __name__ == '__main__':
    import unittest
    unittest.main()
