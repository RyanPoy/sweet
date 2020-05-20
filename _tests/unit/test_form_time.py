# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormTime(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.time('created_at') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="created_at" name="created_at" type="time" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.time('created_at', '1') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="created_at" name="created_at" type="time" value="1" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.time('created_at', class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="created_at" name="created_at" type="time" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.time('created_at', _min=1) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="created_at" name="created_at" type="time" min="1" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.time('created_at', _max=9) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="created_at" name="created_at" type="time" max="9" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.time('created_at', _min=1, _max=9) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="created_at" name="created_at" type="time" min="1" max="9" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.time('created_at', _min=1, _max=9, step=2) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="created_at" name="created_at" type="time" min="1" max="9" step="2" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.time('created_at', '1', class_='special_input', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="created_at" name="created_at" type="time" value="1" disabled="disabled" class="special_input" />
</form>
""", t.render())
        
    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.time('created_at') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_created_at" name="user['created_at']" type="time" value="1589945183" />
</form>
""", t.render(user=User(created_at="1589945183")))


if __name__ == '__main__':
    import unittest
    unittest.main()
