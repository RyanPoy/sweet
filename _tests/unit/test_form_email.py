# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormEmail(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.email('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="email" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.email('email', 'xxx@yyy.com') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="email" name="email" type="email" value="xxx@yyy.com" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.email('email', class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="email" name="email" type="email" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.email('email', 'xxx@yyy.com', disabled=True, class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="email" name="email" type="email" value="xxx@yyy.com" disabled="disabled" class="special_input" />
</form>
""", t.render())

    def test_for_model(self):
        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.email('email') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_email" name="user['email']" type="email" value="jon@fake.com" />
</form>
""", t.render(user=User(name='Jon', email='jon@fake.com')))


if __name__ == '__main__':
    import unittest
    unittest.main()
