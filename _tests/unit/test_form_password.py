# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormPassword(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.password('pass') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="pass" name="pass" type="password" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.password('secret', 'Your secret here') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="secret" name="secret" type="password" value="Your secret here" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.password('masked', class_='masked_input_field') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="masked" name="masked" type="password" class="masked_input_field" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.password('token', '', size=15) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="token" name="token" type="password" value="" size="15" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.password('key', maxlength=16) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="key" name="key" type="password" maxlength="16" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.password('confirm_pass', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="confirm_pass" name="confirm_pass" type="password" disabled="disabled" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.password('pin', '1234', maxlength=4, size=6, class_="pin_input") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="pin" name="pin" type="password" value="1234" size="6" maxlength="4" class="pin_input" />
</form>
""", t.render())

    def test_for_model(self):

        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.password('password') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_password" name="user['password']" type="password" value="abc123" />
</form>
""", t.render(user=User(name='Jon', password="abc123")))

if __name__ == '__main__':
    import unittest
    unittest.main()
