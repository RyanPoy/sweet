# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormCheckbox(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.checkbox('accept') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="accept" name="accept" type="checkbox" value="1" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.checkbox('rock', 'rock music') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="rock" name="rock" type="checkbox" value="rock music" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.checkbox('receive_email', 'yes', checked=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="receive_email" name="receive_email" type="checkbox" value="yes" checked="checked" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.checkbox('tos', 'yes', checked=False, html={"class": 'accept_tos'}) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="tos" name="tos" type="checkbox" value="yes" class="accept_tos" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.checkbox('eula', 'accepted', checked=False, disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="eula" name="eula" type="checkbox" value="accepted" disabled="disabled" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
  <% for choice in ['admin', 'normal'] %>
    <%= f.checkbox('role', choice, id_='role-%s'%choice ) %>
  <% end %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="role-admin" name="role" type="checkbox" value="admin" />
    <input id="role-normal" name="role" type="checkbox" value="normal" />
</form>
""", t.render())


    def test_for_model(self):
        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.checkbox('role', value='admin') %>
    <%= f.checkbox('role', value='normal') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_role_admin" name="user['role']" type="checkbox" value="admin" checked="checked" />
    <input id="user_role_normal" name="user['role']" type="checkbox" value="normal" />
</form>
""", t.render(user=User(id=1, name="Ryan", age=20, sex='F', role="admin")))

        t = Template("""
<%= using form("/user/new", model=user) do f %>
  <% for choice in ['admin', 'normal'] %>
    <%= f.checkbox('role', choice) %>
  <% end %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_role_admin" name="user['role']" type="checkbox" value="admin" checked="checked" />
    <input id="user_role_normal" name="user['role']" type="checkbox" value="normal" />
</form>
""", t.render(user=User(id=1, name="Ryan", age=20, sex='F', role="admin")))


if __name__ == '__main__':
    import unittest
    unittest.main()
