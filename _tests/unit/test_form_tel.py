# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormTel(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.tel('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="tel" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.tel('tel', '0123456789') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="tel" name="tel" type="tel" value="0123456789" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.tel('tel', class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="tel" name="tel" type="tel" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.tel('tel', '0123456789', class_='special_input', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="tel" name="tel" type="tel" value="0123456789" disabled="disabled" class="special_input" />
</form>
""", t.render())

    def test_for_model(self):
        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.tel('phone') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_phone" name="user['phone']" type="tel" value="13936567656" />
</form>
""", t.render(user=User(phone=13936567656)))

if __name__ == '__main__':
    import unittest
    unittest.main()
