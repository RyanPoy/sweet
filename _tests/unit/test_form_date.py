# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template
from sweet.utils import *


class TestFormDate(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.date('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="date" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.date('date', '2020-01-01') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="date" name="date" type="date" value="2020-01-01" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.date('date', class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="date" name="date" type="date" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.date('date', '2020-01-01', disabled=True, class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="date" name="date" type="date" value="2020-01-01" disabled="disabled" class="special_input" />
</form>
""", t.render())

    def test_for_model(self):
        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.date('birthday') %>
<% end %>
""")
        expected_value = """
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_birthday" name="user['birthday']" type="date" value="2019-10-10" />
</form>
"""
        self.assertEqual(expected_value, t.render(user=User(name="Jon", age=20, birthday="2019-10-10")))
        self.assertEqual(expected_value, t.render(user=User(name="Jon", age=20, birthday=str2date("2019-10-10"))))
        self.assertEqual(expected_value, t.render(user=User(name="Jon", age=20, birthday=str2date("2019/10/10"))))


if __name__ == '__main__':
    import unittest
    unittest.main()
