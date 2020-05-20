# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template
from datetime import date, datetime
from sweet.utils import *


class TestFormDatetime(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.datetime("user_born_on") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="datetime-local" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.datetime("user_born_on", date(year=2020, month=1, day=1)) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="datetime-local" value="2020-01-01T00:00:00" />
</form>
""", t.render(date=date))

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.datetime("user_born_on", datetime(year=2020, month=1, day=2, hour=10, minute=20, second=30)) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="datetime-local" value="2020-01-02T10:20:30" />
</form>
""", t.render(datetime=datetime))

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.datetime("user_born_on", _min=date(year=2020, month=1, day=2)) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="datetime-local" min="2020-01-02T00:00:00" />
</form>
""", t.render(date=date))

    def test_for_model(self):
        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.datetime("born_on") %>
<% end %>
""")
        expected_value = """
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user['born_on']" type="datetime-local" value="2020-01-01T00:00:00" />
</form>
"""
        self.assertEqual(expected_value, t.render(user=User(name="Jon", age=20, born_on=str2datetime("2020/01/01"))))
        self.assertEqual(expected_value, t.render(user=User(name="Jon", age=20, born_on=str2datetime("2020-01-01"))))
        self.assertEqual(expected_value, t.render(user=User(name="Jon", age=20, born_on=str2datetime("2020-01-01 00:00:00"))))


if __name__ == '__main__':
    import unittest
    unittest.main()
