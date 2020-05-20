# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormWeek(TestCase):

    def test_for_tag(self):

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.week("user_born_week") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_week" name="user_born_week" type="week" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.week("user_born_week", "06") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_week" name="user_born_week" type="week" value="06" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.week("user_born_week", _min="01") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_week" name="user_born_week" type="week" min="01" />
</form>
""", t.render())
        
    def test_for_model(self):

        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.week("born_week") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_week" name="user['born_week']" type="week" value="34" />
</form>
""", t.render(user=User(born_week=34)))

if __name__ == '__main__':
    import unittest
    unittest.main()
