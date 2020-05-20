# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormRadio(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.radio('favorite_color', 'maroon') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="favorite_color_maroon" name="favorite_color" type="radio" value="maroon" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.radio('receive_updates', 'no', checked=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="receive_updates_no" name="receive_updates" type="radio" value="no" checked="checked" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.radio('time_slot', "3:00 p.m.", checked=False, disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="time_slot_3:00_p.m." name="time_slot" type="radio" value="3:00 p.m." disabled="disabled" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.radio('color', "green", checked=True, class_="color_input") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="color_green" name="color" type="radio" value="green" class="color_input" checked="checked" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
  <% for choice in ['M', 'F'] %>
    <%= f.radio('gender', choice) %>
  <% end %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="gender_M" name="gender" type="radio" value="M" />
    <input id="gender_F" name="gender" type="radio" value="F" />
</form>
""", t.render())

    def test_for_model(self):
        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.radio('gender', 'M') %>
    <%= f.radio('gender', 'F') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_gender_M" name="user['gender']" type="radio" value="M" checked="checked" />
    <input id="user_gender_F" name="user['gender']" type="radio" value="F" />
</form>
""", t.render(user=User(gender='M')))

        t = Template("""
<%= using form("/user/new", model=user) do f %>
  <% for choice in ['M', 'F'] %>
    <%= f.radio('gender', choice) %>
  <% end %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_gender_M" name="user['gender']" type="radio" value="M" />
    <input id="user_gender_F" name="user['gender']" type="radio" value="F" checked="checked" />
</form>
""", t.render(user=User(gender='F')))

if __name__ == '__main__':
    import unittest
    unittest.main()
