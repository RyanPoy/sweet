# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormHidden(TestCase):

    def test_for_tag(self):

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.hidden('tags_list') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="tags_list" name="tags_list" type="hidden" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.hidden('token', 'VUBJKB23UIVI1UU1VOBVI@') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="token" name="token" type="hidden" value="VUBJKB23UIVI1UU1VOBVI@" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.hidden('collected_input', html={"onchange": "alert('Input collected!')" }) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="collected_input" name="collected_input" type="hidden" onchange="alert('Input collected!')" />
</form>
""", t.render())

    def test_for_model(self):

        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.hidden('id') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_id" name="user['id']" type="hidden" value="10" />
</form>
""", t.render(user=User(name='Jon', id=10)))
        

if __name__ == '__main__':
    import unittest
    unittest.main()
