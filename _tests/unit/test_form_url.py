# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormUrl(TestCase):

    def test_for_tag(self):

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.url('name') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="name" name="name" type="url" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.url('url', 'http://www.baidu.com') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="url" name="url" type="url" value="http://www.baidu.com" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.url('url', class_='special_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="url" name="url" type="url" class="special_input" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.url('url', 'http://www.baidu.com', class_='special_input', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="url" name="url" type="url" value="http://www.baidu.com" disabled="disabled" class="special_input" />
</form>
""", t.render())


    def test_for_model(self):

        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.url('blog_address') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_blog_address" name="user['blog_address']" type="url" value="https://www.ryanpoy.com" />
</form>
""", t.render(user=User(blog_address="https://www.ryanpoy.com")))


if __name__ == '__main__':
    import unittest
    unittest.main()
