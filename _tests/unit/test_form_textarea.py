# coding: utf8
from sweet._tests import TestCase, User
from sweet.template import Template


class TestFormTextarea(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.textarea('post') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <textarea id="post" name="post"></textarea>
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.textarea('bio', 'This is my biography.') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <textarea id="bio" name="bio">This is my biography.</textarea>
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.textarea('body', rows=10, cols=25) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <textarea id="body" name="body" rows="10" cols="25"></textarea>
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.textarea('description', "Description goes here.", disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <textarea id="description" name="description" disabled="disabled">Description goes here.</textarea>
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.textarea('comment', class_='comment_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <textarea id="comment" name="comment" class="comment_input"></textarea>
</form>
""", t.render())
        
    def test_for_model(self):

        t = Template("""
<%= using form("/user/new", model=user) do f %>
    <%= f.textarea('intro') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <textarea id="user_intro" name="user['intro']">1111,2222,aaaa,bbbb</textarea>
</form>
""", t.render(user=User(intro="1111,2222,aaaa,bbbb")))

if __name__ == '__main__':
    import unittest
    unittest.main()
