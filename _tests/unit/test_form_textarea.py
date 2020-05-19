# coding: utf8
from sweet._tests import TestCase
from sweet.template import Template


class TestFormTextarea(TestCase):

    def test_for_tag(self):

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.textarea('post') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <textarea id="post" name="post"></textarea>
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.textarea('bio', 'This is my biography.') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <textarea id="bio" name="bio">This is my biography.</textarea>
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.textarea('body', rows=10, cols=25) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <textarea id="body" name="body" rows="10" cols="25"></textarea>
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.textarea('description', "Description goes here.", disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <textarea id="description" name="description" disabled="disabled">Description goes here.</textarea>
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.textarea('comment', _class='comment_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <textarea id="comment" name="comment" class="comment_input"></textarea>
</form>
""", t.render())
        

if __name__ == '__main__':
    import unittest
    unittest.main()
