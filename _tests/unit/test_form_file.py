# coding: utf8
from sweet._tests import TestCase
from sweet.template import Template


class TestFormFile(TestCase):

    def test_for_tag(self):

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.file('attachment') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="attachment" name="attachment" type="file" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.file('avatar', class_='profile_input') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="avatar" name="avatar" type="file" class="profile_input" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.file('picture', disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="picture" name="picture" type="file" disabled="disabled" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.file('resume', value='~/resume.doc') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="resume" name="resume" type="file" value="~/resume.doc" />
</form>
""", t.render())
        
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.file('user_pic', accept='image/png,image/gif,image/jpeg') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_pic" name="user_pic" type="file" accept="image/png,image/gif,image/jpeg" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.file('file', accept='text/html', class_='upload', value='index.html') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="file" name="file" type="file" value="index.html" class="upload" accept="text/html" />
</form>
""", t.render())


if __name__ == '__main__':
    import unittest
    unittest.main()
