# coding: utf8
from sweet._tests import TestCase
from sweet.template import Template


class TestFormSubmit(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.submit() %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input name="commit" type="submit" value="Save changes" data-disable-with="Save changes" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.submit("Edit this article") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input name="commit" type="submit" value="Edit this article" data-disable-with="Edit this article" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.submit( "Save edits", disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input name="commit" type="submit" value="Save edits" disabled="disabled" data-disable-with="Save edits" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.submit("Edit", class_="edit_button") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input name="commit" type="submit" value="Edit" class="edit_button" data-disable-with="Edit" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.submit( "Save", html={ 'data-confirm': "Are you sure?" }) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input name="commit" type="submit" value="Save" data-confirm="Are you sure?" data-disable-with="Save" />
</form>
""", t.render())

        t = Template("""
<%= using form("/user/new") do f %>
    <%= f.submit( "Complete sale", html={ 'data-disable-with': "Submitting..." }) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input name="commit" type="submit" value="Complete sale" data-disable-with="Submitting..." />
</form>
""", t.render())



if __name__ == '__main__':
    import unittest
    unittest.main()
