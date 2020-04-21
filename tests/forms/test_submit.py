# coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from tests.__init__ import TestCase
import unittest
from template import Template


class SubmitTest(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.submit() %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input name="commit" type="submit" value="Save changes" data-disable-with="Save changes" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.submit("Edit this article") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input name="commit" type="submit" value="Edit this article" data-disable-with="Edit this article" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.submit( "Save edits", disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input name="commit" type="submit" value="Save edits" disabled="disabled" data-disable-with="Save edits" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.submit("Edit", _class="edit_button") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input name="commit" type="submit" value="Edit" class="edit_button" data-disable-with="Edit" />
</form>
""", t.render())

# @todo:
#         t = Template("""
# <%= using form(url="/user/new") do f %>
#     <%= f.submit( "Save", data: { confirm: "Are you sure?" }) %>
# <% end %>
# """)
#         self.assertEqual("""
# <form action="/user/new" method="GET" accept-charset="UTF8">
#     <input name='commit' type='submit' value='Save' data-disable-with="Save" data-confirm="Are you sure?" />
# </form>
# """, t.render())

# @todo:
#         t = Template("""
# <%= using form(url="/user/new") do f %>
#     <%= f.submit( "Complete sale", data: { disable_with: "Submitting..." }) %>
# <% end %>
# """)
#         self.assertEqual("""
# <form action="/user/new" method="GET" accept-charset="UTF8">
#     <input name="commit" data-disable-with="Submitting..." type="submit" value="Complete sale" />
# </form>
# """, t.render())



if __name__ == '__main__':
    unittest.main()
