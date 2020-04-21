# coding: utf8
from sweet.tests.unit import TestCase
import unittest
from template import Template


class RadioTest(TestCase):

    def test_for_tag(self):
        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.radio('favorite_color', 'maroon') %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="favorite_color_maroon" name="favorite_color" type="radio" value="maroon" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.radio('receive_updates', 'no', checked=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="receive_updates_no" name="receive_updates" type="radio" value="no" checked="checked" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.radio('time_slot', "3:00 p.m.", checked=False, disabled=True) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="time_slot_3:00_p.m." name="time_slot" type="radio" value="3:00 p.m." disabled="disabled" />
</form>
""", t.render())

        t = Template("""
<%= using form(action="/user/new") do f %>
    <%= f.radio('color', "green", checked=True, _class="color_input") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="color_green" name="color" type="radio" value="green" class="color_input" checked="checked" />
</form>
""", t.render())


if __name__ == '__main__':
    unittest.main()
