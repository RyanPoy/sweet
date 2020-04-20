# coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

from tests.__init__ import TestCase
import unittest
import os
from template import Template
from datetime import date, datetime


class DatetimeTest(TestCase):

    def test_form_with_url_and_datetime_field(self):
        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.datetime_field("user_born_on") %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="datetime-local" />
</form>
""", t.render())

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.datetime_field("user_born_on", date(year=2020, month=1, day=1)) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="datetime-local" value="2020-01-01T00:00:00" />
</form>
""", t.render(date=date))

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.datetime_field("user_born_on", datetime(year=2020, month=1, day=2, hour=10, minute=20, second=30)) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="datetime-local" value="2020-01-02T10:20:30" />
</form>
""", t.render(datetime=datetime))

        t = Template("""
<%= using form(url="/user/new") do f %>
    <%= f.datetime_field("user_born_on", _min=date(year=2020, month=1, day=2)) %>
<% end %>
""")
        self.assertEqual("""
<form action="/user/new" method="GET" accept-charset="UTF8">
    <input id="user_born_on" name="user_born_on" type="datetime-local" min="2020-01-02T00:00:00" />
</form>
""", t.render(date=date))


if __name__ == '__main__':
    unittest.main()
