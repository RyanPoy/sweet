# coding: utf8
from __init__ import TestCase, UserForTest
from template import MemLoader
import unittest
import os


class TemplateTest(TestCase):

    def setUp(self):
        self.dirname = os.path.dirname(os.path.abspath(__file__))
        self.users = [
            UserForTest("露西", 20),
            UserForTest("Lily", 10)
        ]

    def test_include(self):
        loader = MemLoader({
            "index.html": "<ul><% include _partial.html %></ul>",
            "_partial.html": """<% for u in users %><li><%= u.name %>|<%= u.age %></li><% end %>""",
        })
        t = loader.load('index.html')
        r = t.render(users = self.users)
        self.assertEqual("<ul><li>露西|20</li><li>Lily|10</li></ul>", r)


if __name__ == '__main__':
    unittest.main()
