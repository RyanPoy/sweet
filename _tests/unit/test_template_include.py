# coding: utf8
from sweet._tests import TestCase, UserForTemplateTest
from template import MemLoader, FormatError


class TestTemplateInclude(TestCase):

    def setUp(self):
        self.users = [
            UserForTemplateTest("露西", 20),
            UserForTemplateTest("Lily", 10)
        ]
 
    def test_include(self):
        loader = MemLoader({
            "index.html": "<ul><% include _partial.html %></ul>",
            "_partial.html": """<% for u in users %><li><%= u.name %>|<%= u.age %></li><% end %>""",
        })
        t = loader.load('index.html')
        r = t.render(users=self.users)
        self.assertEqual("<ul><li>露西|20</li><li>Lily|10</li></ul>", r)
    
    def test_include_with_customer_variable(self):
        loader = MemLoader({
            "index.html": "<ul><% include _partial.html members=users %></ul>",
            "_partial.html": """<% for m in members %><li><%= m.name %>|<%= m.age %></li><% end %>""",
        })
        t = loader.load('index.html')
        r = t.render(users=self.users)
        self.assertEqual("<ul><li>露西|20</li><li>Lily|10</li></ul>", r)
        
    def test_parse_include_error_if_not_has_fname(self):
        loader = MemLoader({
            "index.html": "<ul><% include %></ul>",
            "_partial.html": """<% for m in members %><li><%= m.name %>|<%= m.age %></li><% end %>""",
        })
        with self.assertRaises(FormatError) as err:
            loader.load('index.html').render(users=self.users)
        self.assertEqual("Missing template file path for '<% include %>' on index.html at line 1", str(err.exception))
        
    def test_parse_include_error_if_has_a_not_exist_fname(self):
        loader = MemLoader({
            "index.html": "<ul><% include not_exist.html %></ul>",
            "_partial.html": """<% for m in members %><li><%= m.name %>|<%= m.age %></li><% end %>""",
        })
        with self.assertRaises(FormatError) as err:
            loader.load('index.html').render(users=self.users)
        self.assertEqual("not_exist.html is not exist on index.html at line 1", str(err.exception))

if __name__ == '__main__':
    import unittest
    unittest.main()
