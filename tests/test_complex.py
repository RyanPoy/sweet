# coding: utf8
from __init__ import TestCase, UserForTest
from template import MemLoader
import unittest


class ComplexTest(TestCase):

    def setUp(self):
        self.users = [
            UserForTest("露西", 20),
            UserForTest("Lily", 10)
        ]
 
    def test_complex(self):
        loader = MemLoader({
            "base.html": 
"""
<html>
    <header>
        <title><%block title%><% end %></title>
        <body><%block body%><% end %></body>
    </header>
</html>
""",
            "index.html": 
"""
<% extends base.html %>
<% block title %>首页<% end %>
<% block body %><% include _members.html members=users %><% end %>
""",
            "_members.html": 
"""
<ul>
  <% for m in members %>
    <% include _member.html %>  
  <% end %>
</ul>
""",
            "_member.html": """<li><%= m.name %>|<%= m.age %></li>"""
        })
        r = loader.load('index.html').render(users=self.users)
        self.assertEqual(''.join("""
<html>
  <header>
    <title>首页</title>
    <body>
      <ul>  
        <li>露西|20</li>
        <li>Lily|10</li>  
      </ul>
    </body>
  </header>
</html>
""".split()), ''.join(r.split()))
    

if __name__ == '__main__':
    unittest.main()
