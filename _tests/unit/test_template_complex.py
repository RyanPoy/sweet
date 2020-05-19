# coding: utf8
from sweet._tests import TestCase, UserForTemplateTest
from template import MemLoader, FileLoader
import os


class TestTemplateComplex(TestCase):

    def setUp(self):
        self.users = [
            UserForTemplateTest("露西", 20),
            UserForTemplateTest("Lily", 10)
        ]
 
    def test_complex1(self):
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
    
    def test_complex2(self):
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
            "minibase.html": 
"""
<% extends base.html %>
<% block title %>首页<% end %>
""",
            "index.html": 
"""
<% extends minibase.html %>
<% block body %>主体<% end %>
""",
        })
        r = loader.load('index.html').render(users=self.users)
        self.assertEqual(''.join("""
<html>
  <header>
    <title>首页</title>
    <body>主体</body>
  </header>
</html>
""".split()), ''.join(r.split()))
        
    def test_complex3(self):
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
            "minibase.html": 
"""
<% extends base.html %>
<% block title %>首页<% end %>
""",
            "index.html": 
"""
<% extends minibase.html %>
<% block title %>首页<% end %>
<% block body %><% include user_body.html %><% end %>
""",
            "user_body_base.html": """
<ul>
    <% block users %> <% end %>
</ul>
""",
            "user_body.html": """
<% extends user_body_base.html %>
<% block users %>
  <% for u in users%>
      <% include user.html %>
  <% end %>
<% end %>
""",
            "user.html": """<li><%= u.name %>|<%= u.age %></li>"""
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
        
    def test_complex3_from_html_files(self):
        html_dir = os.path.join(os.path.dirname(__file__), 'htmls')
        loader = FileLoader(html_dir)
        r = loader.load('users/index.html').render(users=self.users)
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
    import unittest
    unittest.main()
