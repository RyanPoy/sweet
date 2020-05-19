# coding: utf8
from sweet._tests import TestCase
import os
from sweet.template import Template, FormatError


class TestTemplateBasic(TestCase):

    def setUp(self):
        self.dirname = os.path.dirname(os.path.abspath(__file__))
        
    def test_text(self):
        content = """
this is a string 1
this is a string 2
this is a string 3
"""
        self.assertEqual(content, Template(content).render())
    
    def test_variable(self):
        t = Template("""她叫<%= name %>, and <%= age %> years old """)
        self.assertEqual("""她叫露西, and 10 years old """,
                         t.render(name="露西", age=10))
                        
    def test_expressions(self):
        t = Template("<%= 1 + 2 %>")
        self.assertEqual("3", t.render())
                         
        t = Template("1 + 2 = <%= 1 + 2 %>")
        self.assertEqual('1 + 2 = 3', t.render())
                       
        t = Template("<%= sum([ x for x in range(10) ]) %>")
        self.assertEqual('45', t.render())
                  
        t = Template("<%= 1 / 2 %>")
        self.assertEqual('0.5', t.render())

    def test_comment(self):
        t = Template("Hello<%# TODO i18n %> <%= name %>!")
        self.assertEqual("Hello 中国!", t.render(name="中国"))

    def test_unsupport_error(self):
        with self.assertRaises(FormatError) as err:
            t = Template("""<% fuck %>""").render()
        self.assertEqual("Unsupport '<% fuck %>' on <string> at line 1", str(err.exception))


if __name__ == '__main__':
    import unittest
    unittest.main()
