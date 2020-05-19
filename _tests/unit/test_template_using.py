# coding: utf8
from sweet._tests import TestCase
from sweet.template import Template, FormatError
import os


class FakeTag(object):

    def begin_render(self):
        return "<fake-tag>"

    def end_render(self):
        return "</fake-tag>"


class TestTemplateUsing(TestCase):

    def setUp(self):
        self.dirname = os.path.dirname(os.path.abspath(__file__))

    def test_using(self):
        t = Template("""<%= using FakeTag() do f %><% end %>""")
        self.assertEqual("""<fake-tag></fake-tag>""", t.render(FakeTag=FakeTag))

    def test_parse_error_when_has_if_but_not_has_end(self):
        with self.assertRaises(FormatError) as err:
            Template("""<%= using FakeTag() do f %>""").render(FakeTag=FakeTag)
        self.assertEqual("Missing '<% end %>' for '<% = using FakeTag() do f %>' on <string> at line 1", str(err.exception))

    def test_format_error(self):
        with self.assertRaises(FormatError) as err:
            Template("""<%= using FakeTag() %><% end %>""").render(FakeTag=FakeTag)
        self.assertEqual("'<%= using FakeTag() %>' format error on <string> at line 1", str(err.exception))

        with self.assertRaises(FormatError) as err:
            Template("""<%= using FakeTag() do %><% end %>""").render(FakeTag=FakeTag)
        self.assertEqual("'<%= using FakeTag() do %>' format error on <string> at line 1", str(err.exception))


if __name__ == '__main__':
    import unittest
    unittest.main()
