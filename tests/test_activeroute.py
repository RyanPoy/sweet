#coding: utf8
from pyrails.activeroute import ActiveRoute
import unittest


class ActionRouteTest(unittest.TestCase):

    def test_purify_rule1(self):
        self.assertEqual('^/$', ActiveRoute('/', None, None)._purify_rule())
        self.assertEqual('^/abc[/]?$', ActiveRoute('/abc', None, None)._purify_rule())
        self.assertEqual('^/abc[/]?$', ActiveRoute('/abc/', None, None)._purify_rule())
        self.assertEqual('^/abc[/]?$', ActiveRoute('/abc[/]', None, None)._purify_rule())
        self.assertEqual('^/abc[/]?$', ActiveRoute('/abc[/]?', None, None)._purify_rule())

    def test_root_path_rule(self):
        route = ActiveRoute('/', None, None)
        self.assertEqual(r'^/$', route.rule)

    def test_match_root_path(self):
        route = ActiveRoute('/', None, None)
        self.assertTrue(route.match('/') is True)

    def test_int_rule(self):
        route = ActiveRoute('/<id:int>', None, None)
        self.assertEqual(r'^/(?P<id>\d+)[/]?$', route.rule)

    def test_match_int(self):
        route = ActiveRoute('/<id:int>', None, None)
        self.assertTrue(route.match('/123') is True)

    def test_float_rule(self):
        route = ActiveRoute('/<id:float>', None, None)
        self.assertEqual(r'^/(?P<id>\d+(\.\d+)?)[/]?$', route.rule)

    def test_match_float1(self):
        route = ActiveRoute('/<id:float>', None, None)
        self.assertTrue(route.match('/123') is True)

    def test_match_float2(self):
        route = ActiveRoute('/<id:float>', None, None)
        self.assertTrue(route.match('/123.123') is True)

    def test_str_rule(self):
        route = ActiveRoute('/<name:str>', None, None)
        self.assertEqual(r'^/(?P<name>\w+)[/]?$', route.rule)

    def test_str_rule_without_content(self):
        route = ActiveRoute('/<name>', None, None)
        self.assertEqual(r'^/(?P<name>\w+)[/]?$', route.rule)

    def test_match_str(self):
        route = ActiveRoute('/<name:str>', None, None)
        self.assertTrue(route.match('/pengyi') is True)

    def test_list_rule(self):
        route = ActiveRoute('/<ids:list>', None, None)
        self.assertEqual(r'^/(?P<ids>\w+(,\w+)*?)[/]?$', route.rule)

    def test_match_list1(self):
        route = ActiveRoute('/<ids:list>', None, None)
        self.assertTrue(route.match('/1') is True)

    def test_match_list2(self):
        route = ActiveRoute('/<ids:list>', None, None)
        self.assertTrue(route.match('/1,2,3') is True)

    def test_regex_rule(self):
        route = ActiveRoute('/<year:\d{1,4}>', None, None)
        self.assertEqual(r'^/(?P<year>\d{1,4})[/]?$', route.rule)

    def test_match_regex(self):
        route = ActiveRoute('/<year:\d{1,4}>', None, None)
        self.assertTrue(route.match('/2012') is True)

    def test_complex_rule(self):
        route = ActiveRoute('/post/<id>/datetime/<year:\d{1,4}>/<month:int>', None, None)
        self.assertEqual(r'^/post/(?P<id>\w+)/datetime/(?P<year>\d{1,4})/(?P<month>\d+)[/]?$', route.rule)

    def test_match_complex_rule(self):
        route = ActiveRoute('/post/<id>/datetime/<year:\d{1,4}>/<month:int>', None, None)
        self.assertTrue(route.match('/post/123/datetime/2014/10') is True)


if __name__ == '__main__':
    unittest.main()
