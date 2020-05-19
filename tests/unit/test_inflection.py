#coding: utf8
from sweet.tests import TestCase
from sweet.utils import *


class TestInflection(TestCase):

    def setUp(self):
        self.singular_and_plural = [
            ("search"      , "searches"),
            ("switch"      , "switches"),
            ("fix"         , "fixes"),
            ("box"         , "boxes"),
            ("process"     , "processes"),
            ("address"     , "addresses"),
            ("case"        , "cases"),
            ("stack"       , "stacks"),
            ("wish"        , "wishes"),
            ("fish"        , "fish"),

            ("category"    , "categories"),
            ("query"       , "queries"),
            ("ability"     , "abilities"),
            ("agency"      , "agencies"),
            ("movie"       , "movies"),

            ("archive"     , "archives"),

            ("index"       , "indices"),

            ("wife"        , "wives"),
            ("safe"        , "saves"),
            ("half"        , "halves"),

            ("move"        , "moves"),

            ("salesperson" , "salespeople"),
            ("person"      , "people"),

            ("spokesman"   , "spokesmen"),
            ("man"         , "men"),
            ("woman"       , "women"),

            ("basis"       , "bases"),
            ("diagnosis"   , "diagnoses"),

            ("datum"       , "data"),
            ("medium"      , "media"),
            ("analysis"    , "analyses"),

            ("node_child"  , "node_children"),
            ("child"       , "children"),

            ("experience"  , "experiences"),
            ("day"         , "days"),

            ("comment"     , "comments"),
            ("foobar"      , "foobars"),
            ("newsletter"  , "newsletters"),

            ("old_news"    , "old_news"),
            ("news"        , "news"),

            ("series"      , "series"),
            ("species"     , "species"),

            ("quiz"        , "quizzes"),

            ("perspective" , "perspectives"),

            ("ox" , "oxen"),
            ("photo" , "photos"),
            ("buffalo" , "buffaloes"),
            ("tomato" , "tomatoes"),
            ("dwarf" , "dwarves"),
            ("elf" , "elves"),
            ("information" , "information"),
            ("equipment" , "equipment"),
            ("bus" , "buses"),
            ("status" , "statuses"),
            ("mouse" , "mice"),

            ("louse" , "lice"),
            ("house" , "houses"),
            ("octopus" , "octopi"),
            ("virus" , "viri"),
            ("alias" , "aliases"),
            ("portfolio" , "portfolios"),

            ("vertex" , "vertices"),
            ("matrix" , "matrices"),

            ("axis" , "axes"),
            ("testis" , "testes"),
            ("crisis" , "crises"),

            ("rice" , "rice"),
            ("shoe" , "shoes"),

            ("horse" , "horses"),
            ("prize" , "prizes"),
            ("edge" , "edges"),
            ("person", "people"),
            ("student_and_teacher", "student_and_teachers"),
            ("money", "money"),
            ("pretty_fish", "pretty_fish")
        ]

    def test_pluralize(self):
        for singular, plural in self.singular_and_plural:
            self.assertEqual(pluralize(singular), plural)

    def test_singularize(self) :
        for singular, plural in self.singular_and_plural:
            self.assertEqual(singularize(plural), singular)


if __name__ == '__main__':
    import unittest
    unittest.main()
