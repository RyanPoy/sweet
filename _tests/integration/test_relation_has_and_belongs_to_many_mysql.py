#coding: utf8
from sweet._tests import TestCase, Article, Tag


class TestRelationHasAndBelongsToManyMysql(TestCase):
    
    def setUp(self):
        self.remove_record()

    def tearDown(self):
        self.remove_record()

    def remove_record(self):
        Article.delete_all()
        Tag.delete_all()

    def test_associate_and_query(self):
        t1 = Tag.create(name='cartoon')
        t2 = Tag.create(name='movie')

        a1 = Article.create(title='title-1', content='content—1')
        a2 = Article.create(title='title-2', content='content—2')
        a3 = Article.create(title='title-3', content='content—3')
        a4 = Article.create(title='title-4', content='content—4')

        t1.associate_with_articles(a1)
        t1.associate_with_articles(a2, a3, a4)

        articles = t1.articles.all()
        self.assertEqual(4, len(articles))
        self.assertEqual('title-1', articles[0].title)
        self.assertEqual('title-2', articles[1].title)
        self.assertEqual('title-3', articles[2].title)
        self.assertEqual('title-4', articles[3].title)

        a1.associate_with_tags(t2)
        a2.associate_with_tags(t2)
        a3.associate_with_tags(t2)
        a4.associate_with_tags(t2)

        tags = a1.tags.all()
        self.assertEqual(2, len(tags))
        self.assertEqual('cartoon', tags[0].name)
        self.assertEqual('movie', tags[1].name)
        self.assertEqual(2, len(a2.tags.all()))
        self.assertEqual(2, len(a3.tags.all()))
        self.assertEqual(2, len(a4.tags.all()))

    def test_associate_and_query_with_include(self):
        t1 = Tag.create(name='cartoon')
        t2 = Tag.create(name='movie')

        a1 = Article.create(title='title-1', content='content—1')
        a2 = Article.create(title='title-2', content='content—2')
        a3 = Article.create(title='title-3', content='content—3')
        a4 = Article.create(title='title-4', content='content—4')

        t1.associate_with_articles(a1)
        t1.associate_with_articles(a2, a3, a4)

        tags = Tag.include('articles').all()
        t1, t2 = tags[0], tags[1]

        articles = t1.articles.all()
        self.assertEqual(4, len(articles))
        self.assertEqual('title-1', articles[0].title)
        self.assertEqual('title-2', articles[1].title)
        self.assertEqual('title-3', articles[2].title)
        self.assertEqual('title-4', articles[3].title)

        a1.associate_with_tags(t2)
        a2.associate_with_tags(t2)
        a3.associate_with_tags(t2)
        a4.associate_with_tags(t2)

        tags = Article.include("tags").first().tags.all()
        self.assertEqual(2, len(tags))
        self.assertEqual('cartoon', tags[0].name)
        self.assertEqual('movie', tags[1].name)
        self.assertEqual(2, len(a2.tags.all()))
        self.assertEqual(2, len(a3.tags.all()))
        self.assertEqual(2, len(a4.tags.all()))

    def test_should_associate_one_time_when_same_association_create_more_times(self):
        t1 = Tag.create(name='cartoon')

        a1 = Article.create(title='title-1', content='content—1')
        a2 = Article.create(title='title-2', content='content—2')
        a3 = Article.create(title='title-3', content='content—3')
        a4 = Article.create(title='title-4', content='content—4')

        for x in range(10):
            t1.associate_with_articles(a1, a2, a3, a4)

        articles = t1.articles.all()
        self.assertEqual(4, len(articles))
        self.assertEqual('title-1', articles[0].title)
        self.assertEqual('title-2', articles[1].title)
        self.assertEqual('title-3', articles[2].title)
        self.assertEqual('title-4', articles[3].title)

    def test_dissociate(self):
        t1 = Tag.create(name='cartoon')

        a1 = Article.create(title='title-1', content='content—1')
        a2 = Article.create(title='title-2', content='content—2')
        a3 = Article.create(title='title-3', content='content—3')
        a4 = Article.create(title='title-4', content='content—4')

        t1.associate_with_articles(a1, a2, a3, a4)
        self.assertEqual(4, len(t1.articles.all()))
        
        t1.dissociate_with_articles(a1)
        self.assertEqual(3, len(t1.articles.all()))

        t1.dissociate_with_articles(a2, a3, a4)
        self.assertEqual(0, len(t1.articles.all()))


if __name__ == '__main__':
    import unittest
    unittest.main()
