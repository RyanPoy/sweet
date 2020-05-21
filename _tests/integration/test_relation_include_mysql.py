#coding: utf8
from sweet._tests import TestCase, User, Mobile, Car, Tag, Article, Course, Student, Score , \
                        StudentForHasOneThrough as Student2, CourseForHasOneThrough as Course2, ScoreForHasOneThrough as Score2, \
                        db_mgr
from sweet.orm import Model
from sweet.db import MySQL
from sweet.db.recordset import MySQLRecordset
from sweet.utils.collection import Collection
from contextlib import contextmanager


class FakeDB(MySQL):

    SQLS = []

    @classmethod
    def instance(cls):
        return cls(**db_mgr.get_config_with('default'))

    @classmethod
    def clear_sqls(cls):
        cls.SQLS = []

    def fetchall(self, sql, *params):
        relt = super().fetchall(sql, *params)
        # self.__class__.SQLS.append('%s | %s' % (sql, ','.join(map(str, params))))
        self.__class__.SQLS.append(sql)
        return relt


class TestRelationIncludeMysql(TestCase):
    
    def setUp(self):
        self.remove_record()

    def tearDown(self):
        self.remove_record()

    def remove_record(self):
        Mobile.delete_all()
        User.delete_all()
        Car.delete_all()
        Tag.delete_all()
        Article.delete_all()
        Student.delete_all()
        Score.delete_all()
        Course.delete_all()

    @contextmanager
    def mock_db(self):
        src_db = Model.db
        Model.db = FakeDB.instance()
        try:
            yield self
        except Exception:
            raise
        finally:
            Model.db = src_db
            FakeDB.clear_sqls()

    def test_include_belongs_to(self):
        user1 = User.create(name="Jon", age=31)
        user2 = User.create(name="Lily", age=20)
        Mobile.create(name="Nokia", user=user1)
        Mobile.create(name="IPhone", user=user1)
        Mobile.create(name="Vivo", user=user2)
        Mobile.create(name="Xiaomi", user=user2)

        with self.mock_db():
            # not use include
            ms = Mobile.all()
            for m in ms:
                m.user.name
            self.assertEqual(5, len(FakeDB.SQLS))
            self.assertEqual('SELECT * FROM `mobiles`', FakeDB.SQLS[0])
            self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s)', FakeDB.SQLS[1])
            self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s)', FakeDB.SQLS[2])
            self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s)', FakeDB.SQLS[3])
            self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s)', FakeDB.SQLS[4])

        with self.mock_db():
            # include
            ms = Mobile.include("user").all()
            for m in ms:
                m.user.name
            self.assertEqual(2, len(FakeDB.SQLS))
            self.assertEqual('SELECT * FROM `mobiles`', FakeDB.SQLS[0])
            self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s)', FakeDB.SQLS[1])

    def test_include_has_many(self):
        user1 = User.create(name="Jon", age=31)
        user2 = User.create(name="Lily", age=20)
        Mobile.create(name="Nokia", user=user1)
        Mobile.create(name="IPhone", user=user1)
        Mobile.create(name="Vivo", user=user2)
        Mobile.create(name="Xiaomi", user=user2)

        with self.mock_db():
            # not use include
            us = User.all()
            for u in us:
                ms = u.mobiles
                self.assertEqual(MySQLRecordset, type(ms))
                for m in ms.all():
                    m.name
            self.assertEqual(3, len(FakeDB.SQLS))
            self.assertEqual('SELECT * FROM `users`', FakeDB.SQLS[0])
            self.assertEqual('SELECT * FROM `mobiles` WHERE `user_id` = %s', FakeDB.SQLS[1])
            self.assertEqual('SELECT * FROM `mobiles` WHERE `user_id` = %s', FakeDB.SQLS[2])

        with self.mock_db():
            # include
            us = User.include("mobiles").all()
            for u in us:
                ms = u.mobiles
                self.assertEqual(Collection, type(ms))
                for m in ms.all():
                    m.name
            self.assertEqual(2, len(FakeDB.SQLS))
            self.assertEqual('SELECT * FROM `users`', FakeDB.SQLS[0])
            self.assertEqual('SELECT * FROM `mobiles` WHERE `user_id` IN (%s, %s)', FakeDB.SQLS[1])

    def test_include_has_one(self):
        user1 = User.create(name="Jon", age=31)
        user2 = User.create(name="Lily", age=20)

        Car.create(name="Benz", user=user1)
        Car.create(name="Mazda", user=user2)

        with self.mock_db():
            # not use include
            us = User.all()
            for u in us:
                u.car.name
            self.assertEqual(3, len(FakeDB.SQLS))
            self.assertEqual('SELECT * FROM `users`', FakeDB.SQLS[0])
            self.assertEqual('SELECT * FROM `cars` WHERE `user_id` = %s', FakeDB.SQLS[1])
            self.assertEqual('SELECT * FROM `cars` WHERE `user_id` = %s', FakeDB.SQLS[2])

        with self.mock_db():
            # not use include
            us = User.include('car').all()
            for u in us:
                u.car.name
            self.assertEqual(2, len(FakeDB.SQLS))
            self.assertEqual('SELECT * FROM `users`', FakeDB.SQLS[0])
            self.assertEqual('SELECT * FROM `cars` WHERE `user_id` IN (%s, %s)', FakeDB.SQLS[1])

    def test_include_has_many_through(self):
        s1 = Student.create(name='lily')
        s2 = Student.create(name='jon')
        s3 = Student.create(name='Lucy')

        c1 = Course.create(name='math')
        c2 = Course.create(name='sport')
        c3 = Course.create(name='swim')

        Score.create(student=s1, course=c1, value=100)
        Score.create(student=s1, course=c2, value=90)
        Score.create(student=s1, course=c3, value=93)

        Score.create(student=s2, course=c1, value=95)
        Score.create(student=s2, course=c2, value=97)
        Score.create(student=s2, course=c3, value=90)
        
        Score.create(student=s3, course=c1, value=98)
        Score.create(student=s3, course=c2, value=96)
        Score.create(student=s3, course=c3, value=94)

        with self.mock_db():
            # not use include
            for s in Student.all():
                cs = s.courses
                self.assertEqual(type(cs), MySQLRecordset)
                for c in cs.all():
                    c.name
            self.assertEqual(4, len(FakeDB.SQLS))
            self.assertEqual("SELECT * FROM `students`", FakeDB.SQLS[0])
            self.assertEqual("SELECT * FROM `courses` "
                             "INNER JOIN `scores` ON `courses`.`id` = `scores`.`course_id` "
                             "INNER JOIN `students` ON `students`.`id` = `scores`.`student_id` "
                             "WHERE `scores`.`student_id` = %s", FakeDB.SQLS[1])
            self.assertEqual("SELECT * FROM `courses` "
                             "INNER JOIN `scores` ON `courses`.`id` = `scores`.`course_id` "
                             "INNER JOIN `students` ON `students`.`id` = `scores`.`student_id` "
                             "WHERE `scores`.`student_id` = %s", FakeDB.SQLS[2])
            self.assertEqual("SELECT * FROM `courses` "
                             "INNER JOIN `scores` ON `courses`.`id` = `scores`.`course_id` "
                             "INNER JOIN `students` ON `students`.`id` = `scores`.`student_id` "
                             "WHERE `scores`.`student_id` = %s", FakeDB.SQLS[3])

        with self.mock_db():
            # not use include
            for s in Student.include("courses").all():
                cs = s.courses
                self.assertEqual(type(cs), Collection)
                for c in cs.all():
                    c.name
            self.assertEqual(3, len(FakeDB.SQLS))
            self.assertEqual('SELECT * FROM `students`', FakeDB.SQLS[0])
            self.assertEqual('SELECT * FROM `scores` WHERE `student_id` IN (%s, %s, %s)', FakeDB.SQLS[1])
            self.assertEqual('SELECT * FROM `courses` WHERE `id` IN (%s, %s, %s, %s, %s, %s, %s, %s, %s)', FakeDB.SQLS[2])
    
    def test_include_has_one_through(self):
        s1 = Student2.create(name='lily')
        s2 = Student2.create(name='jon')
        s3 = Student2.create(name='Lucy')
        s4 = Student2.create(name='Ryan')

        c1 = Course2.create(name='math')
        c2 = Course2.create(name='sport')
        c3 = Course2.create(name='swim')

        Score2.create(student=s1, course=c1, value=100)
        Score2.create(student=s1, course=c2, value=90)
        Score2.create(student=s1, course=c3, value=93)

        Score2.create(student=s2, course=c1, value=95)
        Score2.create(student=s2, course=c2, value=97)
        Score2.create(student=s2, course=c3, value=90)
        
        Score2.create(student=s3, course=c1, value=98)
        Score2.create(student=s3, course=c2, value=96)
        Score2.create(student=s3, course=c3, value=94)

        self.assertEqual(None, Student2.where(name='Ryan').first().course)
        self.assertEqual(None, Student2.include('course').where(name='Ryan').first().course)

        with self.mock_db():
            # not use include
            for s in Student2.where(name__not='Ryan').all():
                c = s.course
                self.assertEqual(type(c), Course2)
                c.name
            self.assertEqual(4, len(FakeDB.SQLS))
            self.assertEqual("SELECT * FROM `students` WHERE `name` <> %s", FakeDB.SQLS[0])
            self.assertEqual("SELECT * FROM `courses` "
                             "INNER JOIN `scores` ON `courses`.`id` = `scores`.`course_id` "
                             "INNER JOIN `students` ON `students`.`id` = `scores`.`student_id` "
                             "WHERE `scores`.`student_id` = %s", FakeDB.SQLS[1])
            self.assertEqual("SELECT * FROM `courses` "
                             "INNER JOIN `scores` ON `courses`.`id` = `scores`.`course_id` "
                             "INNER JOIN `students` ON `students`.`id` = `scores`.`student_id` "
                             "WHERE `scores`.`student_id` = %s", FakeDB.SQLS[2])
            self.assertEqual("SELECT * FROM `courses` "
                             "INNER JOIN `scores` ON `courses`.`id` = `scores`.`course_id` "
                             "INNER JOIN `students` ON `students`.`id` = `scores`.`student_id` "
                             "WHERE `scores`.`student_id` = %s", FakeDB.SQLS[3])

        with self.mock_db():
            # not use include
            for s in Student2.include("course").where(name__not='Ryan').all():
                c = s.course
                self.assertEqual(type(c), Course2)
                c.name
            self.assertEqual(3, len(FakeDB.SQLS))
            self.assertEqual('SELECT * FROM `students` WHERE `name` <> %s', FakeDB.SQLS[0])
            self.assertEqual('SELECT * FROM `scores` WHERE `student_id` IN (%s, %s, %s)', FakeDB.SQLS[1])
            self.assertEqual('SELECT * FROM `courses` WHERE `id` IN (%s, %s, %s, %s, %s, %s, %s, %s, %s)', FakeDB.SQLS[2])

    def test_include_has_and_belongs_to_many(self):
        t1 = Tag.create(name='cartoon')
        t2 = Tag.create(name='movie')
        t3 = Tag.create(name='money')

        a1 = Article.create(title='title-1', content='content—1')
        a2 = Article.create(title='title-2', content='content—2')
        a3 = Article.create(title='title-3', content='content—3')
        a4 = Article.create(title='title-4', content='content—4')

        t1.associate_with_articles(a1)
        t1.associate_with_articles(a2, a3, a4)
        t2.associate_with_articles(a1, a2)
        t3.associate_with_articles(a3, a4)

        with self.mock_db():
            # not use include
            for t in Tag.all():
                articles = t.articles
                self.assertEqual(type(articles), MySQLRecordset)
                for a in articles.all():
                    a.title
            self.assertEqual(4, len(FakeDB.SQLS))
            self.assertEqual("SELECT * FROM `tags`", FakeDB.SQLS[0])
            self.assertEqual("SELECT * FROM `articles` "
                             "INNER JOIN `articles_tags` ON `articles`.`id` = `articles_tags`.`article_id` "
                             "INNER JOIN `tags` ON `tags`.`id` = `articles_tags`.`tag_id` "
                             "WHERE `articles_tags`.`tag_id` = %s", FakeDB.SQLS[1])
            self.assertEqual("SELECT * FROM `articles` "
                             "INNER JOIN `articles_tags` ON `articles`.`id` = `articles_tags`.`article_id` "
                             "INNER JOIN `tags` ON `tags`.`id` = `articles_tags`.`tag_id` "
                             "WHERE `articles_tags`.`tag_id` = %s", FakeDB.SQLS[2])
            self.assertEqual("SELECT * FROM `articles` "
                             "INNER JOIN `articles_tags` ON `articles`.`id` = `articles_tags`.`article_id` "
                             "INNER JOIN `tags` ON `tags`.`id` = `articles_tags`.`tag_id` "
                             "WHERE `articles_tags`.`tag_id` = %s", FakeDB.SQLS[3])
            

        with self.mock_db():
            # not use include
            for t in Tag.include("articles").all():
                articles = t.articles
                self.assertEqual(type(articles), Collection)
                for a in articles.all():
                    a.title
            self.assertEqual(3, len(FakeDB.SQLS))
            self.assertEqual("SELECT * FROM `tags`", FakeDB.SQLS[0])
            self.assertEqual('SELECT * FROM `articles_tags` WHERE `tag_id` IN (%s, %s, %s)', FakeDB.SQLS[1])
            self.assertEqual('SELECT * FROM `articles` WHERE `id` IN (%s, %s, %s, %s, %s, %s, %s, %s)', FakeDB.SQLS[2])

    def test_include_has_one_and_has_many(self):
        user1 = User.create(name="Jon", age=31)
        user2 = User.create(name="Lily", age=20)
        Mobile.create(name="Nokia", user=user1)
        Mobile.create(name="IPhone", user=user1)
        Mobile.create(name="Vivo", user=user2)
        Mobile.create(name="Xiaomi", user=user2)
        Car.create(name="Benz", user=user1)
        Car.create(name="Mazda", user=user2)

        with self.mock_db():
            for u in User.include("mobiles", "car").all():
                u.car.name
                for m in u.mobiles.all():
                    m.name
            self.assertEqual(3, len(FakeDB.SQLS))
            self.assertEqual('SELECT * FROM `users`', FakeDB.SQLS[0])
            self.assertEqual('SELECT * FROM `mobiles` WHERE `user_id` IN (%s, %s)', FakeDB.SQLS[1])
            self.assertEqual('SELECT * FROM `cars` WHERE `user_id` IN (%s, %s)', FakeDB.SQLS[2])

if __name__ == '__main__':
    import unittest
    unittest.main()
