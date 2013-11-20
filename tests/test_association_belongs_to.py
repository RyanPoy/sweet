#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
from pyrails.tests.test_association_belongs_to_helper import *
import unittest


class BelongsToTest(unittest.TestCase):
    
    def setUp(self):
        drop_posts()
        drop_authors()
        create_authors()
        create_posts()
        
    def tearDown(self):
        drop_posts()
        drop_authors()
        
    def test_belongs_to(self):
        self.assertEqual(1, len(Post.association_dict))
        association = Post.association_of('author')
        self.assertTrue(association.is_belongs_to())
        self.assertEqual(Author, association.target)
        self.assertEqual('author', association.attr_name)
        self.assertEqual('author_id', association.foreign_key)
        self.assertFalse(association.dependent)

    def test_belongs_to_with_customer_some_init_args(self):
        class Post(ActiveRecord): belongs_to(Author, attr_name="user")

        association = Post.association_of('user')
        self.assertTrue(association.is_belongs_to())
        self.assertEqual(Author, association.target)
        self.assertEqual('user', association.attr_name)
        self.assertEqual('author_id', association.foreign_key)
        self.assertFalse(association.dependent)
    
    def test_belongs_to_with_customer_more_init_args(self):
        class Post(ActiveRecord): belongs_to(Author, attr_name="user", foreign_key="user_id", dependent=True)

        association = Post.association_of('user')
        self.assertTrue(association.is_belongs_to())
        self.assertEqual(Author, association.target)
        self.assertEqual('user', association.attr_name)
        self.assertEqual('user_id', association.foreign_key)
        self.assertTrue(association.dependent)
    
    def test_belongs_to_find(self):
        """ Post#author (similar to Author.find(author_id))
        """
        author = Author(name="author1").save()
        Post(title="post1", author=author).save()
        author = Post.find(1).author
        self.assertEqual(1, author.id)
        self.assertEqual('author1', author.name)

    def test_belongs_to_set_attrbuite(self):
        """ Post#author=(author) (similar to post.author_id = author.id)
        """
        author1 = Author(name="author1").save()
        author2 = Author(name="author2").save()
        Post(title="post1", author=author1).save()
        post = Post.find(1)
        post.author = author2

        self.assertEqual(2, post.author_id)
        self.assertEqual('author2', post.author.name)

    def test_belongs_to_build(self):
        """ Post#build_author (similar to post.author = Author())
        """
        post = Post(title="post1")
        author = post.build_author(name='author1')
        self.assertEqual('author1', post.author.name)
        self.assertEqual('author1', author.name)
        self.assertIsNone(author.id)
        self.assertIsNone(post.author.id)

    def test_belongs_to_create(self):
        """ Post#create_author (similar to post.author = Author(); post.author.save(); return post.author)
        """
        post = Post(title="post1")
        post.create_author(name="author1")
        author = Author.find(1)
        self.assertEqual(1, author.id)
        self.assertEqual('author1', author.name)
        self.assertEqual(1, post.author.id)


if __name__ == '__main__':
    unittest.main()
