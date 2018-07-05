# -*- coding:utf-8 -*-

database = 'sweet_blog'

drop_db_sql = """ DROP DATABASE IF EXISTS %s; """ % database

create_db_sql = """ CREATE DATABASE %s DEFAULT CHARSET UTF8; """ % database 

create_table_sql = """
CREATE TABLE users (
    id      INTEGER         NOT NULL AUTO_INCREMENT,
    name    VARCHAR(200)    NOT NULL,
    password VARCHAR(200)   NOT NULL,
    created_at   DATETIME   NOT NULL,
    PRIMARY KEY (id),
    INDEX idx_createa_at (created_at)
);

CREATE TABLE categories (
    id      INTEGER         NOT NULL AUTO_INCREMENT,
    name    VARCHAR(200)    NOT NULL,
    created_at   DATETIME   NOT NULL,
    updated_at   DATETIME   NOT NULL,
    PRIMARY KEY (id),
    INDEX idx_createa_at (created_at),
    INDEX idx_updated_at (updated_at)
);

CREATE TABLE articles (
    id           INTEGER      NOT NULL AUTO_INCREMENT,
    title        VARCHAR(200) NOT NULL,
    content      TEXT         NOT NULL,
    created_at   DATETIME     NOT NULL,
    updated_at   DATETIME     NOT NULL,
    user_id      INTEGER      NOT NULL,
    category_id INTEGER     NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (user_id) REFERENCES users (id),
    FOREIGN KEY (user_id) REFERENCES users (id),
    FOREIGN KEY (category_id) REFERENCES categories (id),
    INDEX idx_createa_at (created_at),
    INDEX idx_updated_at (updated_at)
);

CREATE TABLE tags (
    id          INTEGER         NOT NULL AUTO_INCREMENT,
    name        VARCHAR(200)    NOT NULL,
    PRIMARY KEY (id),
    UNIQUE      idx_unique_name (name)
);

CREATE TABLE articles_tags (
    id          INTEGER     NOT NULL AUTO_INCREMENT,
    article_id  INTEGER     NOT NULL,
    tag_id      INTEGER     NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (article_id) REFERENCES articles (id),
    FOREIGN KEY (tag_id) REFERENCES tags (id),
    UNIQUE      idx_unique_articleid_tagid (article_id, tag_id)
);

CREATE TABLE comments (
    id          INTEGER     NOT NULL AUTO_INCREMENT,
    article_id  INTEGER     NOT NULL,
    user_id     INTEGER     NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (article_id) REFERENCES articles (id),
    FOREIGN KEY (user_id) REFERENCES users (id)
);
"""


from sweet.record import ActiveRecord
from sweet.relation import has_one, has_many, belongs_to, has_and_belongs_to_many


class User(ActiveRecord):
    __columns__ = [ 'id', 'name', 'password', 'created_at']
    __updated_at__ = None
    has_many("sweet.tests.integrations.blog.Article")
    has_many("sweet.tests.integrations.blog.Comment")


class Category(ActiveRecord):
    __columns__ = [ 'id', 'name', 'created_at', 'updated_at' ]
    has_many("sweet.tests.integrations.blog.Article")


class Article(ActiveRecord):
    __columns__ = [ 'id', 'title', 'content', 'created_at', 'updated_at', 'user_id', 'category_id']
    belongs_to(User)
    belongs_to(Category)
    has_and_belongs_to_many("sweet.tests.integrations.blog.Tag")
    has_many("sweet.tests.integrations.blog.Comment")


class Tag(ActiveRecord):
    __columns__ = [ 'id', 'name']
    __created_at__, __updated_at__ = None, None
    has_and_belongs_to_many(Article)
    belongs_to(Category)


class Comment(ActiveRecord):
    __columns__ = [ 'id', 'article_id', 'user_id']
    __created_at__, __updated_at__ = None, None
    belongs_to(Article)
    belongs_to(User)

