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
from pyrails.active_record import ActiveRecord, belongs_to
from pyrails.tests import create_table, drop_table


class Author(ActiveRecord):
    pass


class Post(ActiveRecord):
    belongs_to(Author)


def create_authors():
    create_table("""
create table if not exists authors (
    id int auto_increment,
    name varchar(32) not null,
    PRIMARY KEY (id)
);
""")


def create_posts():
    create_table("""
create table if not exists posts (
    id int auto_increment,
    title varchar(32) not null,
    author_id int,
    PRIMARY KEY (id)
);
""")


def drop_posts():
    drop_table('posts')

    
def drop_authors():
    drop_table('authors')
