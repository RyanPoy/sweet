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
from pyrails.activerecord import ActiveRecord, belongs_to, has_one, has_many, has_and_belongs_to_many
from pyrails.tests import create_table, drop_table


############ belongs_to ############
class Author(ActiveRecord): pass
class Post(ActiveRecord): belongs_to(Author)

drop_authors = lambda: drop_table('authors')
create_authors = lambda: create_table("""
create table if not exists authors (
    id int auto_increment,
    name varchar(32) not null,
    PRIMARY KEY (id)
);
""")

drop_posts = lambda: drop_table('posts')
create_posts = lambda: create_table("""
create table if not exists posts (
    id int auto_increment,
    title varchar(32) not null,
    author_id int,
    PRIMARY KEY (id)
);
""")


################# has_one ####################
class Beneficiary(ActiveRecord): pass
class Account(ActiveRecord): has_one(Beneficiary)

drop_accounts = lambda: drop_table('accounts')
create_accounts = lambda: create_table("""
create table if not exists accounts (
    id int auto_increment,
    name varchar(32) not null,
    PRIMARY KEY (id)
);
""")

drop_beneficiaries = lambda: drop_table('beneficiaries')
create_beneficiaries = lambda: create_table("""
create table if not exists beneficiaries (
    id int auto_increment,
    name varchar(32) not null,
    account_id int,
    PRIMARY KEY (id)
);
""")


################# has_many ####################
class Client(ActiveRecord): pass
class Firm(ActiveRecord): has_many(Client)

drop_firms = lambda: drop_table('firms')
create_firms = lambda: create_table("""
create table if not exists firms (
    id int auto_increment,
    name varchar(32) not null,
    PRIMARY KEY (id)
);
""")

drop_clients = lambda: drop_table('clients')
create_clients = lambda: create_table("""
create table if not exists clients (
    id int auto_increment,
    name varchar(32) not null,
    firm_id int,
    PRIMARY KEY (id)
);
""")


################# has_and_belongs_to_many ####################
class Project(ActiveRecord): has_and_belongs_to_many('pyrails.tests.test_assocation_has_and_belongs_to_many_helper.Developer')
class Developer(ActiveRecord): has_and_belongs_to_many(Project)

drop_projects = lambda: drop_table('projects')
create_projects = lambda: create_table("""
create table if not exists projects (
    id int auto_increment,
    name varchar(32) not null,
    PRIMARY KEY (id)
);
""")

drop_developers = lambda: drop_table('developers')
create_developers = lambda: create_table("""
create table if not exists developers (
    id int auto_increment,
    name varchar(32) not null,
    PRIMARY KEY (id)
);
""")

drop_developers_projects = lambda: drop_table('developers_projects')
create_developers_projects = lambda: create_table("""
create table if not exists developers_projects (
    id int auto_increment,
    developer_id int,
    project_id int,
    PRIMARY KEY (id)
);
""")
