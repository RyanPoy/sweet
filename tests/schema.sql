create database sweet_test default charset utf8;

use sweet_test;

create table users (
    id int auto_increment primary key ,
    name varchar(32) not null default '',
    age int not null default 20
);
